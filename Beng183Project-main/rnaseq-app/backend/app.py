import os, json, time, threading, uuid, subprocess, shutil, glob
from pathlib import Path
from datetime import datetime
from flask import Flask, request, jsonify, send_file
from flask_cors import CORS
from dotenv import load_dotenv

from sqlalchemy import create_engine, String, Integer, Float, Text, ForeignKey
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship, Session

# ---------------------------------
# Load configuration
# ---------------------------------
load_dotenv()
API_PORT = int(os.getenv("API_PORT", "5001"))

# Use /data/qc for browser-served artifacts so relative assets work in iframe
QC_ROOT = Path(os.getenv("QC_ROOT", "/data/qc"))

STORAGE_ROOT = Path(os.getenv("STORAGE_ROOT", "storage"))
UPLOAD_DIR = STORAGE_ROOT / "uploads"
ARTIFACTS_DIR = STORAGE_ROOT / "artifacts"  # keep if you need it for other stuff
DB_DIR = Path("db")
DB_DIR.mkdir(parents=True, exist_ok=True)
DB_PATH = DB_DIR / "rnaseq.sqlite"

for d in (UPLOAD_DIR, ARTIFACTS_DIR, QC_ROOT):
    d.mkdir(parents=True, exist_ok=True)

# ---------------------------------
# Database models
# ---------------------------------
class Base(DeclarativeBase): pass

class Run(Base):
    __tablename__ = "runs"
    id: Mapped[str] = mapped_column(String(32), primary_key=True)
    created_at: Mapped[str] = mapped_column(String(40))
    status: Mapped[str] = mapped_column(String(32), default="queued")
    progress: Mapped[float] = mapped_column(Float, default=0.0)
    sample_name: Mapped[str] = mapped_column(String(255))
    sample_files_json: Mapped[str] = mapped_column(Text)
    params_json: Mapped[str] = mapped_column(Text, default='{}')
    stages: Mapped[list["Stage"]] = relationship(back_populates="run", cascade="all, delete-orphan")
    artifacts: Mapped[list["Artifact"]] = relationship(back_populates="run", cascade="all, delete-orphan")

class Stage(Base):
    __tablename__ = "stages"
    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    run_id: Mapped[str] = mapped_column(ForeignKey("runs.id", ondelete="CASCADE"))
    name: Mapped[str] = mapped_column(String(64))
    status: Mapped[str] = mapped_column(String(32), default="running")
    progress: Mapped[float] = mapped_column(Float, default=0.0)
    time_iso: Mapped[str] = mapped_column(String(40))
    metrics_json: Mapped[str] = mapped_column(Text, default='{}')
    artifact_path: Mapped[str] = mapped_column(Text, default='')
    run: Mapped[Run] = relationship(back_populates="stages")

class Artifact(Base):
    __tablename__ = "artifacts"
    id: Mapped[int] = mapped_column(Integer, primary_key=True, autoincrement=True)
    run_id: Mapped[str] = mapped_column(ForeignKey("runs.id", ondelete="CASCADE"))
    kind: Mapped[str] = mapped_column(String(64))
    path: Mapped[str] = mapped_column(Text)
    run: Mapped[Run] = relationship(back_populates="artifacts")

# New: reference genomes table (FASTA + GTF + STAR index)
class Reference(Base):
    __tablename__ = "references"
    id: Mapped[str] = mapped_column(String(32), primary_key=True)
    name: Mapped[str] = mapped_column(String(255))
    created_at: Mapped[str] = mapped_column(String(40))
    status: Mapped[str] = mapped_column(String(32), default="indexing")
    fasta_path: Mapped[str] = mapped_column(Text)
    gtf_path: Mapped[str] = mapped_column(Text)
    index_dir: Mapped[str] = mapped_column(Text)

engine = create_engine(f"sqlite:///{DB_PATH}", echo=False, future=True)
Base.metadata.create_all(engine)

# ---------------------------------
# Flask app setup
# ---------------------------------
app = Flask(__name__)
CORS(app, resources={r"/*": {"origins": "*"}})
app.config['MAX_CONTENT_LENGTH'] = 10 * 1024 * 1024 * 1024


# ---------------------------------
# Helpers
# ---------------------------------
def now_iso(): return datetime.utcnow().isoformat() + "Z"

def run_to_dict(r: Run):
    return {
        "id": r.id,
        "created_at": r.created_at,
        "status": r.status,
        "progress": r.progress,
        "sample": {"name": r.sample_name, "files": json.loads(r.sample_files_json or "[]")},
        "params": json.loads(r.params_json or "{}"),
        "stages": [{
            "name": s.name,
            "status": s.status,
            "progress": s.progress,
            "time": s.time_iso,
            "metrics": json.loads(s.metrics_json or "{}"),
            "artifact": s.artifact_path or None,
        } for s in r.stages],
        # <-- include kind + path so the UI can group and label
        "artifacts": [{"kind": a.kind, "path": a.path} for a in r.artifacts],
    }


def sh(cmd: list[str], cwd: Path | None = None):
    """Run a tool installed in the container (fastqc, fastp, star, etc.)."""
    p = subprocess.run(cmd, cwd=cwd, text=True, capture_output=True)
    return p.returncode, p.stdout, p.stderr

def parse_fastqc_summary(fastqc_dir: Path) -> dict:
    """Parse FastQC summary.txt → {metric: status}."""
    summ = fastqc_dir / "summary.txt"
    out = {}
    if summ.exists():
        for line in summ.read_text().strip().splitlines():
            parts = line.split("\t")
            if len(parts) >= 3:
                status, metric, _ = parts[:3]
                out[metric] = status  # PASS/WARN/FAIL
    return out

def parse_star_final_log(path: Path) -> dict:
    d = {}
    try:
        for line in path.read_text().splitlines():
            if '|' in line:
                k, v = [x.strip() for x in line.split('|', 1)]
                d[k] = v
    except Exception:
        pass
    return d

def parse_featurecounts_summary(path: Path) -> dict:
    d = {}
    try:
        for line in path.read_text().splitlines():
            parts = line.strip().split('\t')
            if len(parts) >= 2:
                d[parts[0]] = parts[1]
    except Exception:
        pass
    return d

def _fastq_prefix(path: Path) -> str:
    """Return FASTQ name without compression or FASTQ extensions."""
    name = path.name
    for suf in (".gz", ".bz2", ".xz"):
        if name.endswith(suf):
            name = name[:-len(suf)]
    for suf in (".fastq", ".fq"):
        if name.endswith(suf):
            name = name[:-len(suf)]
    return name

def _mate_label(idx: int) -> str:
    return f"Read {idx + 1}"

def _collect_fastqc_reports(reads: list[dict], output_dir: Path) -> list[dict]:
    """Run FastQC on each read and capture artifact/metrics metadata."""
    reports = []
    for entry in reads:
        path = entry["path"]
        label = entry["label"]
        sh(["fastqc", str(path), "-o", str(output_dir), "--quiet", "--extract"])
        prefix = _fastq_prefix(path)
        html = output_dir / f"{prefix}_fastqc.html"
        summary_dir = output_dir / f"{prefix}_fastqc"
        images_dir = summary_dir / "Images"
        reports.append({
            "label": label,
            "html": html,
            "summary_dir": summary_dir,
            "images_dir": images_dir,
            "metrics": parse_fastqc_summary(summary_dir),
        })
    return reports

def _format_fastqc_metrics(reports: list[dict]) -> dict:
    """Flatten per-mate FastQC metrics and keep iframe metadata."""
    table = {}
    frames = []
    for rep in reports:
        label = rep["label"]
        metrics = rep.get("metrics") or {}
        for metric, status in metrics.items():
            table[f"{label} - {metric}"] = status
        html = rep.get("html")
        if html:
            frames.append({"label": label, "artifact": str(html)})
    return {"table": table, "fastqc_reports": frames}

def _emit_stage(session, run, name, pct, metrics=None, artifact=None, status="running"):
    st = Stage(
        run_id=run.id, name=name, status=status, progress=pct,
        time_iso=now_iso(), metrics_json=json.dumps(metrics or {}),
        artifact_path=str(artifact) if artifact else ""
    )
    session.add(st)
    run.progress = pct
    run.status = "finished" if pct >= 100 else "running"
    session.commit()

# ---------------------------------
# API routes
# ---------------------------------
@app.get("/api/health")
def health():
    return jsonify({"ok": True, "time": now_iso()})

@app.get("/api/runs")
def list_runs():
    with Session(engine) as s:
        rows = s.query(Run).order_by(Run.created_at.desc()).all()
        return jsonify({"ok": True, "runs": [run_to_dict(r) for r in rows]})

@app.post("/api/upload")
def upload():
    if 'files' not in request.files:
        return jsonify({"ok": False, "error": "No files uploaded"}), 400
    files = request.files.getlist('files')
    sample_name = request.form.get('sample_name') or f"sample-{uuid.uuid4().hex[:6]}"
    sample_dir = UPLOAD_DIR / sample_name  # FIXED: was STORAGE_DIR
    sample_dir.mkdir(parents=True, exist_ok=True)

    saved = []
    for f in files:
        fname = f.filename.replace("..", "_")
        dest = sample_dir / fname
        f.save(dest)
        saved.append(str(dest))
    return jsonify({"ok": True, "sample": {"name": sample_name, "files": saved}})

# --- Reference upload & indexing ---
@app.post("/api/reference")
def upload_reference():
    if 'fasta' not in request.files or 'gtf' not in request.files:
        return jsonify({"ok": False, "error": "Provide FASTA and GTF"}), 400
    name = request.form.get('name') or f"ref-{uuid.uuid4().hex[:6]}"
    ref_id = uuid.uuid4().hex
    base = STORAGE_ROOT / "references" / ref_id
    base.mkdir(parents=True, exist_ok=True)
    fasta = base / "genome.fa"
    gtf = base / "genes.gtf"
    request.files['fasta'].save(fasta)
    request.files['gtf'].save(gtf)
    index_dir = base / "star_index"
    with Session(engine) as s:
        r = Reference(id=ref_id, name=name, created_at=now_iso(), status="indexing",
                      fasta_path=str(fasta), gtf_path=str(gtf), index_dir=str(index_dir))
        s.add(r); s.commit()
    # async index build
    threading.Thread(target=_build_star_index, args=(ref_id,), daemon=True).start()
    return jsonify({"ok": True, "reference": {"id": ref_id, "name": name}})

def _build_star_index(ref_id: str):
    with Session(engine) as s:
        r = s.get(Reference, ref_id)
        if not r: return
        idx = Path(r.index_dir); idx.mkdir(parents=True, exist_ok=True)
        app.logger.info("STAR genomeGenerate start ref=%s id=%s", r.name, r.id)
        code, out, err = sh(["STAR", "--runThreadN", "4", "--runMode", "genomeGenerate",
                            "--genomeDir", str(idx), "--genomeFastaFiles", r.fasta_path,
                            "--sjdbGTFfile", r.gtf_path, "--sjdbOverhang", "100"])
        if code == 0:
            app.logger.info("STAR genomeGenerate completed ref=%s id=%s", r.name, r.id)
        else:
            app.logger.error("STAR genomeGenerate failed ref=%s id=%s code=%s\nstdout:\n%s\nstderr:\n%s",
                             r.name, r.id, code, out, err)
        r.status = "ready" if code == 0 else "failed"
        s.commit()

@app.get("/api/references")
def list_references():
    with Session(engine) as s:
        rows = s.query(Reference).order_by(Reference.created_at.desc()).all()
        return jsonify({"ok": True, "references": [
            {"id": r.id, "name": r.name, "status": r.status, "created_at": r.created_at}
        for r in rows]})

@app.post("/api/run")
def run_pipeline():
    data = request.get_json(force=True)
    sample = data.get('sample', {})
    params = data.get('params', {})

    job_id = uuid.uuid4().hex
    r = Run(
        id=job_id, created_at=now_iso(),
        status="queued", progress=0.0,
        sample_name=sample.get('name') or f"sample-{job_id[:6]}",
        sample_files_json=json.dumps(sample.get('files') or []),
        params_json=json.dumps(params or {})
    )
    with Session(engine) as s:
        s.add(r); s.commit()
    threading.Thread(target=_run_pipeline_real, args=(job_id,), daemon=True).start()
    return jsonify({"ok": True, "job_id": job_id})

@app.get("/api/status/<job_id>")
def status(job_id):
    with Session(engine) as s:
        r = s.get(Run, job_id)
        if not r:
            return jsonify({"ok": False, "error": "job not found"}), 404
        _ = r.stages, r.artifacts
        return jsonify({"ok": True, "job": run_to_dict(r)})

@app.get("/api/artifact")
def artifact():
    path = request.args.get('path')
    if not path or not Path(path).exists():
        return jsonify({"ok": False, "error": "artifact not found"}), 404
    return send_file(path, as_attachment=True)

# Serve FastQC folders so relative assets work inside iframe
@app.get("/api/qc/<job_id>/<path:rest>")
def qc_static(job_id, rest):
    """
    Serve files under QC_ROOT/<job_id>/... so the FastQC HTML page
    can load its relative CSS/JS/images inside an iframe.
    Example: /api/qc/a1b2c3/fastqc_post/index.html
    """
    base = (QC_ROOT / job_id).resolve()
    full = (base / rest).resolve()
    if not str(full).startswith(str(base)) or not full.exists():
        return jsonify({"ok": False, "error": "artifact not found"}), 404
    return send_file(full, conditional=True)

# Trigger alignment + quantification for an existing job using a reference
@app.post("/api/run_align")
def run_align():
    data = request.get_json(force=True)
    job_id = data.get('job_id'); ref_id = data.get('reference_id')
    if not job_id or not ref_id:
        return jsonify({"ok": False, "error": "job_id and reference_id required"}), 400
    threading.Thread(target=_continue_alignment, args=(job_id, ref_id), daemon=True).start()
    return jsonify({"ok": True})

# ---------------------------------
# Pipeline (FastQC + fastp; slots left for STAR/featureCounts)
# ---------------------------------

def _add_fastqc_plots(session: Session, run: Run, images_dir: Path, tag: str, mate_label: str | None = None):
    """
    Add each FastQC PNG as an Artifact with kind 'fastqc_plot_{tag}:{name}'.
    tag = 'raw' or 'post' (or anything you like)
    """
    if not images_dir.exists():
        return
    mate = (mate_label or "read1").lower().replace(" ", "_")
    for png in sorted(images_dir.glob("*.png")):
        name = png.stem  # e.g., per_base_quality
        session.add(Artifact(
            run_id=run.id,
            kind=f"fastqc_plot_{tag}:{mate}:{name}",
            path=str(png)
        ))
    session.commit()


def _run_pipeline_real(job_id: str):
    with Session(engine) as s:
        r = s.get(Run, job_id)
        if not r: return
        r.status = "running"; s.commit()

        files = json.loads(r.sample_files_json or "[]")
        if not files:
            _emit_stage(s, r, "error", 100, {"error":"no files"}, status="failed")
            return

        # Detect paired-end
        reads = []
        for idx, path_str in enumerate(files[:2]):
            reads.append({"path": Path(path_str), "label": _mate_label(idx)})
        r1 = reads[0]["path"]
        r2 = reads[1]["path"] if len(reads) > 1 else None

        # FIXED: use QC_ROOT so iframe route can serve assets
        work = (QC_ROOT / job_id); work.mkdir(parents=True, exist_ok=True)

        # 1) Raw FastQC (HTML + extracted folder for summary)
        raw_dir = work / "fastqc_raw"; raw_dir.mkdir(exist_ok=True)
        raw_reports = _collect_fastqc_reports(reads, raw_dir)
        raw_metrics = _format_fastqc_metrics(raw_reports)
        first_raw = raw_metrics["fastqc_reports"][0]["artifact"] if raw_metrics["fastqc_reports"] else None
        _emit_stage(s, r, "pre_qc_fastqc", 15, metrics=raw_metrics, artifact=first_raw)
        for rep in raw_reports:
            _add_fastqc_plots(s, r, rep["images_dir"], tag="raw", mate_label=rep["label"])

        # 2) Trimming (fastp)
        trim_dir = work / "trim"; trim_dir.mkdir(exist_ok=True)
        prefix = _fastq_prefix(r1)
        trimmed_r1 = trim_dir / f"{prefix}_trimmed.fastq.gz"
        fastp_html = work / f"{prefix}_fastp.html"
        fastp_json = work / f"{prefix}_fastp.json"

        trimmed_reads = [{"label": reads[0]["label"], "path": trimmed_r1}]
        if r2:
            trimmed_r2 = trim_dir / f"{_fastq_prefix(r2)}_trimmed.fastq.gz"
            trimmed_reads.append({"label": reads[1]["label"], "path": trimmed_r2})
            sh([
                "fastp",
                "-i", str(r1), "-I", str(r2),
                "-o", str(trimmed_r1), "-O", str(trimmed_r2),
                "-h", str(fastp_html), "-j", str(fastp_json),
                "-w", "4"
            ])
        else:
            sh([
                "fastp", "-i", str(r1), "-o", str(trimmed_r1),
                "-h", str(fastp_html), "-j", str(fastp_json),
                "-w", "4"
            ])

        fastp_metrics = {}
        if fastp_json.exists():
            try:
                fastp_metrics = json.loads(fastp_json.read_text()).get("summary", {})
            except Exception:
                fastp_metrics = {"note": "could not parse fastp json"}

        _emit_stage(s, r, "trim_fastp", 45, metrics=fastp_metrics, artifact=str(trim_dir))
        manifest = trim_dir / "trimmed_reads.json"
        manifest.write_text(json.dumps({"mates": [{"label": t["label"], "path": str(t["path"])} for t in trimmed_reads]}, indent=2))

        # 3) Post-trim FastQC (HTML + extracted folder)
        post_dir = work / "fastqc_post"; post_dir.mkdir(exist_ok=True)
        post_reports = _collect_fastqc_reports(trimmed_reads, post_dir)
        post_metrics = _format_fastqc_metrics(post_reports)
        first_post = post_metrics["fastqc_reports"][0]["artifact"] if post_metrics["fastqc_reports"] else None
        _emit_stage(s, r, "post_qc_fastqc", 65, metrics=post_metrics, artifact=first_post)
        for rep in post_reports:
            _add_fastqc_plots(s, r, rep["images_dir"], tag="post", mate_label=rep["label"])

        # 4) Finish QC-only pipeline
        _emit_stage(s, r, "summary", 100, metrics={"status": "complete"}, artifact=str(work), status="finished")

def _continue_alignment(job_id: str, ref_id: str):
    with Session(engine) as s:
        run = s.get(Run, job_id)
        ref = s.get(Reference, ref_id)
        if not run or not ref or ref.status != 'ready':
            return
        files = json.loads(run.sample_files_json or "[]")
        if not files:
            _emit_stage(s, run, "error", run.progress, {"error":"no files"}, status="failed")
            return
        work = (QC_ROOT / job_id)
        trim_dir = work / "trim"
        manifest_path = trim_dir / "trimmed_reads.json"
        trimmed = []
        if manifest_path.exists():
            try:
                data = json.loads(manifest_path.read_text()).get("mates", [])
                for entry in data:
                    if entry.get("path"):
                        trimmed.append(Path(entry["path"]))
            except Exception:
                trimmed = []
        if not trimmed:
            trimmed = sorted(trim_dir.glob("*_trimmed.fastq.gz"))
        if not trimmed:
            _emit_stage(s, run, "error", run.progress, {"error":"trimmed reads not found"}, status="failed")
            return
        # STAR alignment
        align_dir = work / "star_align"; align_dir.mkdir(exist_ok=True)
        bam_path = align_dir / "Aligned.sortedByCoord.out.bam"
        args = ["STAR", "--runThreadN", "4", "--genomeDir", ref.index_dir, "--readFilesIn"]
        reads_for_star = [str(p) for p in trimmed[:2]]
        args += reads_for_star
        if any(str(p).endswith('.gz') for p in trimmed[:2]):
            args += ["--readFilesCommand", "zcat"]
        args += ["--outFileNamePrefix", str(align_dir) + "/", "--outSAMtype", "BAM", "SortedByCoordinate"]
        code, out, err = sh(args)
        log_final = align_dir / "Log.final.out"
        metrics = parse_star_final_log(log_final) if log_final.exists() else {"note":"no Log.final.out"}
        _emit_stage(s, run, "align_star", 85, metrics=metrics, artifact=str(bam_path))

        # featureCounts quantification
        counts_txt = work / "featurecounts.txt"
        summ_txt = work / "featurecounts.summary"
        code, out, err = sh(["featureCounts", "-T", "4", "-a", ref.gtf_path, "-o", str(counts_txt), str(bam_path)])
        # featureCounts writes summary next to output
        metrics2 = parse_featurecounts_summary(summ_txt) if summ_txt.exists() else {"Assigned": "n/a"}
        _emit_stage(s, run, "quant_featurecounts", 95, metrics=metrics2, artifact=str(counts_txt))

        # Final summary after alignment path
        _emit_stage(s, run, "analysis_summary", 100, metrics={"status":"complete"}, artifact=str(work), status="finished")

# ---------------------------------
# Run
# ---------------------------------
if __name__ == "__main__":
    app.run(host="0.0.0.0", port=API_PORT, debug=True)
