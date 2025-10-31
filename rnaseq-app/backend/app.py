import os, json, time, threading, uuid, subprocess
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

engine = create_engine(f"sqlite:///{DB_PATH}", echo=False, future=True)
Base.metadata.create_all(engine)

# ---------------------------------
# Flask app setup
# ---------------------------------
app = Flask(__name__)
CORS(app, resources={r"/*": {"origins": "*"}})
app.config['MAX_CONTENT_LENGTH'] = 2 * 1024 * 1024 * 1024  # 2 GB

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
    Example: /api/qc/a1b2c3/fastqc_post/SRR14546391_fastqc.html
    """
    base = (QC_ROOT / job_id).resolve()
    full = (base / rest).resolve()
    if not str(full).startswith(str(base)) or not full.exists():
        return jsonify({"ok": False, "error": "artifact not found"}), 404
    return send_file(full, conditional=True)

# ---------------------------------
# Pipeline (FastQC + fastp; slots left for STAR/featureCounts)
# ---------------------------------

def _add_fastqc_plots(session: Session, run: Run, images_dir: Path, tag: str):
    """
    Add each FastQC PNG as an Artifact with kind 'fastqc_plot_{tag}:{name}'.
    tag = 'raw' or 'post' (or anything you like)
    """
    if not images_dir.exists():
        return
    for png in sorted(images_dir.glob("*.png")):
        name = png.stem  # e.g., per_base_quality
        session.add(Artifact(
            run_id=run.id,
            kind=f"fastqc_plot_{tag}:{name}",
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
        r1 = Path(files[0])
        r2 = Path(files[1]) if len(files) > 1 else None

        # FIXED: use QC_ROOT so iframe route can serve assets
        work = (QC_ROOT / job_id); work.mkdir(parents=True, exist_ok=True)

        # 1) Raw FastQC (HTML + extracted folder for summary)
        raw_dir = work / "fastqc_raw"; raw_dir.mkdir(exist_ok=True)
        sh(["fastqc", str(r1), "-o", str(raw_dir), "--quiet", "--extract"])
        if r2:
            sh(["fastqc", str(r2), "-o", str(raw_dir), "--quiet", "--extract"])

        prefix = Path(r1.name).with_suffix("").with_suffix("").name  # handle .fastq.gz
        raw_html = raw_dir / f"{prefix}_fastqc.html"
        raw_sum_dir = raw_dir / f"{prefix}_fastqc"                  # contains summary.txt
        raw_images = raw_sum_dir / "Images"                    # NEW
        metrics = parse_fastqc_summary(raw_sum_dir)
        _emit_stage(s, r, "pre_qc_fastqc", 15, metrics=metrics, artifact=str(raw_html))
        _add_fastqc_plots(s, r, raw_images, tag="raw")         # NEW

        # 2) Trimming (fastp)
        trim_dir = work / "trim"; trim_dir.mkdir(exist_ok=True)
        trimmed_r1 = trim_dir / f"{prefix}_trimmed.fastq.gz"
        fastp_html = work / f"{prefix}_fastp.html"
        fastp_json = work / f"{prefix}_fastp.json"

        if r2:
            prefix2 = Path(r2.name).with_suffix("").with_suffix("").name
            trimmed_r2 = trim_dir / f"{prefix2}_trimmed.fastq.gz"
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

        # 3) Post-trim FastQC (HTML + extracted folder)
        post_dir = work / "fastqc_post"; post_dir.mkdir(exist_ok=True)
        sh(["fastqc", str(trimmed_r1), "-o", str(post_dir), "--quiet", "--extract"])

        post_prefix = Path(trimmed_r1.name).with_suffix("").with_suffix("").name
        post_html = post_dir / f"{post_prefix}_fastqc.html"
        post_sum_dir = post_dir / f"{post_prefix}_fastqc"
        post_images = post_sum_dir / "Images"                  # NEW
        post_metrics = parse_fastqc_summary(post_sum_dir)
        _emit_stage(s, r, "post_qc_fastqc", 65, metrics=post_metrics, artifact=str(post_html))
        _add_fastqc_plots(s, r, post_images, tag="post")       # NEW

        # 4) Alignment/Quantification (future)
        # _emit_stage(s, r, "align_star", 85, metrics={...}, artifact=str(...))

        # 5) Summary
        _emit_stage(s, r, "summary", 100, metrics={"status": "complete"}, artifact=str(work), status="finished")

# ---------------------------------
# Run
# ---------------------------------
if __name__ == "__main__":
    app.run(host="0.0.0.0", port=API_PORT, debug=True)
