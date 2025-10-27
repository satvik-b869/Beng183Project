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

STORAGE_ROOT = Path(os.getenv("STORAGE_ROOT", "storage"))
UPLOAD_DIR = STORAGE_ROOT / "uploads"
ARTIFACTS_DIR = STORAGE_ROOT / "artifacts"
DB_DIR = Path("db")
DB_DIR.mkdir(parents=True, exist_ok=True)
DB_PATH = DB_DIR / "rnaseq.sqlite"

for d in (UPLOAD_DIR, ARTIFACTS_DIR):
    d.mkdir(parents=True, exist_ok=True)

# ---------------------------------
# Database models
# ---------------------------------
class Base(DeclarativeBase):
    pass

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
# Helper utilities
# ---------------------------------
def now_iso(): return datetime.utcnow().isoformat() + "Z"

def run_to_dict(r: Run):
    """Convert ORM Run object to dict for JSON responses."""
    return {
        "id": r.id,
        "created_at": r.created_at,
        "status": r.status,
        "progress": r.progress,
        "sample": {
            "name": r.sample_name,
            "files": json.loads(r.sample_files_json or "[]"),
        },
        "params": json.loads(r.params_json or "{}"),
        "stages": [
            {
                "name": s.name,
                "status": s.status,
                "progress": s.progress,
                "time": s.time_iso,
                "metrics": json.loads(s.metrics_json or "{}"),
                "artifact": s.artifact_path or None,
            } for s in r.stages
        ],
        "artifacts": [a.path for a in r.artifacts],
    }

def run_cmd(cmd: list[str], cwd: Path | None = None):
    """Helper for executing shell commands (FastQC, fastp, etc.)."""
    p = subprocess.run(cmd, cwd=cwd, text=True, capture_output=True)
    return p.returncode, p.stdout, p.stderr

# ---------------------------------
# API routes
# ---------------------------------
@app.route("/api/health", methods=["GET"])
def health():
    return jsonify({"ok": True, "time": now_iso()})

@app.route("/api/runs", methods=["GET"])
def list_runs():
    with Session(engine) as s:
        rows = s.query(Run).order_by(Run.created_at.desc()).all()
        return jsonify({"ok": True, "runs": [run_to_dict(r) for r in rows]})

@app.route("/api/upload", methods=["POST"])
def upload():
    """Accept file uploads from the frontend."""
    if 'files' not in request.files:
        return jsonify({"ok": False, "error": "No files uploaded"}), 400

    files = request.files.getlist('files')
    sample_name = request.form.get('sample_name') or f"sample-{uuid.uuid4().hex[:6]}"
    sample_dir = UPLOAD_DIR / sample_name
    sample_dir.mkdir(parents=True, exist_ok=True)

    saved = []
    for f in files:
        dest = sample_dir / f.filename.replace("..", "_")
        f.save(dest)
        saved.append(str(dest.resolve()))

    return jsonify({"ok": True, "sample": {"name": sample_name, "files": saved}})

@app.route("/api/run", methods=["POST"])
def run_pipeline():
    """Launch RNA-seq analysis in background."""
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
        s.add(r)
        s.commit()

    # Background worker thread
    t = threading.Thread(target=_run_pipeline_realistic, args=(job_id,), daemon=True)
    t.start()

    return jsonify({"ok": True, "job_id": job_id})

@app.route("/api/status/<job_id>", methods=["GET"])
def status(job_id):
    with Session(engine) as s:
        r = s.get(Run, job_id)
        if not r:
            return jsonify({"ok": False, "error": "job not found"}), 404
        _ = r.stages, r.artifacts
        return jsonify({"ok": True, "job": run_to_dict(r)})

@app.route("/api/artifact", methods=["GET"])
def artifact():
    path = request.args.get('path')
    if not path or not Path(path).exists():
        return jsonify({"ok": False, "error": "artifact not found"}), 404
    return send_file(path, as_attachment=True)

# ---------------------------------
# Internal helpers for pipeline logging
# ---------------------------------
def _emit_stage(session, run, name, pct, metrics=None, artifact=None, status="running"):
    """Record one stage (progress update) into SQLite."""
    st = Stage(
        run_id=run.id, name=name, status=status, progress=pct,
        time_iso=now_iso(), metrics_json=json.dumps(metrics or {}),
        artifact_path=artifact or ""
    )
    session.add(st)
    run.progress = pct
    run.status = "finished" if pct >= 100 else "running"
    session.commit()

# ---------------------------------
# RNA-seq pipeline (stub: replace with real tool calls)
# ---------------------------------
def _run_pipeline_realistic(job_id: str):
    """This function defines the sequential RNA-seq steps.
    Replace each block with your actual tool invocation (fastp, FastQC, Salmon, etc.)
    """
    with Session(engine) as s:
        r = s.get(Run, job_id)
        if not r:
            return
        r.status = "running"
        s.commit()

        files = json.loads(r.sample_files_json or "[]")
        r1 = files[0] if files else None
        r2 = files[1] if len(files) > 1 else None
        work = ARTIFACTS_DIR / job_id
        work.mkdir(parents=True, exist_ok=True)

        # 1️⃣ Raw FastQC
        raw_qc = work / "fastqc_raw"
        time.sleep(1)
        _emit_stage(s, r, "pre_qc_raw", 10, metrics={"note": "FastQC raw done"}, artifact=str(raw_qc))

        # 2️⃣ Trimming
        trim_out = work / "trim"
        time.sleep(1)
        _emit_stage(s, r, "trim", 35, metrics={"reads_retained_pct": 97.8}, artifact=str(trim_out))

        # 3️⃣ Post-trim QC
        post_qc = work / "fastqc_post_trim"
        time.sleep(1)
        _emit_stage(s, r, "fastqc_post_trim", 50, metrics={"note": "Post-trim FastQC"}, artifact=str(post_qc))

        # 4️⃣ Quantification (e.g., Salmon)
        quant_out = work / "salmon_quant"
        time.sleep(1)
        _emit_stage(s, r, "quant_salmon", 80, metrics={"genes_detected": 16842}, artifact=str(quant_out))

        # 5️⃣ Post-alignment QC or skip
        _emit_stage(s, r, "post_align_qc", 95, metrics={"note": "Alignment-free path; skip alignment"})

        # 6️⃣ Summary
        _emit_stage(s, r, "summary", 100, metrics={"status": "complete"})

# ---------------------------------
# Run Flask app
# ---------------------------------
if __name__ == "__main__":
    app.run(host="127.0.0.1", port=API_PORT, debug=True)
