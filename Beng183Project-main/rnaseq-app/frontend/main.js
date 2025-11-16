// ------- config -------
// Point the frontend to the backend container published on host:5050
const API_PORT = 5050;
const API_BASE = `${window.location.protocol}//${window.location.hostname}:${API_PORT}`;
document.getElementById("api-label").textContent = API_BASE;

// Use the plots module (plots.js must be loaded before this file)
const renderFastqcSection = PlotUI.renderFastqcSectionFactory(API_BASE);

// ------- tiny helpers -------
async function jfetch(url, opts) {
  const r = await fetch(url, opts);
  const t = await r.text();
  if (!r.ok) throw new Error(`${r.status} ${t}`);
  try { return JSON.parse(t); } catch { return { raw: t }; }
}
const el = (id) => document.getElementById(id);
function setBar(pct){ pct=Math.max(0,Math.min(100,Math.floor(pct||0))); el("bar").style.width=pct+"%"; }
function escapeHtml(s){return s.replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));}

const analysisBtn = el("analysis-btn");
if (analysisBtn) {
  analysisBtn.addEventListener("click", () => {
    window.location.href = "./alignment.html";
  });
}

function showJSON(obj){
  const s = el("status");
  if (!s) return;
  const pretty = escapeHtml(JSON.stringify(obj, null, 2));
  s.innerHTML = `<div class="json">${pretty}</div>`;
}

// ------- state -------
let lastSample = null;
let jobId = null;
let pollTimer = null;

// ------- upload -------
document.getElementById("upload-form").addEventListener("submit", async (e)=>{
  e.preventDefault();
  const files = el("file-input").files;
  const name = el("sample-name").value || "sample";
  if (!files.length) return alert("Pick at least one FASTQ");

  const fd = new FormData();
  fd.append("sample_name", name);
  for (const f of files) fd.append("files", f);

  el("upload-msg").textContent = "Uploading…";
  try {
    const res = await jfetch(`${API_BASE}/api/upload`, { method:"POST", body:fd });
    lastSample = res.sample;
    el("upload-msg").textContent = `Uploaded: ${lastSample.files.length} file(s)`;
    el("run-btn").disabled = false;
    showJSON({info:"Ready to run", sample:lastSample});
  } catch (err) {
    el("upload-msg").textContent = "Upload failed";
    showJSON({error:String(err)});
  }
});

// ------- run -------
el("run-btn").addEventListener("click", async ()=>{
  if (!lastSample) return alert("Upload first");
  el("run-btn").disabled = true;
  setBar(0);
  showJSON({info:"Starting run…"});
  try {
    const res = await jfetch(`${API_BASE}/api/run`, {
      method:"POST",
      headers:{ "Content-Type":"application/json" },
      body: JSON.stringify({ sample:lastSample, params:{} })
    });
    jobId = res.job_id;
    pollStatus();
    pollTimer = setInterval(pollStatus, 2000);
  } catch (err) {
    showJSON({error:String(err)});
    el("run-btn").disabled = false;
  }
});

const refreshBtn = el("refresh-btn");
if (refreshBtn) {
  refreshBtn.addEventListener("click", async ()=>{
    try {
      const r = await jfetch(`${API_BASE}/api/runs`);
      if (!r.runs?.length) return showJSON({info:"No runs yet."});
      showJSON(r.runs[0]);
      setBar(r.runs[0].progress||0);
    } catch (e) {
      showJSON({error:String(e)});
    }
  });
}

// ------- poll (JSON + plots only) -------
async function pollStatus() {
  if (!jobId) return;
  try {
    const st = await jfetch(`${API_BASE}/api/status/${jobId}`);
    setBar(st?.job?.progress ?? 0);

    // Clear the status area so we can render the QC HTML and metrics
    const statusRoot = el("status");
    statusRoot.innerHTML = "";

    const job = st?.job || {};
    const stages = job.stages || [];
    const findStage = (n) => stages.find(s => s.name === n);

    // Helper to convert absolute artifact path -> served QC URL
    function toQcUrl(jobId, artifactPath){
      if (!artifactPath) return null;
      const marker = `/${jobId}/`;
      let rest = null;
      if (artifactPath.includes(marker)) {
        rest = artifactPath.split(marker)[1];
      } else if (artifactPath.includes('/data/qc/')) {
        rest = artifactPath.split('/data/qc/')[1].split('/')?.slice(1).join('/') || null;
      }
      return rest ? `${API_BASE}/api/qc/${jobId}/${rest}` : null;
    }

    // Render FastQC HTML (raw and post-trim) in iframes when available
    function addQcFrame(title, stage){
      if (!stage) return;
      const reports = Array.isArray(stage.metrics?.fastqc_reports) && stage.metrics.fastqc_reports.length
        ? stage.metrics.fastqc_reports
        : (stage.artifact ? [{ label: null, artifact: stage.artifact }] : []);
      if (!reports.length) return;
      const sec = document.createElement('section');
      sec.innerHTML = `<h3>${title}</h3>`;
      reports.forEach((rep, idx) => {
        const frameUrl = toQcUrl(job.id, rep.artifact);
        if (!frameUrl) return;
        const block = document.createElement('div');
        block.style.marginBottom = '12px';
        const label = rep.label || (reports.length > 1 ? `Mate ${idx + 1}` : null);
        if (label) {
          const span = document.createElement('div');
          span.className = 'muted';
          span.textContent = label;
          block.appendChild(span);
        }
        const frame = document.createElement('iframe');
        frame.className = 'fastqc-frame';
        frame.src = frameUrl;
        frame.loading = 'lazy';
        block.appendChild(frame);
        sec.appendChild(block);
      });
      statusRoot.appendChild(sec);
      const metricsTable = stage.metrics?.table || stage.metrics;
      if (window.PlotUI && metricsTable) {
        window.PlotUI.renderMetrics(statusRoot, `${title} Metrics`, metricsTable);
      }
    }

    addQcFrame('FastQC (Raw)', findStage('pre_qc_fastqc'));
    addQcFrame('FastQC (Post-trim)', findStage('post_qc_fastqc'));

    // Render per-plot gallery (raw + post)
    const plotsRoot = el("plots");
    if (plotsRoot) {
      plotsRoot.innerHTML = "";
      const arts = job.artifacts || [];
      renderFastqcSection(plotsRoot, arts, "raw",  "FastQC (Raw)");
      renderFastqcSection(plotsRoot, arts, "post", "FastQC (Post-trim)");
    }

    if (["finished", "failed"].includes(st?.job?.status)) {
      clearInterval(pollTimer);
      el("run-btn").disabled = false;
    }
  } catch (err) {
    clearInterval(pollTimer);
    showJSON({ error: String(err) });
    el("run-btn").disabled = false;
  }
}
