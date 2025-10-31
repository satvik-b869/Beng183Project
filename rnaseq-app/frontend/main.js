// ------- config -------
const API_BASE = "http://localhost:5050";
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

function showJSON(obj){
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

el("refresh-btn").addEventListener("click", async ()=>{
  try {
    const r = await jfetch(`${API_BASE}/api/runs`);
    if (!r.runs?.length) return showJSON({info:"No runs yet."});
    showJSON(r.runs[0]);
    setBar(r.runs[0].progress||0);
  } catch (e) {
    showJSON({error:String(e)});
  }
});

// ------- poll (JSON + plots only) -------
async function pollStatus() {
  if (!jobId) return;
  try {
    const st = await jfetch(`${API_BASE}/api/status/${jobId}`);
    setBar(st?.job?.progress ?? 0);

    // Show compact JSON status
    el("status").innerHTML = "";


    // Render per-plot gallery (raw + post)
    const plotsRoot = el("plots");
    if (plotsRoot) {
      plotsRoot.innerHTML = "";
      const arts = st?.job?.artifacts || [];
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
