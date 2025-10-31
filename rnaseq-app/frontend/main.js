// ------- config -------
const API_BASE = "http://localhost:5050";
document.getElementById("api-label").textContent = API_BASE;

// ------- small helpers -------
// ---- FastQC plot helpers ----
const PRETTY = {
  per_base_quality: "Per-base Quality",
  per_sequence_quality: "Per-sequence Quality",
  per_base_sequence_content: "Per-base Sequence Content",
  per_base_gc_content: "Per-base GC Content",
  per_sequence_gc_content: "Per-sequence GC Content",
  per_base_n_content: "Per-base N Content",
  seq_length_distribution: "Sequence Length Distribution",
  duplication_levels: "Sequence Duplication Levels",
  overrepresented_sequences: "Overrepresented Sequences",
  adapter_content: "Adapter Content",
};

function artifactUrl(p) {
  return `${API_BASE}/api/artifact?path=${encodeURIComponent(p)}`;
}

function renderFastqcSection(container, artifacts, tag, title) {
  const plots = (artifacts || [])
    .filter(a => a.kind && a.kind.startsWith(`fastqc_plot_${tag}:`))
    .map(a => {
      const key = a.kind.split(":")[1] || "plot";
      return { src: artifactUrl(a.path), label: PRETTY[key] || key };
    });

  if (!plots.length) return;

  const sec = document.createElement("section");
  sec.className = "fastqc-section";
  sec.innerHTML = `<h3>${title}</h3>`;
  plots.forEach(p => {
    const card = document.createElement("div");
    card.className = "fastqc-card";
    card.innerHTML = `
      <figure>
        <img src="${p.src}" alt="${p.label}" loading="lazy" />
        <figcaption>${p.label}</figcaption>
      </figure>
    `;
    sec.appendChild(card);
  });
  container.appendChild(sec);
}

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
  el("status").innerHTML = `<div class="json">${escapeHtml(JSON.stringify(obj,null,2))}</div>`;
}

function qcUrlFromArtifactPath(artifactPath) {
  // artifactPath looks like: /data/qc/<job_id>/fastqc_post/SRR..._fastqc.html
  const parts = artifactPath.split("/").filter(Boolean);   // ["data","qc","<job>","fastqc_post",...]
  const job = parts[2];
  const rest = parts.slice(3).join("/");
  return `${API_BASE}/api/qc/${job}/${rest}`;              // e.g. http://localhost:5050/api/qc/<job>/<rest>
}


function showFastQC(url){
  el("status").innerHTML = `
    <iframe class="fastqc-frame" src="${url}"></iframe>
    <div class="muted" style="margin-top:8px">
      Open in new tab: <a href="${url}" target="_blank" rel="noopener">${url}</a>
    </div>`;
}

// ------- state -------
let lastSample=null, jobId=null, pollTimer=null;

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
  } catch (err) { el("upload-msg").textContent = "Upload failed"; showJSON({error:String(err)}); }
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
  } catch (err) { showJSON({error:String(err)}); el("run-btn").disabled = false; }
});

el("refresh-btn").addEventListener("click", async ()=>{
  try { const r = await jfetch(`${API_BASE}/api/runs`);
    if (!r.runs?.length) return showJSON({info:"No runs yet."});
    showJSON(r.runs[0]); setBar(r.runs[0].progress||0);
  } catch (e) { showJSON({error:String(e)}); }
});

// ------- poll + EMBED -------
async function pollStatus() {
  if (!jobId) return;
  try {
    const st = await jfetch(`${API_BASE}/api/status/${jobId}`);
    setBar(st?.job?.progress ?? 0);

    // Prefer post-trim FastQC HTML; otherwise any FastQC HTML
    const stages = st?.job?.stages ?? [];
    let stage =
      stages.find(s => /fastqc_post/i.test(s.name) && typeof s.artifact === "string" && s.artifact.endsWith(".html")) ||
      stages.find(s => typeof s.artifact === "string" && s.artifact.endsWith(".html"));

    showJSON(st);

    // ---- NEW: render individual FastQC plots ----
    const plotsRoot = el("plots");                 // NEW (requires a <div id="plots"> in HTML)
    if (plotsRoot) {
      plotsRoot.innerHTML = "";                    // NEW: clear previous
      const arts = st?.job?.artifacts || [];       // NEW
      renderFastqcSection(plotsRoot, arts, "raw",  "FastQC (Raw)");       // NEW
      renderFastqcSection(plotsRoot, arts, "post", "FastQC (Post-trim)"); // NEW
    }
    // ---------------------------------------------

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


