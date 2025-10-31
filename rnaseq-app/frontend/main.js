// ------- config -------
const API_BASE = "http://localhost:5050";
document.getElementById("api-label").textContent = API_BASE;

// ------- small helpers -------
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

    if (stage?.artifact) {
      // Use folder-based URL so CSS/JS/images resolve inside the iframe
      const url = qcUrlFromArtifactPath(stage.artifact);
      showFastQC(url);
    } else {
      // No HTML yet — show JSON status
      showJSON(st);
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

