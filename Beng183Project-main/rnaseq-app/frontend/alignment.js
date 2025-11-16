// Simple analysis UI for alignment and quantification
(function(){
  const API_PORT = 5050;
  const API_BASE = `${window.location.protocol}//${window.location.hostname}:${API_PORT}`;
  const el = (id)=>document.getElementById(id);
  el('api-label').textContent = API_BASE;

  const fastqcBtn = el('fastqc-btn');
  if (fastqcBtn) {
    fastqcBtn.addEventListener('click', () => {
      window.location.href = './indexS.html';
    });
  }

  async function jfetch(url, opts){
    const r = await fetch(url, opts);
    const t = await r.text();
    if (!r.ok) throw new Error(`${r.status} ${t}`);
    try { return JSON.parse(t); } catch { return { raw:t }; }
  }
  function setMsg(id, s){ el(id).textContent = s; }
  function appendLog(s){ const div=el('log'); div.textContent += (div.textContent?"\n":"")+s; div.scrollTop = div.scrollHeight; }

  async function refreshRefs(){
    const sel = el('ref-select');
    sel.innerHTML = '';
    try{
      const r = await jfetch(`${API_BASE}/api/references`);
      for(const ref of r.references||[]){
        const opt = document.createElement('option');
        opt.value = ref.id; opt.textContent = `${ref.name} — ${ref.status}`;
        if (ref.status !== 'ready') opt.disabled = true;
        sel.appendChild(opt);
      }
    }catch(e){ appendLog(`refs error: ${String(e)}`); }
    setAlignEnabled();
  }

  async function refreshRuns(){
    const sel = el('run-select'); sel.innerHTML = '';
    try{
      const r = await jfetch(`${API_BASE}/api/runs`);
      for(const run of r.runs||[]){
        const opt = document.createElement('option');
        opt.value = run.id;
        opt.textContent = `${run.sample.name} — ${run.status} — ${Math.floor(run.progress)}%`;
        if (run.status !== 'finished') opt.disabled = true;
        sel.appendChild(opt);
      }
    }catch(e){ appendLog(`runs error: ${String(e)}`); }
    setAlignEnabled();
  }

  function setAlignEnabled(){
    const refOpt = el('ref-select').selectedOptions[0];
    const runOpt = el('run-select').selectedOptions[0];
    el('align-btn').disabled = !(refOpt && !refOpt.disabled && runOpt && !runOpt.disabled);
  }
  el('ref-select').addEventListener('change', setAlignEnabled);
  el('run-select').addEventListener('change', setAlignEnabled);

  // Upload and index reference
  el('ref-form').addEventListener('submit', async (e)=>{
    e.preventDefault();
    const name = el('ref-name').value || 'ref';
    const fasta = el('ref-fasta').files[0];
    const gtf = el('ref-gtf').files[0];
    if (!fasta || !gtf) return alert('Pick FASTA and GTF');
    const fd = new FormData();
    fd.append('name', name);
    fd.append('fasta', fasta);
    fd.append('gtf', gtf);
    setMsg('ref-msg','Uploading…');
    try{
      const res = await jfetch(`${API_BASE}/api/reference`, { method:'POST', body: fd });
      setMsg('ref-msg', `Reference queued: ${res.reference?.name || name}`);
      await refreshRefs();
    }catch(err){ setMsg('ref-msg','Upload failed'); appendLog(String(err)); }
  });

  // Run alignment + featureCounts on selected run with selected reference
  el('align-btn').addEventListener('click', async ()=>{
    const jobId = el('run-select').value; const refId = el('ref-select').value;
    appendLog(`Starting alignment for job ${jobId} with reference ${refId}`);
    try{
      await jfetch(`${API_BASE}/api/run_align`, {
        method:'POST', headers:{'Content-Type':'application/json'},
        body: JSON.stringify({ job_id: jobId, reference_id: refId })
      });
      poll(jobId);
    }catch(e){ appendLog(String(e)); }
  });

  let timer=null;
  async function poll(jobId){
    if (timer) clearInterval(timer);
    async function once(){
      try{
        const st = await jfetch(`${API_BASE}/api/status/${jobId}`);
        const job = st.job || {}; const stages = job.stages || [];
        const resDiv = el('results'); resDiv.innerHTML = '';
        function addKV(title, obj){
          if (!obj || !Object.keys(obj).length) return;
          const tbl = document.createElement('table');
          tbl.innerHTML = `<thead><tr><th colspan="2">${title}</th></tr></thead><tbody></tbody>`;
          const tb = tbl.querySelector('tbody');
          for(const [k,v] of Object.entries(obj)){
            const tr = document.createElement('tr'); tr.innerHTML = `<td>${k}</td><td>${v}</td>`; tb.appendChild(tr);
          }
          resDiv.appendChild(tbl);
        }
        const get = (n)=>stages.find(s=>s.name===n);
        addKV('STAR Mapping (summary)', get('align_star')?.metrics);
        addKV('featureCounts (summary)', get('quant_featurecounts')?.metrics);
        const countsPath = get('quant_featurecounts')?.artifact;
        if (countsPath) el('counts-download').href = `${API_BASE}/api/artifact?path=${encodeURIComponent(countsPath)}`;
        if (["finished","failed"].includes(job.status)) clearInterval(timer);
      }catch(e){ appendLog(`poll error: ${String(e)}`); clearInterval(timer); }
    }
    timer = setInterval(once, 2000); await once();
  }

  el('refresh-refs').addEventListener('click', refreshRefs);
  el('refresh-runs').addEventListener('click', refreshRuns);
  refreshRefs(); refreshRuns();
})();
