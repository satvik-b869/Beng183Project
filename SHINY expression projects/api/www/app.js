const fileInput = document.getElementById("data-file");
const exportBtn = document.getElementById("export-btn");
const saveBtn = document.getElementById("save-settings-btn");
const loadBtn = document.getElementById("load-settings-btn");
const savedSelect = document.getElementById("saved-settings");
const settingsNameInput = document.getElementById("settings-name");
const loadingIndicator = document.getElementById("loading-indicator");
const plotImage = document.getElementById("plot-image");
const controls = document.querySelectorAll("[data-param]");
const showMACheckbox = document.querySelector('[data-param="showMA"]');
const emptyState = document.querySelector("[data-empty]");
const fileStatus = document.getElementById("file-status");
const plotContainer = document.getElementById("plot-container");
const zoomControls = document.getElementById("plot-zoom-controls");
const zoomInBtn = document.getElementById("zoom-in");
const zoomOutBtn = document.getElementById("zoom-out");


let currentFile = null;
let activeUrl = null;
let cachedDefaults = null;
let zoomScale = 1;
const MIN_ZOOM = 1;
const MAX_ZOOM = 3;
const ZOOM_STEP = 0.25;
let translateState = { x: 0, y: 0 };
let dragStart = { x: 0, y: 0 };
let dragOrigin = { x: 0, y: 0 };
let isDragging = false;
let dragPointerId = null;
const debounce = (fn, delay = 400) => {
  let timeout;
  return (...args) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => fn(...args), delay);
  };
};

const setLoading = (isLoading) => {
  document.body.dataset.loading = isLoading ? "true" : "false";
};

const autoDefaultFields = [
  "title",
  "y_min",
  "y_max",
  "x_min",
  "x_max",
  "text_xaxis",
  "text_yaxis",
  "file_name",
  "breakx",
  "breaky"
];

const getControl = (name) => document.querySelector(`[data-param="${name}"]`);

const applyZoomTransform = () => {
  if (!plotImage) return;
  plotImage.style.transform = `translate(${translateState.x}px, ${translateState.y}px) scale(${zoomScale})`;
  plotImage.style.cursor = zoomScale > 1 ? "grab" : "default";
};

const clampTranslation = () => {
  if (!plotImage || !plotContainer) return;
  const containerWidth = plotContainer.clientWidth;
  const containerHeight = plotContainer.clientHeight;
  const baseWidth = plotImage.offsetWidth;
  const baseHeight = plotImage.offsetHeight;
  if (!baseWidth || !baseHeight || !containerWidth || !containerHeight) {
    translateState = { x: 0, y: 0 };
    return;
  }
  const maxX = Math.max(0, (baseWidth * zoomScale - containerWidth) / 2);
  const maxY = Math.max(0, (baseHeight * zoomScale - containerHeight) / 2);
  translateState.x = Math.min(maxX, Math.max(-maxX, translateState.x));
  translateState.y = Math.min(maxY, Math.max(-maxY, translateState.y));
};

const updateZoomButtons = () => {
  if (!zoomInBtn || !zoomOutBtn) return;
  const atMin = zoomScale <= MIN_ZOOM + 0.001;
  const atMax = zoomScale >= MAX_ZOOM - 0.001;
  zoomOutBtn.disabled = atMin;
  zoomInBtn.disabled = atMax;
};

const resetZoom = () => {
  zoomScale = MIN_ZOOM;
  translateState = { x: 0, y: 0 };
  applyZoomTransform();
  updateZoomButtons();
  if (document.body) {
    document.body.dataset.dragging = "false";
  }
};

const setZoom = (targetScale) => {
  const nextScale = Math.min(MAX_ZOOM, Math.max(MIN_ZOOM, targetScale));
  zoomScale = nextScale;
  if (zoomScale === MIN_ZOOM) {
    translateState = { x: 0, y: 0 };
  } else {
    clampTranslation();
  }
  applyZoomTransform();
  updateZoomButtons();
};

const startDrag = (event) => {
  if (zoomScale <= MIN_ZOOM) return;
  event.preventDefault();
  isDragging = true;
  dragPointerId = event.pointerId;
  dragStart = { x: event.clientX, y: event.clientY };
  dragOrigin = { ...translateState };
  plotImage.setPointerCapture?.(dragPointerId);
  if (document.body) {
    document.body.dataset.dragging = "true";
  }
};

const handleDrag = (event) => {
  if (!isDragging || event.pointerId !== dragPointerId) return;
  const deltaX = event.clientX - dragStart.x;
  const deltaY = event.clientY - dragStart.y;
  translateState.x = dragOrigin.x + deltaX;
  translateState.y = dragOrigin.y + deltaY;
  clampTranslation();
  applyZoomTransform();
};

const endDrag = (event) => {
  if (!isDragging || event.pointerId !== dragPointerId) return;
  isDragging = false;
  plotImage.releasePointerCapture?.(dragPointerId);
  dragPointerId = null;
  if (document.body) {
    document.body.dataset.dragging = "false";
  }
  clampTranslation();
  applyZoomTransform();
};

const applyDefaults = (useMA, triggerPlot = false) => {
  if (!cachedDefaults) return;
  const target = useMA ? cachedDefaults?.ma : cachedDefaults?.volcano;
  if (!target) return;

  autoDefaultFields.forEach((field) => {
    if (!(field in target)) return;
    const el = getControl(field);
    if (!el) return;
    const value = target[field];
    if (el.type === "checkbox") {
      el.checked = Boolean(value);
      return;
    }
    el.value = value ?? "";
  });

  if (triggerPlot && currentFile) {
    debouncedPlotRequest();
  }
};

const updateFileStatus = (file) => {
  if (!fileStatus) return;
  if (!file) {
    fileStatus.textContent = "No file selected.";
    fileStatus.classList.remove("text-emerald-600", "dark:text-emerald-400", "font-medium");
    fileStatus.classList.add("text-gray-500", "dark:text-gray-400");
    return;
  }
  const maxLength = 40;
  const displayName = file.name.length > maxLength
    ? `${file.name.slice(0, maxLength - 3)}...`
    : file.name;
  fileStatus.textContent = `Selected: ${displayName}`;
  fileStatus.classList.remove("text-gray-500", "dark:text-gray-400");
  fileStatus.classList.add("text-emerald-600", "dark:text-emerald-400", "font-medium");
};

const setHasPlot = (hasPlot) => {
  document.body.dataset.hasPlot = hasPlot ? "true" : "false";
  if (emptyState) {
    const shouldShowEmpty = !currentFile;
    emptyState.style.display = shouldShowEmpty ? "flex" : "none";
  }
  if (zoomControls) {
    zoomControls.classList.toggle("hidden", !hasPlot);
  }
};

const clearPlot = () => {
  setHasPlot(false);
  if (activeUrl) {
    URL.revokeObjectURL(activeUrl);
    activeUrl = null;
  }
  plotImage.src = "";
  plotImage.classList.add("hidden");
  resetZoom();
};


const collectParams = () => {
  const payload = {};
  controls.forEach((el) => {
    const key = el.dataset.param;
    if (!key) return;
    if (el.type === "checkbox") {
      payload[key] = el.checked;
      return;
    }
    if (el.tagName.toLowerCase() === "select") {
      payload[key] = el.value;
      return;
    }
    payload[key] = el.value;
  });
  return payload;
};

const updateSavedList = async () => {
  try {
    const response = await fetch("/settings");
    const json = await response.json();
    savedSelect.innerHTML = `<option value="">Select preset</option>`;
    json.settings.forEach((row) => {
      const option = document.createElement("option");
      option.value = row.name;
      option.textContent = `${row.name} (${new Date(row.updated_at).toLocaleString()})`;
      savedSelect.appendChild(option);
    });
  } catch (err) {
    console.error("Failed to load presets", err);
  }
};

const applySettings = (settings) => {
  Object.entries(settings).forEach(([key, value]) => {
    const target = document.querySelector(`[data-param="${key}"]`);
    if (!target) return;
    if (target.type === "checkbox") {
      target.checked = Boolean(value);
      return;
    }
    target.value = value;
  });
  debouncedPlotRequest();
};

const showPlot = (blob) => {
  if (activeUrl) {
    URL.revokeObjectURL(activeUrl);
  }
  activeUrl = URL.createObjectURL(blob);
  plotImage.src = activeUrl;
  plotImage.classList.remove("hidden");
  plotContainer?.classList.add("no-border");
  resetZoom();
  setHasPlot(true);
};

const plotRequest = async () => {
  if (!currentFile) {
    setHasPlot(false);
    // show dashed border again if no file
    plotContainer.classList.remove("no-border");
    return;
  }

  setLoading(true);
  setHasPlot(false);

  try {
    const payload = collectParams();
    const formData = new FormData();
    formData.append("data_file", currentFile);
    formData.append("payload", JSON.stringify(payload));

    const response = await fetch("/plot", { method: "POST", body: formData });

    if (!response.ok) {
      const error = await response.json().catch(() => null);
      alert(error?.error || "Unable to generate plot.");
      // keep dashed border if plot failed
      plotContainer.classList.remove("no-border");
      return;
    }

    const blob = await response.blob();

    showPlot(blob);
  } catch (err) {
    console.error("Plot failed", err);
    alert("Unable to generate plot.");
    plotContainer.classList.remove("no-border");
  } finally {
    setLoading(false);
  }
};


const debouncedPlotRequest = debounce(plotRequest, 600);

const fetchDefaults = async () => {
  if (!currentFile) {
    cachedDefaults = null;
    return null;
  }
  const formData = new FormData();
  formData.append("data_file", currentFile);
  try {
    const response = await fetch("/defaults", { method: "POST", body: formData });
    if (!response.ok) {
      throw new Error("Unable to fetch defaults");
    }
    const json = await response.json();
    cachedDefaults = json?.defaults || null;
    return cachedDefaults;
  } catch (err) {
    console.error("Defaults request failed", err);
    cachedDefaults = null;
    return null;
  }
};

const handleExport = async () => {
  if (!currentFile) {
    alert("Upload a file before exporting.");
    return;
  }
  setLoading(true);
  try {
    const payload = collectParams();
    const formData = new FormData();
    formData.append("data_file", currentFile);
    formData.append("payload", JSON.stringify(payload));
    const response = await fetch("/export", { method: "POST", body: formData });
    if (!response.ok) {
      const error = await response.json().catch(() => null);
      alert(error?.error || "Unable to export plot.");
      return;
    }
    const blob = await response.blob();
    const downloadUrl = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = downloadUrl;
    anchor.download = `${payload.file_name || "volcano_plot"}.${payload.file_type || "png"}`;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
    URL.revokeObjectURL(downloadUrl);
  } catch (err) {
    console.error("Export failed", err);
    alert("Export request failed.");
  } finally {
    setLoading(false);
  }
};

fileInput.addEventListener("change", async (event) => {
  currentFile = event.target.files[0] || null;

  if (!currentFile) {
    setHasPlot(false);
    cachedDefaults = null;

    // Hide the image & revoke old blob if you do that:
    plotImage.classList.add("hidden");
    if (activeUrl) {
      URL.revokeObjectURL(activeUrl);
      activeUrl = null;
    }

    // Show dashed border again when no file
    plotContainer.classList.remove("no-border");

    updateFileStatus(null);

    return;
  }

  updateFileStatus(currentFile);

  // Remove dashed border immediately once a file is picked
  plotContainer.classList.add("no-border");

  await fetchDefaults();
  applyDefaults(Boolean(showMACheckbox?.checked));

  // As soon as defaults are applied, generate the plot
  await plotRequest();
});



controls.forEach((input) => {
  if (input.dataset.param === "showMA") {
    input.addEventListener("input", async () => {
      if (!currentFile) return;
      if (!cachedDefaults) {
        await fetchDefaults();
      }
      applyDefaults(Boolean(showMACheckbox?.checked));
      debouncedPlotRequest();
    });
    return;
  }
  input.addEventListener("input", () => {
    if (!currentFile) return;
    debouncedPlotRequest();
  });
});

zoomInBtn?.addEventListener("click", () => {
  if (plotImage.classList.contains("hidden")) return;
  setZoom(zoomScale + ZOOM_STEP);
});

zoomOutBtn?.addEventListener("click", () => {
  if (plotImage.classList.contains("hidden")) return;
  setZoom(zoomScale - ZOOM_STEP);
});

if (plotImage) {
  plotImage.addEventListener("pointerdown", startDrag);
  plotImage.addEventListener("pointermove", handleDrag);
  plotImage.addEventListener("pointerup", endDrag);
  plotImage.addEventListener("pointercancel", endDrag);
  plotImage.addEventListener("dragstart", (event) => event.preventDefault());
}

exportBtn.addEventListener("click", handleExport);

saveBtn.addEventListener("click", async () => {
  const name = settingsNameInput.value.trim();
  if (!name) {
    alert("Name the preset before saving.");
    return;
  }
  try {
    await fetch("/settings", {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({ name, settings: collectParams() })
    });
    await updateSavedList();
    alert("Settings saved.");
  } catch (err) {
    console.error("Save failed", err);
    alert("Unable to save settings.");
  }
});

loadBtn.addEventListener("click", async () => {
  const name = savedSelect.value;
  if (!name) {
    alert("Select a preset to load.");
    return;
  }
  try {
    const response = await fetch(`/settings/${encodeURIComponent(name)}`);
    if (!response.ok) {
      const err = await response.json().catch(() => null);
      alert(err?.error || "Preset not found.");
      return;
    }
    const json = await response.json();
    applySettings(json.settings);
  } catch (err) {
    console.error("Load failed", err);
    alert("Unable to load preset.");
  }
});

const hydrateFromSessionCsv = async () => {
  const stored = sessionStorage.getItem("deseqVolcanoCsv");
  if (!stored) return;
  sessionStorage.removeItem("deseqVolcanoCsv");
  try {
    const file = new File([stored], "deseq_volcano_ready.csv", { type: "text/csv" });
    currentFile = file;
    plotContainer.classList.add("no-border");
    updateFileStatus(currentFile);
    cachedDefaults = null;
    await fetchDefaults();
    applyDefaults(Boolean(showMACheckbox?.checked));
    await plotRequest();
    setHasPlot(true);
  } catch (err) {
    console.error("Failed to load DESeq CSV from sessionStorage", err);
  }
};

resetZoom();
updateSavedList();
setHasPlot(false);
setLoading(false);
updateFileStatus(null);
(async () => { await hydrateFromSessionCsv(); })();
