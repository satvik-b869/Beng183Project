const fileInput = document.getElementById("data-file");
const exportBtn = document.getElementById("export-btn");
const saveBtn = document.getElementById("save-settings-btn");
const loadBtn = document.getElementById("load-settings-btn");
const savedSelect = document.getElementById("saved-settings");
const settingsNameInput = document.getElementById("settings-name");
const loadingIndicator = document.getElementById("loading-indicator");
const plotImage = document.getElementById("plot-image");
const controls = document.querySelectorAll("[data-param]");
const emptyState = document.querySelector("[data-empty]");
const fileStatus = document.getElementById("file-status");
const plotContainer = document.getElementById("plot-container");


let currentFile = null;
let activeUrl = null;
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

const setHasPlot = (hasPlot) => {
  document.body.dataset.hasPlot = hasPlot ? "true" : "false";
  emptyState.style.display = hasPlot ? "none" : "flex";
};

const clearPlot = () => {
  setHasPlot(false);
  if (activeUrl) {
    URL.revokeObjectURL(activeUrl);
    activeUrl = null;
  }
  plotImage.src = "";
  plotImage.classList.add("hidden");
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

    if (activeUrl) {
      URL.revokeObjectURL(activeUrl);
    }
    activeUrl = URL.createObjectURL(blob);

    plotImage.src = activeUrl;
    plotImage.classList.remove("hidden");

    // 🔴 THIS is the important line:
    plotContainer.classList.add("no-border");

    setHasPlot(true);
  } catch (err) {
    console.error("Plot failed", err);
    alert("Unable to generate plot.");
    plotContainer.classList.remove("no-border");
  } finally {
    setLoading(false);
  }
};


const debouncedPlotRequest = debounce(plotRequest, 600);

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

fileInput.addEventListener("change", (event) => {
  currentFile = event.target.files[0];

  if (!currentFile) {
    setHasPlot(false);

    // Hide the image & revoke old blob if you do that:
    plotImage.classList.add("hidden");
    if (activeUrl) {
      URL.revokeObjectURL(activeUrl);
      activeUrl = null;
    }

    // Show dashed border again when no file
    plotContainer.classList.remove("no-border");

    // Optional: update any “file status” text if you added it
    // fileStatus.textContent = "No file selected.";

    return;
  }

  // Optional: display file name
  // fileStatus.textContent = `File uploaded: ${currentFile.name}`;

  // As soon as a file is selected, generate the plot
  plotRequest();
});



controls.forEach((input) => {
  input.addEventListener("input", () => {
    if (!currentFile) return;
    debouncedPlotRequest();
  });
});

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

updateSavedList();
setHasPlot(false);
setLoading(false);
