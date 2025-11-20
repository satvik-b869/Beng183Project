const dataFileInput = document.getElementById("expression-file");
const attributeFileInput = document.getElementById("attribute-file");
const metadataStatus = document.getElementById("metadata-status");
const dataStatus = document.getElementById("expression-status");
const attributeStatus = document.getElementById("attribute-status");
const geneSelect = document.getElementById("gene-select");
const attributeColumnSelect = document.getElementById("attribute-column");
const attributeValuesSelect = document.getElementById("attribute-values");
const groupColumnSelect = document.getElementById("group-column");
const plotTypeSelect = document.getElementById("plot-type");
const removeXCheckbox = document.getElementById("remove-x");
const includeErrorCheckbox = document.getElementById("include-error");
const colorSchemeSelect = document.getElementById("color-scheme");
const clusterRowsCheckbox = document.getElementById("cluster-rows");
const clusterColumnsCheckbox = document.getElementById("cluster-columns");
const textXAxisInput = document.getElementById("text-xaxis");
const textYAxisInput = document.getElementById("text-yaxis");
const yMinInput = document.getElementById("y-min");
const yMaxInput = document.getElementById("y-max");
const titleInput = document.getElementById("title-input");
const subtitleInput = document.getElementById("subtitle-input");
const legendTitleInput = document.getElementById("legend-title");
const legendPositionSelect = document.getElementById("legend-position");
const widthInput = document.getElementById("width-input");
const heightInput = document.getElementById("height-input");
const unitsInput = document.getElementById("units-input");
const dpiInput = document.getElementById("dpi-input");
const fileNameInput = document.getElementById("file-name");
const fileTypeSelect = document.getElementById("file-type");
const exportBtn = document.getElementById("export-btn");
const plotImage = document.getElementById("plot-image");
const emptyState = document.querySelector("[data-empty]");
const plotOptionVisibilityTargets = document.querySelectorAll("[data-plot-visible]");

let state = {
  dataFile: null,
  attributeFile: null,
  metadata: null,
  isPlotReady: false
};

const debounce = (fn, delay = 500) => {
  let timeout;
  return (...args) => {
    clearTimeout(timeout);
    timeout = setTimeout(() => fn(...args), delay);
  };
};

const setStatus = (text, variant = "info") => {
  metadataStatus.textContent = text;
  metadataStatus.classList.remove("text-red-500", "text-amber-500", "text-gray-500", "dark:text-gray-400");
  if (variant === "error") {
    metadataStatus.classList.add("text-red-500");
  } else if (variant === "warn") {
    metadataStatus.classList.add("text-amber-500");
  } else {
    metadataStatus.classList.add("text-gray-500", "dark:text-gray-400");
  }
};

const setLoading = (isLoading) => {
  document.body.dataset.loading = isLoading ? "true" : "false";
};

const populateSelect = (select, values = [], { multiple = false, selectAll = false } = {}) => {
  select.innerHTML = "";
  values.forEach((value) => {
    const opt = document.createElement("option");
    opt.value = value;
    opt.textContent = value;
    if (selectAll) opt.selected = true;
    select.appendChild(opt);
  });
  if (!multiple && values.length) {
    select.value = values[0];
  }
};

const getSelectedValues = (select) =>
  Array.from(select.selectedOptions).map((option) => option.value).filter(Boolean);

const updateAttributeValues = () => {
  const column = attributeColumnSelect.value;
  if (!column || !state.metadata) return;
  const values = state.metadata.attribute_values[column] || [];
  populateSelect(attributeValuesSelect, values, { multiple: true, selectAll: true });
};

const updateUploadStatus = (element, file) => {
  if (!element) return;
  element.classList.remove("text-emerald-600", "dark:text-emerald-400", "font-medium");
  element.classList.add("text-gray-500", "dark:text-gray-400");
  if (!file) {
    element.textContent = "No file selected.";
    return;
  }
  const maxLength = 40;
  const displayName = file.name.length > maxLength
    ? `${file.name.slice(0, maxLength - 3)}...`
    : file.name;
  element.textContent = `Selected: ${displayName}`;
  element.classList.remove("text-gray-500", "dark:text-gray-400");
  element.classList.add("text-emerald-600", "dark:text-emerald-400", "font-medium");
};

const updateGroupColumns = () => {
  groupColumnSelect.innerHTML = "";
  const placeholder = document.createElement("option");
  placeholder.value = "";
  placeholder.textContent = "None";
  groupColumnSelect.appendChild(placeholder);
  if (!state.metadata) return;
  state.metadata.attribute_columns.forEach((col) => {
    if (col === attributeColumnSelect.value) return;
    const opt = document.createElement("option");
    opt.value = col;
    opt.textContent = col;
    groupColumnSelect.appendChild(opt);
  });
};

const normalizePlotTypeValue = (value = "") => value.toLowerCase().replace(/\s+/g, "-");

const updatePlotOptionVisibility = () => {
  if (!plotOptionVisibilityTargets.length) return;
  const target = normalizePlotTypeValue(plotTypeSelect.value);
  plotOptionVisibilityTargets.forEach((element) => {
    const allowed = (element.dataset.plotVisible || "")
      .split(",")
      .map((value) => normalizePlotTypeValue(value.trim()))
      .filter(Boolean);
    const shouldShow = allowed.length === 0 || allowed.includes(target);
    element.classList.toggle("hidden", !shouldShow);
  });
};


const loadMetadata = async () => {
  if (!state.dataFile || !state.attributeFile) return;
  setStatus("Loading metadata…");
  setLoading(true);
  try {
    const formData = new FormData();
    formData.append("data_file", state.dataFile);
    formData.append("attribute_file", state.attributeFile);
    const response = await fetch("/expression/metadata", {
      method: "POST",
      body: formData
    });
    if (!response.ok) {
      throw new Error("Unable to load metadata");
    }
    const json = await response.json();
    state.metadata = json;
    autoSelectGenes();
    populateSelect(attributeColumnSelect, json.attribute_columns);
    updateAttributeValues();
    updateGroupColumns();
    setStatus("Metadata loaded. Choose genes and groups.");
    requestPlot();
  } catch (err) {
    console.error(err);
    setStatus("Failed to load metadata. Check your files.", "error");
  } finally {
    setLoading(false);
  }
};

// Auto-select and populate genes list from metadata
const autoSelectGenes = () => {
  if (!state.metadata || !Array.isArray(state.metadata.genes)) return;
  const genes = state.metadata.genes.slice();
  genes.sort();
  populateSelect(geneSelect, genes, { multiple: true });
  // select first gene by default if none selected
  if (!getSelectedValues(geneSelect).length && genes.length) {
    Array.from(geneSelect.options)[0].selected = true;
  }
};

const requireMetadata = () => {
  if (!state.metadata) {
    setStatus("Upload files to fetch metadata first.", "warn");
    return false;
  }
  return true;
};

const collectParams = () => {
  const selectedGenes = getSelectedValues(geneSelect);
  const selectedValues = getSelectedValues(attributeValuesSelect);
  // follow the same order and keys as `expression_defaults` in the Shiny server
  return {
    plot_type: plotTypeSelect.value,
    remove_x_axis_labels: removeXCheckbox.checked,
    include_error_bars: includeErrorCheckbox.checked,
    attribute_column: attributeColumnSelect.value || "",
    selected_values: selectedValues,
    group_by_column: groupColumnSelect.value,
    selected_gene: selectedGenes,
    text_xaxis: textXAxisInput.value,
    text_yaxis: textYAxisInput.value,
    title: titleInput.value,
    subtitle: subtitleInput.value,
    legend_title: legendTitleInput.value,
    legendPosition: legendPositionSelect.value,
    color_scheme: colorSchemeSelect.value,
    cluster_rows: clusterRowsCheckbox.checked,
    cluster_columns: clusterColumnsCheckbox.checked,
    y_min: yMinInput.value !== "" ? Number(yMinInput.value) : null,
    y_max: yMaxInput.value !== "" ? Number(yMaxInput.value) : null,
    width: Number(widthInput.value) || 11,
    height: Number(heightInput.value) || 8,
    units: unitsInput.value,
    dpi: Number(dpiInput.value) || 300,
    file_name: fileNameInput.value || "expression_plot",
    file_type: fileTypeSelect.value,
    // client does not have UI for these fields yet, but include ordering arrays
    value_order: [],
    value_order_barplot: []
  };
};

const validateParams = (params) => {
  if (!state.dataFile || !state.attributeFile) {
    setStatus("Upload both files before generating plots.", "warn");
    return false;
  }
  if (!params.attribute_column) {
    setStatus("Select an attribute column.", "warn");
    return false;
  }
  if (!params.selected_values || params.selected_values.length === 0) {
    setStatus("Select at least one attribute value.", "warn");
    return false;
  }
  if (!params.selected_gene || params.selected_gene.length === 0) {
    setStatus("Select at least one gene.", "warn");
    return false;
  }
  return true;
};

const requestPlot = async () => {
  const params = collectParams();
  if (!validateParams(params)) return;
  setLoading(true);
  emptyState.style.display = "none";
  plotImage.classList.add("hidden");
  try {
    const formData = new FormData();
    formData.append("data_file", state.dataFile);
    formData.append("attribute_file", state.attributeFile);
    formData.append("payload", JSON.stringify(params));
    const response = await fetch("/expression/plot", {
      method: "POST",
      body: formData
    });
    if (!response.ok) {
      const err = await response.json().catch(() => null);
      throw new Error(err?.error || "Unable to generate plot.");
    }
    const blob = await response.blob();
    const url = URL.createObjectURL(blob);
    plotImage.src = url;
    plotImage.onload = () => URL.revokeObjectURL(url);
    plotImage.classList.remove("hidden");
    state.isPlotReady = true;
    setStatus("Plot updated.");
  } catch (err) {
    console.error(err);
    plotImage.classList.add("hidden");
    emptyState.style.display = "flex";
    state.isPlotReady = false;
    setStatus(err.message, "error");
  } finally {
    setLoading(false);
  }
};

const debouncedPlot = debounce(() => {
  if (state.metadata) {
    requestPlot();
  }
}, 600);

const handleExport = async () => {
  const params = collectParams();
  if (!validateParams(params)) return;
  if (!state.isPlotReady) {
    setStatus("Generate the plot before exporting.", "warn");
    return;
  }
  exportBtn.disabled = true;
  try {
    const formData = new FormData();
    formData.append("data_file", state.dataFile);
    formData.append("attribute_file", state.attributeFile);
    formData.append("payload", JSON.stringify(params));
    const response = await fetch("/expression/export", {
      method: "POST",
      body: formData
    });
    if (!response.ok) {
      const err = await response.json().catch(() => null);
      throw new Error(err?.error || "Unable to export plot.");
    }
    const blob = await response.blob();
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = `${params.file_name}.${params.file_type}`;
    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
    URL.revokeObjectURL(url);
    setStatus("Exported plot.");
  } catch (err) {
    console.error(err);
    setStatus(err.message, "error");
  } finally {
    exportBtn.disabled = false;
  }
};

dataFileInput.addEventListener("change", (event) => {
  state.dataFile = event.target.files[0] || null;
  setStatus(state.dataFile ? "Expression matrix selected." : "Upload both files to begin.");
  updateUploadStatus(dataStatus, state.dataFile);
  if (state.attributeFile && state.dataFile) {
    loadMetadata();
  }
});

attributeFileInput.addEventListener("change", (event) => {
  state.attributeFile = event.target.files[0] || null;
  setStatus(state.attributeFile ? "Attributes file selected." : "Upload both files to begin.");
  updateUploadStatus(attributeStatus, state.attributeFile);
  if (state.attributeFile && state.dataFile) {
    loadMetadata();
  }
});

attributeColumnSelect.addEventListener("change", () => {
  updateAttributeValues();
  updateGroupColumns();
  debouncedPlot();
});

attributeValuesSelect.addEventListener("change", debouncedPlot);
geneSelect.addEventListener("change", debouncedPlot);
groupColumnSelect.addEventListener("change", debouncedPlot);
plotTypeSelect.addEventListener("change", () => {
  updatePlotOptionVisibility();
  debouncedPlot();
});
removeXCheckbox.addEventListener("change", debouncedPlot);
includeErrorCheckbox.addEventListener("change", debouncedPlot);
colorSchemeSelect.addEventListener("change", debouncedPlot);
clusterRowsCheckbox.addEventListener("change", debouncedPlot);
clusterColumnsCheckbox.addEventListener("change", debouncedPlot);
textXAxisInput.addEventListener("input", debouncedPlot);
textYAxisInput.addEventListener("input", debouncedPlot);
yMinInput.addEventListener("input", debouncedPlot);
yMaxInput.addEventListener("input", debouncedPlot);
titleInput.addEventListener("input", debouncedPlot);
subtitleInput.addEventListener("input", debouncedPlot);
legendTitleInput.addEventListener("input", debouncedPlot);
legendPositionSelect.addEventListener("change", debouncedPlot);
widthInput.addEventListener("input", debouncedPlot);
heightInput.addEventListener("input", debouncedPlot);
unitsInput.addEventListener("change", debouncedPlot);
dpiInput.addEventListener("input", debouncedPlot);

exportBtn.addEventListener("click", (e) => {
  e.preventDefault();
  handleExport();
});

updatePlotOptionVisibility();
