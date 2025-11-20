.libPaths(c(file.path("..", ".Rlibs"), .libPaths()))

library(plumber)
library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)
library(colorspace)
library(jsonlite)
library(DBI)
library(RSQLite)
library(magick)
library(gplots)
library(DESeq2)


source("gene_expression_module.R")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (!is.null(x) && length(x)) x else y
}

data_dir <- "data"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
settings_db_path <- file.path(data_dir, "settings.sqlite")

init_db <- function() {
  conn <- dbConnect(SQLite(), settings_db_path)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS settings (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      payload TEXT NOT NULL,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  dbDisconnect(conn)
}

save_settings <- function(name, payload) {
  conn <- dbConnect(SQLite(), settings_db_path)
  json_payload <- toJSON(payload, auto_unbox = TRUE)
  dbExecute(
    conn,
    "
      INSERT INTO settings (name, payload)
      VALUES (?, ?)
      ON CONFLICT(name) DO UPDATE SET
      payload = excluded.payload,
      updated_at = CURRENT_TIMESTAMP
    ",
    params = list(name, json_payload)
  )
  dbDisconnect(conn)
}

list_settings <- function() {
  conn <- dbConnect(SQLite(), settings_db_path)
  rows <- dbGetQuery(conn, "SELECT name, updated_at FROM settings ORDER BY updated_at DESC")
  dbDisconnect(conn)
  rows
}

get_setting <- function(name) {
  conn <- dbConnect(SQLite(), settings_db_path)
  row <- dbGetQuery(conn, "SELECT payload FROM settings WHERE name = ? LIMIT 1", params = list(name))
  dbDisconnect(conn)
  if (nrow(row) == 0) return(NULL)
  fromJSON(row$payload[1], simplifyVector = TRUE)
}

init_db()

default_params <- list(
  showMA = FALSE,
  advancedOptions = FALSE,
  fc_thresholdupreg = 1.5,
  fc_thresholddownreg = -1.5,
  p_val_threshold = 0.05,
  line_color = "#000000",
  up_color = "#75ba84",
  down_color = "#c09ed6",
  no_color = "#A8A8A8",
  y_min = 0,
  y_max = 50,
  x_min = -10,
  x_max = 10,
  breakx = 2,
  breaky = 10,
  text_xaxis = "Log2 Fold Change",
  text_yaxis = "-log10(p-val)",
  textlab_size_axis = 14,
  title = "",
  subtitle = "",
  textlab_size_title = 25,
  textlab_size_subtitle = 0,
  legend_title = "Legend",
  legendPosition = "right",
  textlab_size_legend = 14,
  textlab_size_legend_content = 14,
  custom_upreg_text = "Up",
  custom_downreg_text = "Down",
  custom_nochange_text = "No Change",
  textlab_color_axis = "#000000",
  point_size_UP = 1.5,
  point_size_DOWN = 1.5,
  point_size_NO = 1.0,
  alpha_UP = 1,
  alpha_DOWN = 1,
  alpha_NO = 0.25,
  label_gene_symbols = "",
  partial_match = "",
  top_genes = 0,
  up_color_selected = "#1B75BB",
  down_color_selected = "#E62319",
  no_color_selected = "#000000",
  point_size_selected = 3.0,
  text_size_pointlabs = 12,
  text_color_pointlabs = "#000000",
  alpha_UPselected = 1,
  alpha_DOWNselected = 1,
  alpha_NOselected = 1,
  file_name = "volcano_plot",
  colormodel = "RGB",
  file_type = "png",
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  zoom = 1
)

expression_defaults <- list(
  plot_type = "Boxplot",
  remove_x_axis_labels = FALSE,
  include_error_bars = FALSE,
  attribute_column = "",
  selected_values = list(),
  group_by_column = "",
  selected_gene = list(),
  text_xaxis = "",
  text_yaxis = "Log2 Normalized Counts",
  title = "",
  subtitle = "",
  legend_title = "Legend",
  legendPosition = "right",
  color_scheme = "RdYlBu",
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  y_min = NA,
  y_max = NA,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  file_name = "expression_plot",
  file_type = "png",
  colormodel = "RGB",
  zoom = 1,
  textlab_size_axis = 14,
  textlab_size_title = 23,
  textlab_size_subtitle = 18,
  textlab_size_legend = 14,
  textlab_size_legend_content = 14,
  textlab_color_axis = "#000000",
  value_order = list(),
  value_order_barplot = list()
)

safe_nzchar <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

ensure_numeric <- function(value, default) {
  if (is.null(value) || length(value) == 0) return(default)
  if (all(is.na(value))) return(default)
  if (is.character(value) && all(trimws(value) == "")) return(default)
  maybe_num <- suppressWarnings(as.numeric(value))
  if (length(maybe_num) == 0 || all(is.na(maybe_num))) default else maybe_num
}

ensure_boolean <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  if (is.character(value)) {
    val <- tolower(value)
    return(val %in% c("true", "1", "yes", "on"))
  }
  as.logical(value)
}

write_part_to_tempfile <- function(part, fallback_ext = NULL) {
  if (is.null(part) || is.null(part$value)) {
    stop("Upload part is missing file data.")
  }
  filename <- part$filename %||% ""
  ext <- tolower(tools::file_ext(filename))
  if (!nzchar(ext) && !is.null(fallback_ext)) {
    ext <- fallback_ext
  }
  fileext <- if (nzchar(ext)) paste0(".", ext) else ""
  tmp_path <- tempfile(fileext = fileext)
  writeBin(part$value, tmp_path)
  tmp_path
}

normalize_params <- function(raw) {
  params <- modifyList(default_params, raw)
  params$showMA <- ensure_boolean(params$showMA, FALSE)
  params$advancedOptions <- ensure_boolean(params$advancedOptions, FALSE)
  params$fc_thresholdupreg <- ensure_numeric(params$fc_thresholdupreg, default_params$fc_thresholdupreg)
  params$fc_thresholddownreg <- ensure_numeric(params$fc_thresholddownreg, default_params$fc_thresholddownreg)
  params$p_val_threshold <- ensure_numeric(params$p_val_threshold, default_params$p_val_threshold)
  params$y_min <- ensure_numeric(params$y_min, default_params$y_min)
  params$y_max <- ensure_numeric(params$y_max, default_params$y_max)
  params$x_min <- ensure_numeric(params$x_min, default_params$x_min)
  params$x_max <- ensure_numeric(params$x_max, default_params$x_max)
  params$breakx <- max(ensure_numeric(params$breakx, default_params$breakx), 1)
  params$breaky <- max(ensure_numeric(params$breaky, default_params$breaky), 1)
  params$textlab_size_axis <- ensure_numeric(params$textlab_size_axis, default_params$textlab_size_axis)
  params$textlab_size_title <- ensure_numeric(params$textlab_size_title, default_params$textlab_size_title)
  params$textlab_size_subtitle <- ensure_numeric(params$textlab_size_subtitle, default_params$textlab_size_subtitle)
  params$textlab_size_legend <- ensure_numeric(params$textlab_size_legend, default_params$textlab_size_legend)
  params$textlab_size_legend_content <- ensure_numeric(params$textlab_size_legend_content, default_params$textlab_size_legend_content)
  params$point_size_UP <- ensure_numeric(params$point_size_UP, default_params$point_size_UP)
  params$point_size_DOWN <- ensure_numeric(params$point_size_DOWN, default_params$point_size_DOWN)
  params$point_size_NO <- ensure_numeric(params$point_size_NO, default_params$point_size_NO)
  params$alpha_UP <- ensure_numeric(params$alpha_UP, default_params$alpha_UP)
  params$alpha_DOWN <- ensure_numeric(params$alpha_DOWN, default_params$alpha_DOWN)
  params$alpha_NO <- ensure_numeric(params$alpha_NO, default_params$alpha_NO)
  params$top_genes <- max(0, round(ensure_numeric(params$top_genes, default_params$top_genes)))
  params$point_size_selected <- ensure_numeric(params$point_size_selected, default_params$point_size_selected)
  params$text_size_pointlabs <- ensure_numeric(params$text_size_pointlabs, default_params$text_size_pointlabs)
  params$alpha_UPselected <- ensure_numeric(params$alpha_UPselected, default_params$alpha_UPselected)
  params$alpha_DOWNselected <- ensure_numeric(params$alpha_DOWNselected, default_params$alpha_DOWNselected)
  params$alpha_NOselected <- ensure_numeric(params$alpha_NOselected, default_params$alpha_NOselected)
  params$width <- ensure_numeric(params$width, default_params$width)
  params$height <- ensure_numeric(params$height, default_params$height)
  params$dpi <- ensure_numeric(params$dpi, default_params$dpi)
  params$zoom <- ensure_numeric(params$zoom, default_params$zoom)
  params
}

normalize_expression_params <- function(raw) {
  params <- modifyList(expression_defaults, raw)
  params$plot_type <- params$plot_type %||% "Boxplot"
  params$remove_x_axis_labels <- ensure_boolean(params$remove_x_axis_labels, FALSE)
  params$include_error_bars <- ensure_boolean(params$include_error_bars, FALSE)
  params$cluster_rows <- ensure_boolean(params$cluster_rows, TRUE)
  params$cluster_columns <- ensure_boolean(params$cluster_columns, TRUE)
  params$text_xaxis <- params$text_xaxis %||% ""
  params$text_yaxis <- params$text_yaxis %||% "Log2 Normalized Counts"
  params$title <- params$title %||% ""
  params$subtitle <- params$subtitle %||% ""
  params$legend_title <- params$legend_title %||% "Legend"
  params$legendPosition <- params$legendPosition %||% "right"
  params$color_scheme <- params$color_scheme %||% "RdYlBu"
  params$group_by_column <- params$group_by_column %||% ""
  params$attribute_column <- params$attribute_column %||% ""
  params$selected_values <- params$selected_values %||% list()
  params$selected_gene <- params$selected_gene %||% list()
  params$value_order <- params$value_order %||% list()
  params$value_order_barplot <- params$value_order_barplot %||% list()
  params$selected_gene <- as.character(unlist(params$selected_gene))
  params$selected_values <- as.character(unlist(params$selected_values))
  params$value_order <- as.character(unlist(params$value_order))
  params$value_order_barplot <- as.character(unlist(params$value_order_barplot))
  if (!length(params$value_order)) params$value_order <- NULL
  if (!length(params$value_order_barplot)) params$value_order_barplot <- NULL
  params$width <- ensure_numeric(params$width, expression_defaults$width)
  params$height <- ensure_numeric(params$height, expression_defaults$height)
  params$dpi <- ensure_numeric(params$dpi, expression_defaults$dpi)
  params$y_min <- ensure_numeric(params$y_min, params$y_min)
  params$y_max <- ensure_numeric(params$y_max, params$y_max)
  params
}

convert_to_inches <- function(value, units) {
  if (units == "in") {
    value
  } else if (units == "cm") {
    value / 2.54
  } else if (units == "mm") {
    value / 25.4
  } else if (units == "px") {
    value / default_params$dpi
  } else {
    value
  }
}

read_uploaded_data <- function(datapath) {
  ext <- tolower(tools::file_ext(datapath))
  df <- switch(
    ext,
    "txt" = readr::read_delim(datapath, show_col_types = FALSE),
    "csv" = readr::read_csv(datapath, show_col_types = FALSE),
    stop("Unsupported file format. Please upload a .csv or .txt file.")
  )
  
  df <- janitor::clean_names(df)
  
  # Map your actual column names → the names the rest of the code expects
  if ("basemean" %in% names(df) && !"base_mean" %in% names(df)) {
    df <- dplyr::rename(df, base_mean = basemean)
  }
  if ("log2foldchange" %in% names(df) && !"log2fold_change" %in% names(df)) {
    df <- dplyr::rename(df, log2fold_change = log2foldchange)
  }
  if ("geneid" %in% names(df) && !"gene_id" %in% names(df)) {
    df <- dplyr::rename(df, gene_id = geneid)
  }
  
  expected <- c("base_mean", "log2fold_change", "padj", "symbol")
  if (!all(expected %in% colnames(df))) {
    stop(paste(
      "Uploaded file is missing required columns after cleaning/renaming.",
      "Required:", paste(expected, collapse = ", "),
      "Have:", paste(colnames(df), collapse = ", ")
    ))
  }
  if (!"gene_id" %in% colnames(df)) {
    df$gene_id <- NA_character_
  }
  
  df
}


prepare_dataframe <- function(df, params) {
  min_padj <- suppressWarnings(min(df$padj[df$padj > 0], na.rm = TRUE))
  if (!is.finite(min_padj) || is.na(min_padj)) {
    min_padj <- 1e-6
  }

  df <- df %>%
    mutate(padj = ifelse(padj == 0, (min_padj / 100), padj)) %>%
    select(base_mean, log2fold_change, padj, symbol, gene_id)

  df <- df %>%
    mutate(colordiff = params$custom_nochange_text) %>%
    mutate(colordiff = case_when(
      (log2fold_change >= log2(params$fc_thresholdupreg)) & (padj <= params$p_val_threshold) ~ params$custom_upreg_text,
      (log2fold_change <= -log2(abs(params$fc_thresholddownreg))) & (padj <= params$p_val_threshold) ~ params$custom_downreg_text,
      TRUE ~ params$custom_nochange_text
    ))

  label_symbols <- if (safe_nzchar(params$label_gene_symbols)) {
    unlist(strsplit(params$label_gene_symbols, ",\\s*"))
  } else {
    character(0)
  }

  df <- df %>%
    mutate(selected = ifelse(symbol %in% label_symbols, "SELECTED", "NOT_SELECTED"))

  if (safe_nzchar(params$partial_match)) {
    partials <- unlist(strsplit(params$partial_match, ",\\s*"))
    pattern <- paste0(partials, collapse = "|")
    df <- df %>%
      mutate(selected = ifelse(str_detect(symbol, pattern) | symbol %in% label_symbols, "SELECTED", selected))
  }

  if (params$top_genes > 0) {
    top <- df %>% slice_max(order_by = abs(log2fold_change), n = params$top_genes)
    df <- df %>% mutate(selected = ifelse(symbol %in% top$symbol, "SELECTED", selected))
  }

  df <- df %>%
    mutate(
      colordiff_selected = ifelse(selected == "SELECTED",
        ifelse(colordiff == params$custom_upreg_text, paste0(params$custom_upreg_text, "_SELECTED"),
               ifelse(colordiff == params$custom_downreg_text, paste0(params$custom_downreg_text, "_SELECTED"),
                      paste0(params$custom_nochange_text, "_SELECTED"))),
        colordiff
      ),
      colordiff_nonselected = ifelse(selected == "NOT_SELECTED", colordiff, colordiff)
    ) %>%
    arrange(colordiff_nonselected)

  df
}

calculate_plot_defaults <- function(df, fallback = default_params) {
  if (nrow(df) == 0) {
    return(list(
      volcano = fallback[c("title", "y_min", "y_max", "x_min", "x_max",
                           "text_xaxis", "text_yaxis", "file_name", "breakx", "breaky")],
      ma = fallback[c("title", "y_min", "y_max", "x_min", "x_max",
                      "text_xaxis", "text_yaxis", "file_name", "breakx", "breaky")]
    ))
  }

  min_log2fc <- suppressWarnings(min(df$log2fold_change, na.rm = TRUE))
  max_log2fc <- suppressWarnings(max(df$log2fold_change, na.rm = TRUE))
  if (!is.finite(min_log2fc)) min_log2fc <- fallback$x_min
  if (!is.finite(max_log2fc)) max_log2fc <- fallback$x_max

  positive_padj <- df$padj[df$padj > 0]
  min_padj <- suppressWarnings(min(positive_padj, na.rm = TRUE))
  if (!is.finite(min_padj) || min_padj <= 0) min_padj <- fallback$p_val_threshold
  volcano_y_max <- ceiling(-log10(min_padj) / 5) * 5
  if (!is.finite(volcano_y_max) || volcano_y_max <= 0) volcano_y_max <- 8

  base_vals <- df$base_mean
  mean_base <- suppressWarnings(mean(base_vals, na.rm = TRUE))
  sd_base <- suppressWarnings(sd(base_vals, na.rm = TRUE))
  max_base <- suppressWarnings(max(base_vals, na.rm = TRUE))
  min_base <- suppressWarnings(min(base_vals, na.rm = TRUE))
  if (!is.finite(mean_base)) mean_base <- fallback$x_max
  if (!is.finite(sd_base)) sd_base <- 0
  if (!is.finite(max_base)) max_base <- mean_base
  if (!is.finite(min_base)) min_base <- mean_base

  new_max_x <- min(mean_base + 3 * sd_base, max_base)
  new_min_x <- max(mean_base - 3 * sd_base, min_base)
  if (!is.finite(new_max_x) || !is.finite(new_min_x) || identical(new_min_x, new_max_x)) {
    new_max_x <- max_base
    new_min_x <- min_base
  }
  if (!is.finite(new_max_x)) new_max_x <- fallback$x_max
  if (!is.finite(new_min_x)) new_min_x <- fallback$x_min

  calc_break <- function(max_val, min_val) {
    range_val <- ceiling(max_val) - floor(min_val)
    br <- floor(range_val / 5)
    if (!is.finite(br) || br < 1) 1 else br
  }

  calc_break_from_value <- function(value) {
    br <- floor(value / 5)
    if (!is.finite(br) || br < 1) 1 else br
  }

  volcano_defaults <- list(
    title = "Volcano Plot",
    y_min = 0,
    y_max = volcano_y_max,
    x_min = floor(min_log2fc),
    x_max = ceiling(max_log2fc),
    text_xaxis = "Log2 Fold Change",
    text_yaxis = "-log10(p-val)",
    file_name = "volcano_plot",
    breakx = calc_break(max_log2fc, min_log2fc),
    breaky = calc_break_from_value(volcano_y_max)
  )

  ma_defaults <- list(
    title = "MA Plot",
    y_min = floor(min_log2fc),
    y_max = ceiling(max_log2fc),
    x_min = floor(new_min_x),
    x_max = ceiling(new_max_x),
    text_xaxis = "Log2 Mean Expression",
    text_yaxis = "Log2 Fold Change",
    file_name = "MA_plot",
    breakx = calc_break(new_max_x, new_min_x),
    breaky = calc_break(max_log2fc, min_log2fc)
  )

  list(volcano = volcano_defaults, ma = ma_defaults)
}

build_plot <- function(df, params) {
  cols_selected <- c(
    setNames(params$up_color_selected, paste0(params$custom_upreg_text, "_SELECTED")),
    setNames(params$down_color_selected, paste0(params$custom_downreg_text, "_SELECTED")),
    setNames(params$no_color_selected, paste0(params$custom_nochange_text, "_SELECTED"))
  )

  sizes_selected <- c(
    setNames(params$point_size_selected, paste0(params$custom_upreg_text, "_SELECTED")),
    setNames(params$point_size_selected, paste0(params$custom_downreg_text, "_SELECTED")),
    setNames(params$point_size_selected, paste0(params$custom_nochange_text, "_SELECTED"))
  )

  alphas_selected <- c(
    setNames(params$alpha_UPselected, paste0(params$custom_upreg_text, "_SELECTED")),
    setNames(params$alpha_DOWNselected, paste0(params$custom_downreg_text, "_SELECTED")),
    setNames(params$alpha_NOselected, paste0(params$custom_nochange_text, "_SELECTED"))
  )

  cols_nonselected <- c(
    setNames(params$up_color, params$custom_upreg_text),
    setNames(params$down_color, params$custom_downreg_text),
    setNames(params$no_color, params$custom_nochange_text)
  )

  sizes_nonselected <- c(
    setNames(params$point_size_UP, params$custom_upreg_text),
    setNames(params$point_size_DOWN, params$custom_downreg_text),
    setNames(params$point_size_NO, params$custom_nochange_text)
  )

  alphas_nonselected <- c(
    setNames(params$alpha_UP, params$custom_upreg_text),
    setNames(params$alpha_DOWN, params$custom_downreg_text),
    setNames(params$alpha_NO, params$custom_nochange_text)
  )

  legend_pos <- params$legendPosition

  base_plot <- if (params$showMA) {
    ggplot(df, aes(x = base_mean, y = log2fold_change)) +
      geom_point(data = subset(df, selected == "NOT_SELECTED"),
                 aes(color = colordiff_nonselected, size = colordiff_nonselected, alpha = colordiff_nonselected)) +
      geom_point(data = subset(df, selected == "SELECTED"),
                 aes(fill = colordiff_selected, size = colordiff_selected, alpha = colordiff_selected),
                 color = "black", shape = 21, show.legend = FALSE) +
      geom_hline(yintercept = c(-log2(abs(params$fc_thresholddownreg)), log2(params$fc_thresholdupreg)),
                 linetype = "dashed", color = params$line_color) +
      scale_x_continuous(breaks = seq(params$x_min, params$x_max, params$breakx), limits = c(params$x_min, params$x_max)) +
      scale_y_continuous(breaks = seq(params$y_min, params$y_max, params$breaky), limits = c(params$y_min, params$y_max))
  } else {
    ggplot(df, aes(x = log2fold_change, y = -log10(padj))) +
      geom_point(data = subset(df, selected == "NOT_SELECTED"),
                 aes(color = colordiff_nonselected, size = colordiff_nonselected, alpha = colordiff_nonselected)) +
      geom_point(data = subset(df, selected == "SELECTED"),
                 aes(fill = colordiff_selected, size = colordiff_selected, alpha = colordiff_selected),
                 color = "black", shape = 21, show.legend = FALSE) +
      geom_vline(xintercept = c(-log2(abs(params$fc_thresholddownreg)), log2(params$fc_thresholdupreg)),
                 linetype = "dashed", color = params$line_color) +
      geom_hline(yintercept = -log10(params$p_val_threshold), linetype = "dashed", color = params$line_color) +
      scale_x_continuous(breaks = seq(params$x_min, params$x_max, params$breakx), limits = c(params$x_min, params$x_max)) +
      scale_y_continuous(breaks = seq(params$y_min, params$y_max, params$breaky), limits = c(params$y_min, params$y_max))
  }

  p <- base_plot +
    scale_color_manual(values = cols_nonselected, name = params$legend_title) +
    scale_fill_manual(values = cols_selected, name = "Selected Genes") +
    scale_alpha_manual(values = c(alphas_nonselected, alphas_selected)) +
    scale_size_manual(values = c(sizes_nonselected, sizes_selected)) +
    theme_bw() +
    theme(
      text = element_text(color = params$textlab_color_axis),
      legend.title = element_text(size = params$textlab_size_legend),
      legend.text = element_text(size = params$textlab_size_legend_content),
      plot.title = element_text(size = params$textlab_size_title),
      plot.subtitle = element_text(size = params$textlab_size_subtitle),
      axis.title = element_text(size = params$textlab_size_axis),
      axis.text = element_text(size = params$textlab_size_axis),
      legend.position = legend_pos
    ) +
    labs(x = params$text_xaxis, y = params$text_yaxis, title = params$title, subtitle = params$subtitle) +
    guides(
      color = guide_legend(override.aes = list(alpha = alphas_nonselected[1], size = sizes_nonselected[1])),
      fill = "none",
      alpha = "none",
      size = "none"
    )

  if (safe_nzchar(params$label_gene_symbols)) {
    selected_genes <- unlist(strsplit(params$label_gene_symbols, ",\\s*"))
    p <- p + geom_label_repel(data = df %>% filter(symbol %in% selected_genes),
                              aes(label = symbol, color = colordiff),
                              size = (params$text_size_pointlabs / .pt),
                              color = params$text_color_pointlabs,
                              max.overlaps = Inf, nudge_x = 0.5)
  }

  if (safe_nzchar(params$partial_match)) {
    partial_patterns <- unlist(strsplit(params$partial_match, ",\\s*"))
    combined_pattern <- paste0(partial_patterns, collapse = "|")
    partial_genes <- df %>% filter(str_detect(symbol, combined_pattern))
    p <- p + geom_label_repel(data = partial_genes,
                              aes(label = symbol, color = colordiff),
                              size = (params$text_size_pointlabs / .pt),
                              color = params$text_color_pointlabs,
                              max.overlaps = Inf, nudge_x = 0.5)
  }

  if (params$top_genes > 0) {
    top <- df %>% slice_max(order_by = abs(log2fold_change), n = params$top_genes)
    p <- p + geom_label_repel(data = top,
                              aes(label = symbol, color = colordiff),
                              size = (params$text_size_pointlabs / .pt),
                              color = params$text_color_pointlabs,
                              max.overlaps = Inf, nudge_x = 0.5)
  }

  p
}

save_plot_image <- function(plot_obj, params, file_path, file_type = "png") {
  units <- params$units
  width_value <- params$width
  height_value <- params$height
  zoomed_width <- width_value * params$zoom
  zoomed_height <- height_value * params$zoom

  if (file_type == "png") {
    ggsave(file_path, plot = plot_obj, width = zoomed_width, height = zoomed_height, units = units, dpi = params$dpi, device = "png")
    return(invisible(TRUE))
  }

  if (params$colormodel == "CMYK" && file_type %in% c("pdf", "eps", "tiff")) {
    if (file_type %in% c("pdf", "eps")) {
      ggsave(file_path, plot = plot_obj, width = width_value, height = height_value, units = units, device = file_type, colormodel = "cmyk")
      return(invisible(TRUE))
    }
    tmp_png <- tempfile(fileext = ".png")
    ggsave(tmp_png, plot = plot_obj, width = width_value, height = height_value, units = units, dpi = params$dpi)
    img <- image_read(tmp_png)
    img <- image_convert(img, colorspace = "cmyk")
    image_write(img, path = file_path, format = "tiff")
    unlink(tmp_png)
    return(invisible(TRUE))
  }

  ggsave(file_path, plot = plot_obj, width = width_value, height = height_value, units = units, dpi = params$dpi, device = file_type)
}

build_expression_plot_bundle <- function(expr_df, attr_df, params) {
  long_data <- prepare_expression_long(
    expr_df,
    attr_df,
    params$selected_gene,
    params$attribute_column,
    params$selected_values,
    params$group_by_column
  )
  colors <- expression_group_colors(long_data)
  axis_limits <- expression_axis_limits(long_data)
  if (is.null(params$y_min) || is.na(params$y_min)) params$y_min <- axis_limits$y_min
  if (is.null(params$y_max) || is.na(params$y_max)) params$y_max <- axis_limits$y_max

  if (identical(params$plot_type, "Heatmap")) {
    components <- prepare_heatmap_components(
      long_data,
      color_scheme = params$color_scheme,
      cluster_rows = params$cluster_rows,
      cluster_columns = params$cluster_columns
    )
    return(list(kind = "heatmap", components = components, colors = colors, params = params))
  }

  long_data <- long_data %>%
    group_by(Gene_Attribute) %>%
    mutate(
      Q1 = as.numeric(stats::quantile(NormalizedCounts, 0.25, na.rm = TRUE)),
      Q3 = as.numeric(stats::quantile(NormalizedCounts, 0.75, na.rm = TRUE)),
      Min = min(NormalizedCounts, na.rm = TRUE),
      Max = max(NormalizedCounts, na.rm = TRUE),
      SD = sd(NormalizedCounts, na.rm = TRUE)
    ) %>%
    ungroup()

  text_color <- params$textlab_color_axis %||% "#000000"
  y_limits <- c(params$y_min, params$y_max)

  plot_obj <- if (identical(params$plot_type, "Bar Graph")) {
    build_bar_plot(
      long_data,
      colors = colors,
      remove_x_labels = params$remove_x_axis_labels,
      text_xaxis = params$text_xaxis,
      text_yaxis = params$text_yaxis,
      title = params$title,
      subtitle = params$subtitle,
      legend_title = params$legend_title,
      legend_position = params$legend_position %||% "",
      axis_size = params$textlab_size_axis,
      title_size = params$textlab_size_title,
      subtitle_size = params$textlab_size_subtitle,
      legend_title_size = params$textlab_size_legend,
      legend_text_size = params$textlab_size_legend_content,
      text_color = text_color,
      y_limits = y_limits,
      sample_order = params$value_order_barplot,
      facet_column = params$group_by_column,
      include_error_bars = params$include_error_bars
    )
  } else {
    build_boxplot_plot(
      long_data,
      colors = colors,
      remove_x_labels = params$remove_x_axis_labels,
      text_xaxis = params$text_xaxis,
      text_yaxis = params$text_yaxis,
      title = params$title,
      subtitle = params$subtitle,
      legend_title = params$legend_title,
      legend_position = params$legend_position %||% "",
      axis_size = params$textlab_size_axis,
      title_size = params$textlab_size_title,
      subtitle_size = params$textlab_size_subtitle,
      legend_title_size = params$textlab_size_legend,
      legend_text_size = params$textlab_size_legend_content,
      text_color = text_color,
      y_limits = y_limits,
      value_order = params$value_order,
      facet_column = params$group_by_column
    )
  }

  list(kind = "ggplot", plot = plot_obj, params = params)
}

write_expression_plot_file <- function(bundle, params, file_path, file_type = "png") {
  if (bundle$kind == "heatmap") {
    device_call <- switch(
      file_type,
      "pdf" = function(...) pdf(...),
      "svg" = function(...) svg(...),
      "eps" = function(...) postscript(..., onefile = FALSE, paper = "special"),
      "tiff" = function(...) tiff(...),
      "jpeg" = function(...) jpeg(...),
      function(...) png(...)
    )
    device_call(file_path, width = params$width, height = params$height, units = params$units, res = params$dpi)
    annotation <- bundle$components$annotation
    color_lookup <- bundle$colors
    col_side_colors <- color_lookup[annotation$Broad_Group]
    col_side_colors[is.na(col_side_colors)] <- "#D1D5DB"
    axis_label_size <- params$textlab_size_axis %||% 14
    legend_title_size <- params$textlab_size_legend %||% 14
    legend_text_size <- params$textlab_size_legend_content %||% 14
    cex_factor <- axis_label_size / 14
    heatmap.2(
      bundle$components$matrix,
      Colv = if (params$cluster_columns) TRUE else FALSE,
      Rowv = if (params$cluster_rows) TRUE else FALSE,
      col = bundle$components$palette,
      ColSideColors = col_side_colors,
      labRow = rownames(bundle$components$matrix),
      labCol = colnames(bundle$components$matrix),
      scale = "none",
      margins = c(10, 13),
      cexCol = cex_factor,
      cexRow = cex_factor,
      trace = "none",
      xlab = params$text_xaxis %||% "Samples",
      ylab = params$text_yaxis %||% "Genes",
      main = NULL
    )
    legend_position <- switch(params$legendPosition %||% "right",
      "left" = "topleft",
      "top" = "topright",
      "bottom" = "bottomleft",
      "none" = "none",
      "topright"
    )
    if (!identical(legend_position, "none") && length(color_lookup)) {
      legend(
        x = legend_position,
        legend = names(color_lookup),
        col = unname(color_lookup),
        pch = 15,
        pt.cex = 1.5,
        cex = legend_text_size / 14,
        title = params$legend_title,
        title.cex = legend_title_size / 14
      )
    }
    dev.off()
    return(invisible(TRUE))
  }
  save_plot_image(bundle$plot, params, file_path, file_type)
}


#* @parser multi
#* @post /defaults
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    parts <- req$body %||% list()

    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }

    file_part <- get_part("data_file")
    if (is.null(file_part)) {
      res$status <- 400
      return(list(error = "Data file (data_file) is required."))
    }

    tmp_file <- write_part_to_tempfile(file_part, "csv")
    on.exit(unlink(tmp_file), add = TRUE)

    df <- read_uploaded_data(tmp_file)
    list(defaults = calculate_plot_defaults(df))
  }, error = function(e) {
    message("Defaults error: ", conditionMessage(e))
    res$status <- 400
    list(error = conditionMessage(e))
  })
}


#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  forward()
}


#* @parser multi
#* @post /plot
#* @serializer contentType list(type = "image/png")
function(req, res) {
  tryCatch({
    # req$body is a list of multipart parts
    parts <- req$body %||% list()
    
    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }
    
    file_part <- get_part("data_file")
    payload_part <- get_part("payload")
    
    if (is.null(file_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "Data file (data_file) is required."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    
    if (is.null(payload_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "Missing JSON payload (payload)."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    
    tmp_file <- write_part_to_tempfile(file_part, "csv")
    
    # payload_part$value is raw JSON
    payload_json <- rawToChar(payload_part$value)
    params <- normalize_params(jsonlite::fromJSON(payload_json, simplifyVector = TRUE))
    
    df <- read_uploaded_data(tmp_file) %>% prepare_dataframe(params)
    plot_obj <- build_plot(df, params)
    
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(c(tmp_file, tmp_png)), add = TRUE)
    
    save_plot_image(plot_obj, params, tmp_png, "png")
    readBin(tmp_png, "raw", n = file.info(tmp_png)$size)
  }, error = function(e) {
    message("Plot error: ", conditionMessage(e))
    res$status <- 400
    res$setHeader("Content-Type", "application/json")
    err_json <- jsonlite::toJSON(
      list(error = conditionMessage(e)),
      auto_unbox = TRUE
    )
    charToRaw(err_json)
  })
}



#* @parser multi
#* @post /export
#* @serializer contentType list(type = "application/octet-stream")
function(req, res) {
  tryCatch({
    parts <- req$body %||% list()
    
    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }
    
    file_part <- get_part("data_file")
    payload_part <- get_part("payload")
    
    if (is.null(file_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "Data file (data_file) is required."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    
    if (is.null(payload_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "Missing JSON payload (payload)."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    
    tmp_file <- write_part_to_tempfile(file_part, "csv")
    
    payload_json <- rawToChar(payload_part$value)
    params <- normalize_params(jsonlite::fromJSON(payload_json, simplifyVector = TRUE))
    
    df <- read_uploaded_data(tmp_file) %>% prepare_dataframe(params)
    plot_obj <- build_plot(df, params)
    
    tmp_out <- tempfile(fileext = paste0(".", params$file_type))
    on.exit(unlink(c(tmp_file, tmp_out)), add = TRUE)
    
    save_plot_image(plot_obj, params, tmp_out, params$file_type)
    readBin(tmp_out, "raw", n = file.info(tmp_out)$size)
  }, error = function(e) {
    message("Export error: ", conditionMessage(e))
    res$status <- 400
    res$setHeader("Content-Type", "application/json")
    err_json <- jsonlite::toJSON(
      list(error = conditionMessage(e)),
      auto_unbox = TRUE
    )
    charToRaw(err_json)
  })
}


#* @parser json
#* @post /settings
function(req, res) {
  body <- req$body
  name <- body$name
  settings <- body$settings
  if (is.null(name) || !safe_nzchar(name)) {
    res$status <- 400
    return(list(error = "A name is required to save settings."))
  }
  save_settings(name, settings)
  list(result = "ok")
}

#* @get /settings
#* @serializer unboxedJSON
function() {
  list(settings = list_settings())
}

#* @get /settings/<name>
#* @serializer unboxedJSON
function(name, res) {
  settings <- get_setting(name)
  if (is.null(settings)) {
    res$status <- 404
    return(list(error = "Settings not found."))
  }
  list(name = name, settings = settings)
}

# serve_static <- function(path, type) {
#   function(req, res) {
#     file_path <- file.path("www", path)
#     if (!file.exists(file_path)) {
#       res$status <- 404
#       return("Not found")
#     }
#     res$setHeader("Content-Type", type)
#     readChar(file_path, file.info(file_path)$size)
#   }
# }
# 
# pr$handle("GET", "/", function(req, res) {
#   res$setHeader("Content-Type", "text/html")
#   readChar(file.path("www", "index.html"), file.info(file.path("www", "index.html"))$size)
# })
# pr$handle("GET", "/styles.css", serve_static("styles.css", "text/css"))
# pr$handle("GET", "/app.js", serve_static("app.js", "text/javascript"))
# 
# pr$run(host = "0.0.0.0", port = 8000)

# helper to read file contents safely
read_file <- function(path) {
  if (!file.exists(path)) {
    return("Not found")
  }
  readChar(path, file.info(path)$size)
}


#* @parser multi
#* @post /expression/metadata
#* @serializer unboxedJSON
function(req, res) {
  tryCatch({
    parts <- req$body %||% list()
    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }
    data_part <- get_part("data_file")
    attr_part <- get_part("attribute_file")
    if (is.null(data_part) || is.null(attr_part)) {
      res$status <- 400
      return(list(error = "Both data_file and attribute_file are required."))
    }
    tmp_expr <- write_part_to_tempfile(data_part, "csv")
    tmp_attr <- write_part_to_tempfile(attr_part, "csv")
    on.exit(unlink(c(tmp_expr, tmp_attr)), add = TRUE)

    expr_df <- read_expression_matrix(tmp_expr)
    attr_df <- read_attribute_table(tmp_attr)

    attr_cols <- names(attr_df)[sapply(attr_df, function(col) length(unique(col)) >= 2)]
    attr_cols <- setdiff(attr_cols, "sample_name")
    attr_vals <- lapply(attr_cols, function(col) sort(unique(attr_df[[col]])))
    names(attr_vals) <- attr_cols

    list(
      genes = sort(unique(expr_df$Symbol)),
      attribute_columns = attr_cols,
      attribute_values = attr_vals,
      defaults = expression_defaults
    )
  }, error = function(e) {
    res$status <- 400
    list(error = conditionMessage(e))
  })
}


#* @parser multi
#* @post /expression/plot
#* @serializer contentType list(type = "image/png")
function(req, res) {
  tryCatch({
    parts <- req$body %||% list()
    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }
    data_part <- get_part("data_file")
    attr_part <- get_part("attribute_file")
    payload_part <- get_part("payload")
    if (is.null(data_part) || is.null(attr_part) || is.null(payload_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "data_file, attribute_file, and payload are required."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    tmp_expr <- write_part_to_tempfile(data_part, "csv")
    tmp_attr <- write_part_to_tempfile(attr_part, "csv")
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(c(tmp_expr, tmp_attr, tmp_png)), add = TRUE)

    payload_json <- rawToChar(payload_part$value)
    params <- normalize_expression_params(fromJSON(payload_json, simplifyVector = TRUE))
    expr_df <- read_expression_matrix(tmp_expr)
    attr_df <- read_attribute_table(tmp_attr)
    # Diagnostics: capture previews and run the same helper used for plotting
    diag <- list(
      payload = tryCatch(substr(payload_json, 1, 2000), error = function(e) NULL),
      expr_preview = tryCatch(utils::head(expr_df, 5), error = function(e) NULL),
      attr_preview = tryCatch(utils::head(attr_df, 5), error = function(e) NULL),
      expr_cols = tryCatch(names(expr_df), error = function(e) NULL),
      attr_cols = tryCatch(names(attr_df), error = function(e) NULL),
      genes = tryCatch(as.character(params$selected_gene), error = function(e) NULL),
      attribute_column = tryCatch(as.character(params$attribute_column[1]), error = function(e) NULL),
      selected_values = tryCatch(as.character(params$selected_values), error = function(e) NULL)
    )

    diag_long <- tryCatch({
      prepare_expression_long(
        expr_df = expr_df,
        attr_df = attr_df,
        genes = params$selected_gene,
        attribute_column = params$attribute_column,
        selected_values = params$selected_values,
        group_by_column = params$group_by_column
      )
    }, error = function(e) {
      structure(list(error = conditionMessage(e)), class = "diag_error")
    })

    if (inherits(diag_long, "diag_error")) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      debug_info <- list(
        error = diag_long$error,
        diagnostics = diag
      )
      return(charToRaw(jsonlite::toJSON(debug_info, auto_unbox = TRUE, null = "null", dataframe = "rows")))
    }

    diag$long_preview <- tryCatch(utils::head(diag_long, 20), error = function(e) NULL)

    # If diagnostics passed, proceed to build plot bundle
    bundle <- build_expression_plot_bundle(expr_df, attr_df, params)
    write_expression_plot_file(bundle, params, tmp_png, "png")
    readBin(tmp_png, "raw", n = file.info(tmp_png)$size)
  }, error = function(e) {
    message("Expression plot error: ", conditionMessage(e))
    # Try to include helpful debug info without exposing file contents
    safe_payload <- tryCatch({ rawToChar(payload_part$value) }, error = function(e) NULL)
    safe_params <- tryCatch({ if (exists("params")) params else NULL }, error = function(e) NULL)
    safe_expr_cols <- tryCatch({ if (exists("expr_df")) names(expr_df) else NULL }, error = function(e) NULL)
    safe_attr_cols <- tryCatch({ if (exists("attr_df")) names(attr_df) else NULL }, error = function(e) NULL)
    debug_info <- list(
      error = conditionMessage(e),
      payload = if (!is.null(safe_payload)) substr(safe_payload, 1, 1000) else NULL,
      params = if (!is.null(safe_params)) { list(
        attribute_column = safe_params$attribute_column %||% NULL,
        selected_values_len = length(safe_params$selected_values %||% list()),
        selected_gene_len = length(safe_params$selected_gene %||% list())
      ) } else NULL,
      expr_cols = safe_expr_cols,
      attr_cols = safe_attr_cols
    )
    res$status <- 400
    res$setHeader("Content-Type", "application/json")
    err_json <- jsonlite::toJSON(debug_info, auto_unbox = TRUE, null = "null")
    charToRaw(err_json)
  })
}


#* @parser multi
#* @post /expression/export
#* @serializer contentType list(type = "application/octet-stream")
function(req, res) {
  tryCatch({
    parts <- req$body %||% list()
    get_part <- function(name) {
      for (p in parts) {
        if (!is.null(p$name) && identical(p$name, name)) return(p)
      }
      NULL
    }
    data_part <- get_part("data_file")
    attr_part <- get_part("attribute_file")
    payload_part <- get_part("payload")
    if (is.null(data_part) || is.null(attr_part) || is.null(payload_part)) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      err_json <- jsonlite::toJSON(
        list(error = "data_file, attribute_file, and payload are required."),
        auto_unbox = TRUE
      )
      return(charToRaw(err_json))
    }
    tmp_expr <- write_part_to_tempfile(data_part, "csv")
    tmp_attr <- write_part_to_tempfile(attr_part, "csv")

    payload_json <- rawToChar(payload_part$value)
    params <- normalize_expression_params(fromJSON(payload_json, simplifyVector = TRUE))
    file_type <- params$file_type %||% "png"
    tmp_target <- tempfile(fileext = paste0(".", file_type))
    on.exit(unlink(c(tmp_expr, tmp_attr, tmp_target)), add = TRUE)
    expr_df <- read_expression_matrix(tmp_expr)
    attr_df <- read_attribute_table(tmp_attr)
    bundle <- build_expression_plot_bundle(expr_df, attr_df, params)

    write_expression_plot_file(bundle, params, tmp_target, file_type)
    readBin(tmp_target, "raw", n = file.info(tmp_target)$size)
  }, error = function(e) {
    message("Expression export error: ", conditionMessage(e))
    res$status <- 400
    res$setHeader("Content-Type", "application/json")
    err_json <- jsonlite::toJSON(list(error = conditionMessage(e)), auto_unbox = TRUE)
    charToRaw(err_json)
  })
}

#* @get /
#* @serializer html
function() {
  read_file(file.path("..", "home.html"))
}

#* @get /expression
#* @serializer html
function() {
  read_file(file.path("www", "expression.html"))
}

#* @get /volcano
#* @serializer html
function() {
  read_file(file.path("www", "volcano-ma.html"))
}

#* @get /styles.css
#* @serializer contentType list(type="text/css")
function() {
  read_file(file.path("www", "styles.css"))
}

#* @get /app.js
#* @serializer contentType list(type="text/javascript")
function() {
  read_file(file.path("www", "app.js"))
}

#* @get /expression.js
#* @serializer contentType list(type="text/javascript")
function() {
  read_file(file.path("www", "expression.js"))
}

# =====================================================
# DESeq2 ENDPOINT (volcano/MA ready)
# =====================================================

map_symbols_for_species <- function(ids, species = "human", id_type = NULL) {
  # Returns a character vector of symbols, falling back to the input IDs.
  if (!requireNamespace("AnnotationDbi", quietly = TRUE)) {
    warning("AnnotationDbi not installed; returning input IDs as symbols.")
    return(ids)
  }
  pkg <- switch(tolower(species),
    "human" = "org.Hs.eg.db",
    "mouse" = "org.Mm.eg.db",
    "fly"   = "org.Dm.eg.db",
    NULL
  )
  if (is.null(pkg) || !requireNamespace(pkg, quietly = TRUE)) {
    warning(sprintf("Org package for species '%s' not installed; returning input IDs as symbols.", species))
    return(ids)
  }

  # Guess keytype if not provided
  guess_keytype <- function(x) {
    if (all(grepl("^FBgn", x, ignore.case = FALSE))) return("FLYBASE")
    if (all(grepl("^ENS", x, ignore.case = TRUE))) return("ENSEMBL")
    "SYMBOL"
  }
  keytype <- id_type %||% guess_keytype(ids)

  suppressWarnings({
    db_obj <- get(pkg, envir = asNamespace(pkg))
    mapped <- AnnotationDbi::mapIds(
      x = db_obj,
      keys = ids,
      column = "SYMBOL",
      keytype = keytype,
      multiVals = "first"
    )
  })
  mapped[is.na(mapped) | mapped == ""] <- ids[is.na(mapped) | mapped == ""]
  unname(as.character(mapped))
}

#' Run DESeq2 differential expression
#' @parser multi
#' @post /api/deseq
#' @serializer json
function(req, res) {
  library(DESeq2)
  library(jsonlite)

  parts <- req$body %||% list()

  get_part <- function(name) {
    for (p in parts) {
      if (!is.null(p$name) && identical(p$name, name)) return(p)
    }
    NULL
  }

  file_part <- get_part("counts_file")
  json_part <- get_part("payload")

  if (is.null(file_part)) {
    res$status <- 400
    return(list(error = "No counts matrix uploaded."))
  }

  if (is.null(json_part)) {
    res$status <- 400
    return(list(error = "No sample groups (payload) provided."))
  }

  tmp_path <- tempfile(fileext = ".csv")
  writeBin(file_part$value, tmp_path)
  counts <- read.csv(tmp_path, row.names = 1, check.names = FALSE)

  body <- jsonlite::fromJSON(rawToChar(json_part$value))
  g1 <- trimws(unlist(strsplit(body$group1, ",")))
  g2 <- trimws(unlist(strsplit(body$group2, ",")))
  samples <- c(g1, g2)

  if (!all(samples %in% colnames(counts))) {
    missing <- samples[!samples %in% colnames(counts)]
    res$status <- 400
    return(list(error = paste("Missing samples:", paste(missing, collapse=", "))))
  }

  condition <- factor(c(rep("group1", length(g1)), rep("group2", length(g2))))
  colData <- data.frame(row.names = samples, condition = condition)

  dds <- DESeqDataSetFromMatrix(
    countData = counts[, samples],
    colData = colData,
    design = ~ condition
  )

  dds <- DESeq(dds)
  res_df <- as.data.frame(results(dds))
  res_df$gene <- rownames(res_df)

  species <- body$species %||% "human"
  id_type <- body$id_type %||% NULL
  res_df$symbol <- map_symbols_for_species(res_df$gene, species = species, id_type = id_type)

  volcano_df <- res_df %>%
    dplyr::transmute(
      base_mean = baseMean,
      log2fold_change = log2FoldChange,
      padj = padj,
      symbol = symbol,
      gene_id = gene
    )

  list(
    results = volcano_df,
    meta = list(
      species = species,
      id_type = id_type %||% NA_character_
    )
  )
}


#* @get /deseq
#* @serializer html
function() {
  read_file(file.path("www", "deseq.html"))
}
