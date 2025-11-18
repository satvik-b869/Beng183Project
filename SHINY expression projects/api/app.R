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

safe_nzchar <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

ensure_numeric <- function(value, default) {
  if (is.null(value) || value == "") return(default)
  maybe_num <- suppressWarnings(as.numeric(value))
  if (is.na(maybe_num)) default else maybe_num
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
  
  expected <- c("base_mean", "log2fold_change", "padj", "symbol", "gene_id")
  if (!all(expected %in% colnames(df))) {
    stop(paste(
      "Uploaded file is missing required columns after cleaning/renaming.",
      "Required:", paste(expected, collapse = ", "),
      "Have:", paste(colnames(df), collapse = ", ")
    ))
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

    tmp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp_file), add = TRUE)
    writeBin(file_part$value, tmp_file)

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
    
    # file_part$value is a raw vector with the file contents
    tmp_file <- tempfile(fileext = ".csv")
    writeBin(file_part$value, tmp_file)
    
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
    
    tmp_file <- tempfile(fileext = ".csv")
    writeBin(file_part$value, tmp_file)
    
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

#* @get /
#* @serializer html
function() {
  read_file(file.path("..", "home.html"))
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

