library(magrittr)

utils::globalVariables(c("Symbol", "sample_name", "NormalizedCounts", "Group", "Broad_Group", "Gene_Attribute", "Sample_Attribute", "SD")) # nolint

read_expression_matrix <- function(path) {
  if (!file.exists(path)) stop("Expression matrix not found")
  ext <- tools::file_ext(path)
  df <- switch(
    tolower(ext),
    "txt" = readr::read_delim(path, show_col_types = FALSE),
    "csv" = readr::read_csv(path, show_col_types = FALSE),
    stop("Unsupported expression matrix format")
  )
  df <- janitor::clean_names(df)
  gene_col <- names(df)[grepl("symbol|gene|gene_id|hgnc|ensembl|id", names(df), ignore.case = TRUE)][1] # nolint: line_length_linter.
  if (is.na(gene_col) || gene_col == "") gene_col <- names(df)[1]
  df %>% dplyr::rename(Symbol = tidyselect::all_of(gene_col)) %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x, 4))) # nolint: line_length_linter.
}

read_attribute_table <- function(path) {
  if (!file.exists(path)) stop("Attribute table not found")
  ext <- tolower(tools::file_ext(path))
  attr_tbl <- switch(
    ext,
    "txt" = readr::read_delim(path, show_col_types = FALSE),
    "csv" = readr::read_csv(path, show_col_types = FALSE),
    stop("Unsupported attribute table format")
  )
  attr_tbl <- janitor::clean_names(attr_tbl)
  sample_matches <- names(attr_tbl)[grepl("sample|sample_name|run|accession|library", names(attr_tbl), ignore.case = TRUE)] # nolint: line_length_linter.
  sample_col <- if (length(sample_matches) >= 1) sample_matches[1] else if (ncol(attr_tbl) >= 1) names(attr_tbl)[1] else stop("Attribute table has no columns") # nolint: line_length_linter.
  attr_tbl %>%
    dplyr::rename(sample_name = dplyr::all_of(sample_col)) %>%
    dplyr::mutate(sample_name = janitor::make_clean_names(as.character(sample_name))) # nolint: line_length_linter.
}

prepare_expression_long <- function(expr_df,
                                    attr_df,
                                    genes,
                                    attribute_column,
                                    selected_values,
                                    group_by_column = NULL,
                                    time_regex = "_\\d+ hr") {
  # Mirror the Shiny `plot_data` logic
  if (is.null(expr_df) || is.null(attr_df)) stop("Expression and attribute data are required")# nolint: line_length_linter.
  if (is.null(genes) || length(genes) == 0) stop("At least one gene must be provided")# nolint: line_length_linter.
  if (is.null(attribute_column) || length(attribute_column) == 0) stop("Attribute column missing in metadata")# nolint: line_length_linter.

  attribute_column <- as.character(attribute_column[1])
  if (!nzchar(attribute_column) || !(attribute_column %in% names(attr_df))) stop("Attribute column missing in metadata")# nolint: line_length_linter.
  if (is.null(selected_values) || length(selected_values) == 0) stop("Select at least one attribute value")# nolint: line_length_linter.

  # Select samples by attribute values
  selected_samples <- attr_df$sample_name[attr_df[[attribute_column]] %in% selected_values]# nolint: line_length_linter.
  selected_attributes <- attr_df[attr_df$sample_name %in% selected_samples, , drop = FALSE]

  # Create Group column flexibly based on available metadata
  group_by_column <- if (is.null(group_by_column) || length(group_by_column) == 0) {
    character(0)
  } else {
    as.character(group_by_column[1])
  }
  group_column <- if ("group" %in% names(selected_attributes)) {
    "group"
  } else if (length(group_by_column) == 1 && nzchar(group_by_column) && group_by_column %in% names(selected_attributes)) {
    group_by_column
  } else {
    attribute_column
  }
  if (!group_column %in% names(selected_attributes)) stop(sprintf("Unable to derive grouping column '%s'", group_column))
  selected_attributes$Group <- selected_attributes[[group_column]]
  

  # Subset expression matrix
  if (!"Symbol" %in% names(expr_df)) stop("Expression dataframe missing 'Symbol' column") # nolint
  selected_data <- expr_df %>% dplyr::filter(Symbol %in% genes)
  if (ncol(selected_data) < 2) stop(sprintf("Selected expression data has insufficient columns: %d", ncol(selected_data))) # nolint

  # Pivot and join
  long_data <- selected_data %>%
    tidyr::pivot_longer(cols = -Symbol, names_to = "sample_name", values_to = "NormalizedCounts") %>%
    dplyr::left_join(selected_attributes, by = "sample_name") %>%
    dplyr::mutate(
      Broad_Group = if (!is.null(time_regex) && nzchar(time_regex)) sub(time_regex, "", Group) else Group,
      Gene_Attribute = if (length(genes) >= 2) paste(Group, Symbol, sep = " - ") else Group,
      Sample_Attribute = paste(sample_name, Symbol, sep = " - ")
    ) %>%
    dplyr::filter(!is.na(Gene_Attribute) & !is.na(Group))

  if (nrow(long_data) == 0) stop("No data available after filtering. Please check your data.")
  long_data
}

expression_group_colors <- function(long_data, palette = "Set1") {
  group_values <- unique(long_data$Broad_Group)
  n_colors <- max(3, length(group_values))
  base_palette <- RColorBrewer::brewer.pal(min(n_colors, 9), palette)
  color_palette <- grDevices::colorRampPalette(base_palette)(n_colors)
  stats::setNames(color_palette[seq_along(group_values)], group_values)
}

expression_axis_limits <- function(long_data) {
  list(
    y_min = floor(min(long_data$NormalizedCounts, na.rm = TRUE)),
    y_max = ceiling(max(long_data$NormalizedCounts, na.rm = TRUE))
  )
}

build_boxplot_plot <- function(long_data,
                               colors,
                               remove_x_labels = FALSE,
                               text_xaxis = "",
                               text_yaxis = "Log2 Normalized Counts",
                               title = "",
                               subtitle = "",
                               legend_title = "Legend",
                               legend_position = "right",
                               axis_size = 14,
                               title_size = 23,
                               subtitle_size = 18,
                               legend_title_size = 14,
                               legend_text_size = 14,
                               text_color = "#000000",
                               y_limits = NULL,
                               value_order = NULL,
                               facet_column = NULL) {
  long_data <- long_data %>% dplyr::arrange(Gene_Attribute, Group)
  if (!is.null(value_order)) {
    long_data$Gene_Attribute <- factor(long_data$Gene_Attribute, levels = value_order)
  }
  p <- ggplot2::ggplot(long_data, ggplot2::aes(x = Gene_Attribute, y = NormalizedCounts, fill = Broad_Group)) +
    ggplot2::geom_boxplot(outlier.shape = NA, color = "darkgray", alpha = 0.75) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.5, color = "darkgray") +
    ggplot2::geom_jitter(width = 0.2, size = 1.5, color = "black") +
    ggplot2::labs(x = text_xaxis, y = text_yaxis, title = title, subtitle = subtitle, fill = legend_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = if (remove_x_labels) ggplot2::element_blank() else ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = if (remove_x_labels) ggplot2::element_blank() else ggplot2::element_line(),
      text = ggplot2::element_text(color = text_color),
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.text = ggplot2::element_text(size = legend_text_size),
      plot.title = ggplot2::element_text(size = title_size),
      plot.subtitle = ggplot2::element_text(size = subtitle_size),
      axis.title = ggplot2::element_text(size = axis_size),
      axis.text = ggplot2::element_text(size = axis_size),
      legend.position = legend_position
    ) +
    ggplot2::scale_fill_manual(values = colors, name = legend_title) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title))

  if (!is.null(y_limits)) {
    p <- p + ggplot2::ylim(y_limits[1], y_limits[2])
  }
  if (!is.null(facet_column) && nzchar(facet_column) && facet_column %in% names(long_data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste(". ~", facet_column)), scales = "free")
  }
  p
}

build_bar_plot <- function(long_data,
                           colors,
                           remove_x_labels = FALSE,
                           text_xaxis = "",
                           text_yaxis = "Log2 Normalized Counts",
                           title = "",
                           subtitle = "",
                           legend_title = "Legend",
                           legend_position = "right",
                           axis_size = 14,
                           title_size = 23,
                           subtitle_size = 18,
                           legend_title_size = 14,
                           legend_text_size = 14,
                           text_color = "#000000",
                           y_limits = NULL,
                           sample_order = NULL,
                           facet_column = NULL,
                           include_error_bars = FALSE) {
  long_data <- long_data %>% dplyr::arrange(Sample_Attribute)
  if (!is.null(sample_order)) {
    long_data$Sample_Attribute <- factor(long_data$Sample_Attribute, levels = sample_order)
  }
  p <- ggplot2::ggplot(long_data, ggplot2::aes(x = Sample_Attribute, y = NormalizedCounts, fill = Broad_Group)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(x = text_xaxis, y = text_yaxis, title = title, subtitle = subtitle, fill = legend_title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = if (remove_x_labels) ggplot2::element_blank() else ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = if (remove_x_labels) ggplot2::element_blank() else ggplot2::element_line(),
      text = ggplot2::element_text(color = text_color),
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.text = ggplot2::element_text(size = legend_text_size),
      plot.title = ggplot2::element_text(size = title_size),
      plot.subtitle = ggplot2::element_text(size = subtitle_size),
      axis.title = ggplot2::element_text(size = axis_size),
      axis.text = ggplot2::element_text(size = axis_size),
      legend.position = legend_position
    ) +
    ggplot2::scale_fill_manual(values = colors, name = legend_title) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title))

  if (!is.null(y_limits)) {
    p <- p + ggplot2::ylim(0, y_limits[2])
  }
  if (include_error_bars && "SD" %in% names(long_data)) {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = NormalizedCounts - SD, ymax = NormalizedCounts + SD), width = 0.25, position = ggplot2::position_dodge(width = 0.9))
  }
  if (!is.null(facet_column) && nzchar(facet_column) && facet_column %in% names(long_data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste(". ~", facet_column)), scales = "free")
  }
  p
}

prepare_heatmap_components <- function(long_data,
                                       color_scheme = "RdYlBu",
                                       cluster_rows = TRUE,
                                       cluster_columns = TRUE) {
  heatmap_data <- long_data %>%
    dplyr::group_by(Symbol, sample_name) %>%
    dplyr::summarise(NormalizedCounts = mean(NormalizedCounts, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = sample_name, values_from = NormalizedCounts)
  matrix_data <- heatmap_data %>%
    tibble::column_to_rownames(var = "Symbol") %>%
    as.matrix()
  matrix_data[is.na(matrix_data)] <- 0

  annotation_col <- long_data %>%
    dplyr::select(sample_name, Group, Broad_Group) %>%
    dplyr::distinct() %>%
    tibble::column_to_rownames("sample_name")
  annotation_col <- annotation_col[colnames(matrix_data), , drop = FALSE]
  annotation_col$Group <- factor(annotation_col$Group)

  palette <- colorRampPalette(RColorBrewer::brewer.pal(9, color_scheme))(100)

  list(
    matrix = matrix_data,
    annotation = annotation_col,
    palette = palette,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns
  )
}
