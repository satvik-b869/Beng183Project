library(plyr)
library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)
library(colorspace)
library(shinyWidgets)
library(colourpicker) 
library(bslib)
library(ggplot2)


#install.packages('rsconnect')
# install.packages("shiny")
# install.packages("jsonlite")


#copy to clipboard
#add watermark



convertUnits <- function(value, from, to) { # nolint
  conversion_factors <- list(
    "in" = 1,
    "cm" = 2.54,
    "mm" = 25.4
  )
  return(value * conversion_factors[[to]] / conversion_factors[[from]])
}


ui <- fluidPage(
  theme = bs_theme(font_scale = 0.7),
  tags$head(
    tags$script(src = "https://cdn.tailwindcss.com?plugins=forms,container-queries"),
    tags$script(HTML("tailwind.config = {darkMode: 'class', theme: {extend: {colors: {primary: '#2C097F', 'background-light': '#f6f6f8', 'background-dark': '#151022'}, fontFamily: {display: 'Inter'}, borderRadius: {DEFAULT: '0.25rem', lg: '0.5rem', xl: '0.75rem', full: '9999px'}}}};")),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined"),
    tags$style(HTML("
      .material-symbols-outlined {
        font-variation-settings:
          'FILL' 0,
          'wght' 400,
          'GRAD' 0,
          'opsz' 24;
      }
      [type = 'number'] { font-size:14px; height:20px; }
      [type = 'text'] { font-size:14px; height:20px; }
    ")),
    tags$script(HTML("
      var dimension = [0, 0];
      $(document).on('shiny:connected', function(e) {
        var plot = $('#plot');
        dimension[0] = plot.width();
        dimension[1] = plot.height();
        Shiny.onInputChange('dimension', dimension);
      });
      $(window).resize(function(e) {
        var plot = $('#plot');
        dimension[0] = plot.width();
        dimension[1] = plot.height();
        Shiny.onInputChange('dimension', dimension);
      });
      var updateHeight = function() {
        var plot = $('#plot');
        var height = $(window).height() - plot.offset().top - 50;
        plot.css('height', height + 'px');
        Shiny.onInputChange('plot_height', height);
      };
      $(document).on('shiny:connected', function(e) {
        updateHeight();
      });
      $(window).resize(function(e) {
        updateHeight();
      });
    "))
  ),
  div(
    class = "flex h-screen bg-background-light dark:bg-background-dark font-display text-gray-800 dark:text-gray-200",
    tags$aside(
      class = "w-[320px] flex-shrink-0 bg-white/5 dark:bg-background-dark border-r border-gray-200/10 dark:border-white/10 p-6 flex flex-col h-full overflow-y-auto",
      tags$div(
        class = "flex items-center gap-3 mb-8",
        tags$div(class = "bg-center bg-no-repeat aspect-square bg-cover rounded-full size-10",
                 style = 'background-image: url("https://lh3.googleusercontent.com/aida-public/AB6AXuB6VYa5Jg-ez2xlIiIshEJNkZe72jVCCZuBpVLehHiJgI3kXcB-L6BVUfqT7LMR2mCKEIhlDVRiIDXKn6LTQDrmpWvjESptBx8gcPlMZGCrUXOpr1zZB6TmcNW_s9P-BOZJM6t-kQ8fHVDb7iG7u-_uPCPXfNnbZ3Zvz6kRwY0rxbpoWy385Nskm96X3fgWKTeEITfx-GB4gCazSuWm1VJoqVtupmXryLkDAn6xZTYh5WKkG-2skdIS7Owj9W9QO5b8mA1tnAwUVSE-");'),
        tags$div(
          class = "flex flex-col",
          tags$h1(class = "text-white text-base font-medium leading-normal", "Plot Generator"),
          tags$p(class = "text-gray-400 dark:text-gray-500 text-sm font-normal leading-normal", "Volcano / MA Plots")
        )
      ),
      tags$nav(
        class = "flex flex-col gap-4",
        tags$div(
          class = "flex flex-col gap-2 p-4 rounded-lg bg-white/5 dark:bg-black/20",
          tags$div(class = "flex items-center gap-3",
                   tags$span(class = "material-symbols-outlined text-primary text-2xl", "upload_file"),
                   tags$p(class = "text-white text-sm font-medium leading-normal", "File Upload")),
          tags$p(class = "text-gray-400 dark:text-gray-500 text-xs", "Upload your .csv or .txt file to begin."),
          fileInput("file1", NULL, buttonLabel = "Browse Files", placeholder = "No file selected",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt"))
        ),
        tags$div(
          class = "flex flex-col gap-2 p-4 rounded-lg bg-white/5 dark:bg-black/20",
          checkboxInput("showMA", "Switch to MA Plot", FALSE),
          checkboxInput("advancedOptions", "Show Advanced Options", FALSE)
        ),
        tags$div(
          class = "flex flex-col gap-2",
          accordion(
            accordion_panel(
              title = "Primary controls",
              numericInput("fc_thresholdupreg", "Fold Change Threshold for Up Regulated Genes:", 1.5, min = 1),
              numericInput("fc_thresholddownreg", "Fold Change Threshold for Down Regulated Genes :", -1.5, max = -1),
              numericInput("p_val_threshold", "Adjusted p-value Threshold:", 0.05),
              colourInput("line_color", "Color for threshold lines:", value = "black"),
              colourInput("up_color", "Color for up regulated genes:", value = "#75ba84"),
              colourInput("down_color", "Color for downregulated genes:", value = "#c09ed6"),
              colourInput("no_color", "Color for non-significant genes:", value = "#A8A8A8")
            ),
            accordion_panel(
              "Axis controls",
              numericInput("y_min", "Y-axis minimum value:", 0),
              numericInput("y_max", "Y-axis maximum value:", 50),
              numericInput("x_min", "X-axis minimum value:", -10),
              numericInput("x_max", "X-axis maximum value:", 10),
              conditionalPanel(
                condition = "input.advancedOptions == true",
                numericInput("breakx", "X-axis Tick Interval:", 2),
                numericInput("breaky", "Y-axis Tick Interval:", 10)
              ),
              textInput("text_xaxis", "X-axis label:", value = "Log2 Fold Change"),
              textInput("text_yaxis", "Y-axis label:", value = "-log10(p-val)"),
              numericInput("textlab_size_axis", "Text size for labels on axis:", 14)
            ),
            accordion_panel(
              "Title and Labels controls",
              textInput("title", "Plot title:", value = ""),
              textInput("subtitle", "Plot subtitle:", value = ""),
              numericInput("textlab_size_title", "Text size for Title:", 25),
              numericInput("textlab_size_subtitle", "Text size for subtitle:", 0),
              textInput("legend_title", "Legend title:", value = "Legend"),
              selectInput("legendPosition", "Legend Position:",
                          choices = c("right", "left", "top", "bottom", "none")),
              numericInput("textlab_size_legend", "Text size for legend:", 14),
              numericInput("textlab_size_legend_content", "Text size for legend contents:", 14),
              textInput("custom_upreg_text", "Custom Label for Up-regulated genes:", value = "Up"),
              textInput("custom_downreg_text", "Custom Label for Down-regulated genes:", value = "Down"),
              textInput("custom_nochange_text", "Custom Label for Non-significant genes:", value = "No Change"),
              colourInput("textlab_color_axis", "Text color for labels:", value = "black")
            ),
            accordion_panel(
              "Points controls",
              numericInput("point_size_UP", "Point Size for up regulated genes:", 1.5),
              numericInput("point_size_DOWN", "Point Size for downregulated genes:", 1.5),
              numericInput("point_size_NO", "Point Size for non diff. expressed genes:", 1.0),
              conditionalPanel(
                condition = "input.advancedOptions == true",
                sliderInput("alpha_UP", "Transparency for up regulated genes:", min = 0, max = 1, value = 1),
                sliderInput("alpha_DOWN", "Transparency for downregulated genes:", min = 0, max = 1, value = 1),
                sliderInput("alpha_NO", "Transparency for non diff. expressed genes:", min = 0, max = 1, value = 0.25)
              )
            ),
            accordion_panel(
              "Genes to label",
              textInput("label_gene_symbols", "Specific Gene Symbols to Label (comma-separated):", value = ""),
              numericInput("top_genes", "Select the top x diff. expressed genes", value = 0, min = 0),
              colourInput("up_color_selected", "Color for selected up regulated genes:", value = "#1B75BB"),
              colourInput("down_color_selected", "Color for selected downregulated genes:", value = "#E62319"),
              colourInput("no_color_selected", "Color for selected non diff. expressed genes:", value = "black"),
              numericInput("point_size_selected", "Point Size for Selected Genes:", 3.0),
              numericInput("text_size_pointlabs", "Text size for labels on points:", 12),
              colourInput("text_color_pointlabs", "Text color for labels on points:", value = "black"),
              conditionalPanel(
                condition = "input.advancedOptions == true",
                sliderInput("alpha_UPselected", "Transparency for Selected up regulated genes:", 1.0, min = 0, max = 1),
                sliderInput("alpha_DOWNselected", "Transparency for Selected downregulated genes:", 1.0, min = 0, max = 1),
                sliderInput("alpha_NOselected", "Transparency for Selected non diff. expressed genes:", 1.0, min = 0, max = 1)
              )
            ),
            accordion_panel(
              "Export plot",
              textInput("file_name", "Output file name for the plot:", value = "volcano_plot"),
              radioButtons("colormodel", "Color Model", choices = c("RGB", "CMYK")),
              uiOutput("file_type_ui"),
              numericInput("width", "Width of plot:", 11),
              numericInput("height", "Height of plot:", 8),
              selectInput("units", "Units :", choices = c("in", "cm", "mm", "px")),
              conditionalPanel(
                condition = "input.advancedOptions == true",
                numericInput("dpi", "DPI:", 300),
                numericInput("zoom", "Zoom:", 1)
              )
            ),
            accordion_panel(
              "Save/Load Settings",
              p("This feature allows you to save all your favorite options, so you can easily reload them in the future. It will not save the file inputted, but will save all modifications."),
              fileInput("loadSettings", label = "Load Settings from a File:", accept = ".csv"),
              textInput("saveFilename", label = "Filename to Save Current Settings:", value = "settings.csv")
            )
          )
        )
      ),
      tags$div(
        class = "mt-auto pt-8 flex flex-col gap-2",
        downloadButton("downloadPlot", "Export Plot", class = "flex w-full items-center justify-center gap-2 overflow-hidden rounded-md h-10 px-4 bg-primary/20 text-primary text-sm font-bold tracking-[0.015em] hover:bg-primary/30"),
        actionButton("saveSettings", "Save Settings", class = "flex w-full items-center justify-center gap-2 overflow-hidden rounded-md h-10 px-4 bg-white/10 text-white text-sm font-bold tracking-[0.015em] hover:bg-white/20")
      )
    ),
    tags$main(
      class = "flex-1 p-8 overflow-y-auto",
      tags$div(
        class = "flex flex-col items-center justify-center h-full w-full",
        conditionalPanel(
          condition = "!input.file1",
          tags$div(
            class = "flex flex-col items-center gap-6 rounded-lg border-2 border-dashed border-gray-300/20 dark:border-white/20 px-6 py-14 w-full max-w-2xl text-center",
            tags$div(
              class = "flex flex-col items-center gap-2",
              tags$p(class = "text-white text-lg font-bold leading-tight tracking-[-0.015em]", "Upload your data to begin"),
              tags$p(class = "text-gray-400 dark:text-gray-500 text-sm font-normal leading-normal max-w-md",
                     "Drag and drop your .csv or .txt file here, or use the sidebar to browse your files. The generated plot will be displayed in this area.")
            ),
            tags$span(class = "material-symbols-outlined text-5xl text-gray-500/50", "monitoring")
          )
        ),
        conditionalPanel(
          condition = "input.file1",
          tags$div(
            class = "w-full max-w-5xl",
            plotOutput("plot", height = "calc(100vh - 220px)", width = "100%")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) { # nolint: cyclocomp_linter.
  
  # Function to save current inputs to a CSV file
  observeEvent(input$saveSettings, {
    saveData <- reactiveValuesToList(input)  # Capture all inputs
    
    # Ensure each input is a scalar value
    saveDataFrame <- data.frame(
      variable = names(saveData),
      value = sapply(saveData, function(x) paste(x, collapse = ",")),  # Collapse multiple values into a single string
      stringsAsFactors = FALSE
    )
    
    # Save the data frame to a CSV file
    write.csv(saveDataFrame, input$saveFilename, row.names = FALSE)
    
    
    # Notify the user that the settings have been saved
    showModal(modalDialog(
      title = "Settings Saved",
      "Your settings have been saved successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  # Function to load settings from a CSV file
  observeEvent(input$loadSettings, {
    req(input$loadSettings)
    
    # Read the CSV file
    loadedData <- read.csv(input$loadSettings$datapath, stringsAsFactors = FALSE)
    
    # Update the inputs with the loaded values
    lapply(seq_len(nrow(loadedData)), function(i) {
      var_name <- loadedData$variable[i]
      var_value <- loadedData$value[i]
      
      # Convert back to the appropriate type if needed
      if (grepl(",", var_value)) {
        var_value <- strsplit(var_value, ",")[[1]]  # Split comma-separated values back into a vector
      }
      
      # Update the input based on its type
      if (var_name %in% names(input)) {
        if (is.numeric(input[[var_name]])) {
          updateNumericInput(session, var_name, value = as.numeric(var_value))
        } else if (is.character(input[[var_name]])) {
          updateTextInput(session, var_name, value = var_value)
        } else if (is.logical(input[[var_name]])) {
          updateCheckboxInput(session, var_name, value = as.logical(var_value))
        } else if (is.factor(input[[var_name]])) {
          updateSelectInput(session, var_name, selected = var_value)
        } # Add other input types as needed
      }
      
    })
    
    # Notify the user that the settings have been loaded
    showModal(modalDialog(
      title = "Settings Loaded",
      "Your settings have been loaded successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  #for reactive file choices
  current_units <- reactiveVal("in")
  output$file_type_ui <- renderUI({
    if (input$colormodel == "CMYK") {
      selectInput("file_type", "Output file type:", choices = c("pdf", "eps", "tiff"))
    } else {
      selectInput("file_type", "Output file type:", choices = c("png", "jpeg", "tiff", "pdf", "svg", "eps", "ps", "wmf"))
    }
  })
  
  observeEvent(input$units, {
    old_units <- current_units()
    new_units <- input$units
    
    if (old_units != new_units) {
      new_width <- convertUnits(input$width, old_units, new_units)
      new_height <- convertUnits(input$height, old_units, new_units)
      
      updateNumericInput(session, "width", value = new_width)
      updateNumericInput(session, "height", value = new_height)
      
      current_units(new_units)
    }
  }, ignoreInit = TRUE)
  
  options(shiny.maxRequestSize = 30*1024^2)  # 30 MB
  
  
  #read data, IMPORTANT  
  data <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$datapath)
    
    tryCatch({
      df <- if (ext == 'txt') {
        read_delim(input$file1$datapath) # nolint
      } else if (ext == 'csv') {
        read_csv(input$file1$datapath)
      } else {
        stop("Unsupported file type")
      }
      
      # Clean the column names before checking
      df <- df %>% clean_names()
      
      # Recheck if the uploaded file has the expected columns
      expected_columns <- c("base_mean", "log2fold_change", "padj", "symbol", "gene_id")
      if (!all(expected_columns %in% colnames(df))) {
        stop("The uploaded file does not have the correct structure. Please ensure it contains the following columns: baseMean, log2FoldChange, padj, Symbol, GeneID")
      }
      
      min_padj <- min(df$padj[df$padj > 0], na.rm = TRUE) # get smallest nonzero padj
      
      df <- df %>%
        clean_names() %>%
        mutate(padj = ifelse(padj == 0, (min_padj / 100), padj)) %>% # nolint
        select(base_mean, log2fold_change, padj, symbol, gene_id) # nolint
      
      df$colordiff <- input$custom_nochange_text
      df$colordiff[(df$log2fold_change >= log2(input$fc_thresholdupreg)) & (df$padj <= input$p_val_threshold)] <- input$custom_upreg_text
      df$colordiff[(df$log2fold_change <= -log2(abs(input$fc_thresholddownreg))) & (df$padj <= input$p_val_threshold)] <- input$custom_downreg_text
      
      # Mark selected genes
      if (!is.null(input$label_gene_symbols)) {
        selected_genes <- unlist(strsplit(input$label_gene_symbols, ",\\s*"))
        df <- df %>%
          mutate(selected = ifelse(symbol %in% selected_genes, "SELECTED", "NOT_SELECTED"))
      }
      
      # Update 'selected' column for partial matches
      if (!is.null(input$partial_match) && input$partial_match != '') {
        partial_patterns <- unlist(strsplit(input$partial_match, ",\\s*"))
        combined_pattern <- paste0(partial_patterns, collapse = "|")
        df <- df %>%
          mutate(selected = ifelse(symbol %in% selected_genes | str_detect(symbol, combined_pattern), "SELECTED", "NOT_SELECTED"))
      }
      
      # Select top differentially expressed genes if specified
      if (input$top_genes > 0) {
        top <- df %>%
          top_n(input$top_genes, wt = abs(log2fold_change)) # nolint
        df <- df %>%
          mutate(selected = ifelse(symbol %in% top$symbol, "SELECTED", selected))
      }
      
      # Update colordiff_selected and colordiff_nonselected based on user input
      df <- df %>%
        mutate(
          colordiff_selected = ifelse(selected == "SELECTED",
                                      ifelse(colordiff == input$custom_upreg_text,
                                             paste0(input$custom_upreg_text, "_SELECTED"),
                                             ifelse(colordiff == input$custom_downreg_text,
                                                    paste0(input$custom_downreg_text, "_SELECTED"),
                                                    paste0(input$custom_nochange_text, "_SELECTED"))),
                                      colordiff),
          colordiff_nonselected = ifelse(selected == "NOT_SELECTED", colordiff, colordiff)
        )
      
      # # Initialize legend order if not set yet
      # if (is.null(input$legend_order)) {
      #   updateRankList(session, "legend_order", labels = unique(df$colordiff_nonselected))
      # }
      
      # # Set the colordiff_nonselected column as a factor with levels specified by the user
      # df$colordiff_nonselected <- factor(df$colordiff_nonselected, levels = input$legend_order)
      
      # Arrange the dataframe based on the reordered colordiff_nonselected
      df <- df %>% arrange(colordiff_nonselected)
      
      removeNotification("file_error") # Remove the error notification if the file is correct
      return(df)
    }, error = function(e) {
      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = NULL,  # Keep the notification until the user dismisses it
        id = "file_error" # Use an ID to ensure we can remove it later
      )
      return(NULL)
    })
  })
  
  
  
  # Helper function to update inputs for the volcano plot
  updateVolcanoPlotInputs <- function(min_log2fc, max_log2fc, max_y, session) {
    x_min_value <- if (floor(min_log2fc) < 0) floor(min_log2fc) else 0
    
    # Calculate breakx with the conditional logic
    breakx_value <- floor((ceiling(max_log2fc) - x_min_value) / 5)
    breakx_value <- ifelse(breakx_value > 0, breakx_value, 1)
    
    # Calculate breaky with the conditional logic
    breaky_value <- floor((max_y + 0) / 5)
    breaky_value <- ifelse(breaky_value > 0, breaky_value, 1)
    
    updateTextInput(session, "title", value = "Volcano Plot")
    updateNumericInput(session, "y_min", value = 0)
    updateNumericInput(session, "y_max", value = max_y)
    updateNumericInput(session, "x_min", value = x_min_value)
    updateNumericInput(session, "x_max", value = ceiling(max_log2fc))
    updateTextInput(session, "text_xaxis", value = "Log2 Fold Change")
    updateTextInput(session, "text_yaxis", value = "-log10(p-val)")
    updateTextInput(session, "file_name", value = "volcano_plot")
    updateNumericInput(session, "breakx", value = breakx_value)
    updateNumericInput(session, "breaky", value = breaky_value)
  }
  
  
  
  # Helper function to update inputs for the MA plot
  updateMAPlotInputs <- function(min_log2fc, max_log2fc, new_min_x, new_max_x, session) {
    # Calculate breakx with the conditional logic
    breakx_value <- floor((ceiling(new_max_x) - new_min_x) / 5)
    breakx_value <- ifelse(breakx_value > 0, breakx_value, 1)
    
    # Calculate breaky with the conditional logic
    breaky_value <- floor((max_log2fc - min_log2fc) / 5)
    breaky_value <- ifelse(breaky_value > 0, breaky_value, 1)
    
    updateTextInput(session, "title", value = "MA Plot")
    updateNumericInput(session, "y_min", value = min_log2fc)
    updateNumericInput(session, "y_max", value = max_log2fc)
    updateNumericInput(session, "x_min", value = new_min_x)
    updateNumericInput(session, "x_max", value = ceiling(new_max_x))
    updateTextInput(session, "text_xaxis", value = "Log2 Mean Expression")
    updateTextInput(session, "text_yaxis", value = "Log2 Fold Change")
    updateTextInput(session, "file_name", value = "MA_plot")
    updateNumericInput(session, "breakx", value = breakx_value)
    updateNumericInput(session, "breaky", value = breaky_value)
  }
  
  
  # Observe the file input and update default values accordingly
  observeEvent(input$file1, {
    df <- data()
    min_log2fc <- min(df$log2fold_change, na.rm = TRUE)
    max_log2fc <- max(df$log2fold_change, na.rm = TRUE)
    
    c <- df$base_mean
    new_max_x <- min(mean(c, na.rm = TRUE) + 3 * sd(c, na.rm = TRUE), max(c, na.rm = TRUE))
    new_min_x <- max(mean(c, na.rm = TRUE) - 3 * sd(c, na.rm = TRUE), min(c, na.rm = TRUE))
    
    min_padj <- min(df$padj[df$padj > 0], na.rm = TRUE)
    max_y <- ceiling(-log10(min_padj) / 5) * 5
    
    # Conditional assignment for max_y
    max_y <- ifelse(max_y == 0, 8, max_y)
    
    if (input$showMA) {
      updateMAPlotInputs(floor(min_log2fc), ceiling(max_log2fc), floor(new_min_x), ceiling(new_max_x), session)
    } else {
      updateVolcanoPlotInputs(min_log2fc, max_log2fc, max_y, session)
    }
  })
  
  
  
  
  # Observe the showMA checkbox and update inputs accordingly
  observeEvent(input$showMA, {
    df <- data()
    
    
    min_log2fc <- min(df$log2fold_change, na.rm = TRUE)
    max_log2fc <- max(df$log2fold_change, na.rm = TRUE)
    
    c <-df$base_mean
    new_max_x <- min(mean(c,na.rm = T)+3*sd(c,na.rm = T), max(c,na.rm = T))
    new_min_x <- max(mean(c,na.rm = T)-3*sd(c,na.rm = T), min(c,na.rm = T))
    
    min_padj <- min(df$padj[df$padj > 0], na.rm = TRUE)
    max_y <- ceiling(-log10(min_padj) / 5) * 5
    
    # Conditional assignment for max_y
    max_y <- ifelse(max_y == 0, 8, max_y)
    
    if (input$showMA) {
      updateMAPlotInputs(floor(min_log2fc), ceiling(max_log2fc), floor(new_min_x), ceiling(new_max_x), session)
    } else {
      updateVolcanoPlotInputs(min_log2fc, max_log2fc, max_y, session)
    }
  })
  
  
  width <- reactive({
    width_units <- input$width
    if (input$units == "in") {
      width_units * input$dpi
    } else if (input$units == "cm") {
      width_units * input$dpi / 2.54
    } else if (input$units == "mm") {
      width_units * input$dpi / 25.4
    } else {
      width_units
    }
  })
  
  height <- reactive({
    height_units <- input$height
    if (input$units == "in") {
      height_units * input$dpi
    } else if (input$units == "cm") {
      height_units * input$dpi / 2.54
    } else if (input$units == "mm") {
      height_units * input$dpi / 25.4
    } else {
      height_units
    }
  })
  
  
  
  
  
  
  # Reactive expression to generate the plot
  output$plot <- renderImage({
    df <- data()
    legendPosition <- input$legendPosition
    req(df) # Ensure df is not NULL
    
    # Define colors, sizes, and alphas for selected and non-selected points
    cols_selected <- c(
      setNames(input$up_color_selected, paste0(input$custom_upreg_text, "_SELECTED")),
      setNames(input$down_color_selected, paste0(input$custom_downreg_text, "_SELECTED")),
      setNames(input$no_color_selected, paste0(input$custom_nochange_text, "_SELECTED"))
    )
    
    sizes_selected <- c(
      setNames(input$point_size_selected, paste0(input$custom_upreg_text, "_SELECTED")),
      setNames(input$point_size_selected, paste0(input$custom_downreg_text, "_SELECTED")),
      setNames(input$point_size_selected, paste0(input$custom_nochange_text, "_SELECTED"))
    )
    
    alphas_selected <- c(
      setNames(input$alpha_UPselected, paste0(input$custom_upreg_text, "_SELECTED")),
      setNames(input$alpha_DOWNselected, paste0(input$custom_downreg_text, "_SELECTED")),
      setNames(input$alpha_NOselected, paste0(input$custom_nochange_text, "_SELECTED"))
    )
    
    cols_nonselected <- c(
      setNames(input$up_color, input$custom_upreg_text),
      setNames(input$down_color, input$custom_downreg_text),
      setNames(input$no_color, input$custom_nochange_text)
    )
    
    sizes_nonselected <- c(
      setNames(input$point_size_UP, input$custom_upreg_text),
      setNames(input$point_size_DOWN, input$custom_downreg_text),
      setNames(input$point_size_NO, input$custom_nochange_text)
    )
    
    alphas_nonselected <- c(
      setNames(input$alpha_UP, input$custom_upreg_text),
      setNames(input$alpha_DOWN, input$custom_downreg_text),
      setNames(input$alpha_NO, input$custom_nochange_text)
    )
    
    
    # Set the colordiff_nonselected column as a factor with levels specified by the user
    # df$colordiff_nonselected <- factor(df$colordiff_nonselected, levels = input$legend_order)
    
    # # Arrange the dataframe based on the reordered colordiff_nonselected
    # df <- df %>% arrange(colordiff_nonselected)
    
    view(df)
    # Generate plot based on whether MA plot or Volcano plot is selected
    if(input$showMA){
      p <- ggplot(df, aes(x = base_mean, y = log2fold_change)) +
        geom_point(data = subset(df, selected == "NOT_SELECTED"),
                   aes(color = colordiff_nonselected, size = colordiff_nonselected, alpha = colordiff_nonselected)) +
        geom_point(data = subset(df, selected == "SELECTED"),
                   aes(fill = colordiff_selected, size = colordiff_selected, alpha = colordiff_selected), 
                   color = "black", shape = 21, show.legend = FALSE) +
        geom_hline(yintercept = c(-log2(abs(input$fc_thresholddownreg)), log2(input$fc_thresholdupreg)), 
                   linetype = "dashed", color = input$line_color) +
        scale_x_continuous(breaks = seq(input$x_min, input$x_max, input$breakx), limits = c(input$x_min, input$x_max)) +
        scale_y_continuous(breaks = seq(input$y_min, input$y_max, input$breaky), limits = c(input$y_min, input$y_max)) +
        scale_color_manual(values = cols_nonselected, name = input$legend_title) +
        scale_fill_manual(values = cols_selected, name = "Selected Genes") +
        scale_alpha_manual(values = c(alphas_nonselected, alphas_selected)) +
        scale_size_manual(values = c(sizes_nonselected, sizes_selected)) +
        theme_bw() +
        theme(
          text = element_text(color = input$textlab_color_axis),
          legend.title = element_text(size = input$textlab_size_legend),
          legend.text = element_text(size = input$textlab_size_legend_content),
          plot.title = element_text(size = input$textlab_size_title),
          plot.subtitle = element_text(size = input$textlab_size_subtitle),
          axis.title = element_text(size = input$textlab_size_axis),
          axis.text = element_text(size = input$textlab_size_axis),
          legend.position = legendPosition
        ) +
        labs(x = input$text_xaxis, y = input$text_yaxis, title = input$title, subtitle = input$subtitle) +
        guides(
          color = guide_legend(override.aes = list(alpha = alphas_nonselected[1], size = sizes_nonselected[1])),
          fill = "none",
          alpha = "none",
          size = "none"
        )
    } else {
      p <- ggplot(df, aes(x = log2fold_change, y = -log10(padj))) +
        geom_point(data = subset(df, selected == "NOT_SELECTED"), 
                   aes(color = colordiff_nonselected, size = colordiff_nonselected, alpha = colordiff_nonselected)) +
        geom_point(data = subset(df, selected == "SELECTED"), 
                   aes(fill = colordiff_selected, size = colordiff_selected, alpha = colordiff_selected), 
                   color = "black", shape = 21, show.legend = FALSE) +
        geom_vline(xintercept = c(-log2(abs(input$fc_thresholddownreg)), log2(input$fc_thresholdupreg)), 
                   linetype = "dashed", color = input$line_color) +
        geom_hline(yintercept = -log10(input$p_val_threshold), linetype = "dashed", color = input$line_color) +
        scale_x_continuous(breaks = seq(input$x_min, input$x_max, input$breakx), limits = c(input$x_min, input$x_max)) + #error
        scale_y_continuous(breaks = seq(input$y_min, input$y_max, input$breaky), limits = c(input$y_min, input$y_max)) +
        scale_color_manual(values = cols_nonselected, name = input$legend_title) +
        scale_fill_manual(values = cols_selected, name = "Selected Genes") +
        scale_alpha_manual(values = c(alphas_nonselected, alphas_selected)) +
        scale_size_manual(values = c(sizes_nonselected, sizes_selected)) +
        theme_bw() +
        theme(
          text = element_text(color = input$textlab_color_axis),
          legend.title = element_text(size = input$textlab_size_legend),
          legend.text = element_text(size = input$textlab_size_legend_content),
          plot.title = element_text(size = input$textlab_size_title),
          plot.subtitle = element_text(size = input$textlab_size_subtitle),
          axis.title = element_text(size = input$textlab_size_axis),
          axis.text = element_text(size = input$textlab_size_axis),
          legend.position = legendPosition
        ) +
        labs(x = input$text_xaxis, y = input$text_yaxis, title = input$title, subtitle = input$subtitle) +
        guides(
          color = guide_legend(override.aes = list(alpha = alphas_nonselected[1], size = sizes_nonselected[1])),
          fill = "none",
          alpha = "none",
          size = "none"
        )
    }
    
    # Label specific gene symbols
    if (!is.null(input$label_gene_symbols)) {
      selected_genes <- unlist(strsplit(input$label_gene_symbols, ",\\s*"))
      p <- p + geom_label_repel(data = df %>% filter(symbol %in% selected_genes), 
                                aes(label = symbol, color = colordiff), 
                                size = (input$text_size_pointlabs / .pt), 
                                color = input$text_color_pointlabs, 
                                max.overlaps = Inf, nudge_x = 0.5)
    }
    
    # Label genes matching partial string
    if (!is.null(input$partial_match) && input$partial_match != '') {
      partial_patterns <- unlist(strsplit(input$partial_match, ",\\s*"))
      combined_pattern <- paste0(partial_patterns, collapse = "|")
      partial_genes <- df %>% filter(str_detect(symbol, combined_pattern))
      p <- p + geom_label_repel(data = partial_genes, 
                                aes(label = symbol, color = colordiff), 
                                size = (input$text_size_pointlabs / .pt), 
                                color = input$text_color_pointlabs, 
                                max.overlaps = Inf, nudge_x = 0.5)
    }
    
    # Label top N genes by absolute log2fold_change
    if(input$top_genes > 0){
      top <- df %>% top_n(input$top_genes, wt = abs(log2fold_change))
      p <- p + geom_label_repel(data = top, 
                                aes(label = symbol, color = colordiff), 
                                size = (input$text_size_pointlabs / .pt), 
                                color = input$text_color_pointlabs, 
                                max.overlaps = Inf, nudge_x = 0.5)
    }
    
    width_px_max = input$dimension[1]
    height_px_max = input$dimension[2]
    ratio_export = input$width / input$height
    
    new_height_px = height_px_max
    new_width_px = height_px_max * ratio_export
    if (new_width_px > width_px_max) {
      new_width_px = width_px_max
      new_height_px = width_px_max / ratio_export
    }
    
    # Save the plot to a temporary file
    tmpfile <- tempfile(fileext = ".png")
    ggsave(tmpfile, plot = p, width = reactive({
      width_units <- input$width
      if (input$units == "in") {
        width_units
      } else if (input$units == "cm") {
        width_units / 2.54
      } else if (input$units == "mm") {
        width_units / 25.4
      } else {
        width_units / input$dpi
      }
    })(), height = reactive({
      height_units <- input$height
      if (input$units == "in") {
        height_units
      } else if (input$units == "cm") {
        height_units / 2.54
      } else if (input$units == "mm") {
        height_units / 25.4
      } else {
        height_units / input$dpi
      }
    })(), dpi = input$dpi)
    
    # Return the temporary file path and dimensions
    list(
      src = tmpfile,
      contentType = "image/png",
      width = reactive({
        width_units <- input$width
        width_pixels <- if (input$units == "in") {
          width_units * input$dpi
        } else if (input$units == "cm") {
          width_units * input$dpi / 2.54
        } else if (input$units == "mm") {
          width_units * input$dpi / 25.4
        } else {
          width_units
        }
        min(width_pixels, new_width_px * input$zoom) #min width of window/main panel, calcutlate this and put in, add input "zoom"
      })(),
      height = reactive({
        height_units <- input$height
        height_pixels <- if (input$units == "in") {
          height_units * input$dpi
        } else if (input$units == "cm") {
          height_units * input$dpi / 2.54
        } else if (input$units == "mm") {
          height_units * input$dpi / 25.4
        } else {
          height_units
        }
        min(height_pixels, new_height_px* input$zoom)
      })()
    )
  }, deleteFile = TRUE)
  
  
  
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$file_name, ".", input$file_type, sep = "")
    },
    content = function(file) {
      if (input$colormodel == "CMYK") {
        if (input$file_type == "pdf") {
          pdf(file, width = input$width, height = input$height, colormodel = "cmyk") # nolint: line_length_linter.
        } else if (input$file_type == "eps") {
          postscript(file, width = input$width, height = input$height, colormodel = "cmyk")
        } else if (input$file_type == "tiff") {
          # Generate a temporary PNG in RGB colorspace
          temp_file <- tempfile(fileext = ".png")
          
          ggsave(temp_file, plot = last_plot(), width = input$width, height = input$height, dpi = input$dpi)
          
          # Convert the PNG to a CMYK TIFF using magick
          img <- magick::image_read(temp_file)
          img <- magick::image_convert(img, colorspace = "cmyk")
          magick::image_write(img, path = file, format = "tiff")
        }
        print(last_plot())
        dev.off()
      } else {
        ggsave(file, plot = last_plot(), device = input$file_type, width = input$width, height = input$height, units = input$units, dpi = input$dpi) # nolint
      }
    }
  )
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
