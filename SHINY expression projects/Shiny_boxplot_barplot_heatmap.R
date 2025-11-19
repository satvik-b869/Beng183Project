library(shiny)
library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)
library(colorspace)
library(shinyWidgets)
library(colourpicker)
library(RColorBrewer)
library(bslib)
library(shinyjqui)
library(ggplot2)
library("gplots")
library(rlang)
library(magick)
library(shinyjqui)
library(sortable)
#library(pheatmap)


#have somethign where people can order how they want
#change the color to match the website (heatmap)
#have only up to 5 genes for title...
#fill but make opaque the box plot

ui <- fluidPage(
  theme = bs_theme(font_scale = 0.7),
  tags$style("font: 50px 'Arial', sans-serif;"),
  tags$style("[type ='text/css']  {font-size:8px;height:20px;}"),
  
  titlePanel("Gene Expression Visualization: Boxplots, Bar Graphs, and Heatmaps"),
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$script("
      var dimension = [0, 0];
      $(document).on('shiny:connected', function(e) {
        var plot = $('#plot'); // Ensure the ID matches your plotOutput ID
        dimension[0] = plot.width();
        dimension[1] = plot.height();
        Shiny.onInputChange('dimension', dimension);
      });

      $(window).resize(function(e) {
        var plot = $('#plot'); // Ensure the ID matches your plotOutput ID
        dimension[0] = plot.width();
        dimension[1] = plot.height();
        Shiny.onInputChange('dimension', dimension);
      });

      var updateHeight = function() {
        var plot = $('#plot'); // Ensure the ID matches your plotOutput ID
        var height = $(window).height() - plot.offset().top - 50; // Adjust 50 for margin
        plot.css('height', height + 'px');
        Shiny.onInputChange('plot_height', height);
      };

      $(document).on('shiny:connected', function(e) {
        updateHeight();
      });

      $(window).resize(function(e) {
        updateHeight();
      });
    ")),
      style = "height: 90vh; overflow-y: auto;", 
      width = 3,
      tags$style("[type = 'number']  {font-size:14px;height:20px;}"),
      tags$style("[type = 'text']  {font-size:14px;height:20px;}"),
      
      fileInput("dataFile", "Upload data/normalized counts file (csv/txt):",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt")),
      fileInput("attributeFile", "Upload attributes file (csv/txt):",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".txt")),
      checkboxInput("advancedOptions", "Show advanced Options", FALSE),
      #fileInput("loadSettings", "Load Saved Settings:", accept = ".csv"),
      accordion(
        accordion_panel(
          "Primary controls",
          radioButtons("plot_type", "Select Plot Type:", choices = c("Boxplot", "Bar Graph", "Heatmap")),
          
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",  # Hide for Heatmap
            checkboxInput("remove_x_axis_labels", "Remove X-axis Tick Labels", FALSE),
          ),
          
          # Checkbox for error bars, shown only for Bar Graph
          conditionalPanel(
            condition = "input.plot_type == 'Bar Graph'",
            checkboxInput("include_error_bars", "Include Error Bars", FALSE)
          ),
          
          selectizeInput("selected_gene", "Select Genes:", choices = NULL, multiple = TRUE),
          uiOutput("attributeColumnSelector"),
          uiOutput("attributeValueSelector"),
          uiOutput("groupByTimeCountUI"),
          uiOutput("colorPicker"),
          
          # Conditional panel for groupByColumnSelector, which is shown for Boxplot and Bar Graph
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",
            uiOutput("groupByColumnSelector"), # Only show "Group by" if not Heatmap
          ),
          
          # Conditional panel for value_order_ui, which is shown only for Boxplot
          conditionalPanel(
            condition = "input.plot_type == 'Boxplot'",
            uiOutput("value_order_ui")  # Dynamically generated UI for order selection
          ),
          
          # Conditional panel for value_order_ui_barplot, which is shown only for Bar Graph
          conditionalPanel(
            condition = "input.plot_type == 'Bar Graph'",
            uiOutput("value_order_ui_barplot")
          ),
          
          # Conditional panel for Heatmap-specific controls
          conditionalPanel(
            condition = "input.plot_type == 'Heatmap'",
            selectInput("color_scheme", "Color Scheme:", choices = rownames(brewer.pal.info)),
            checkboxInput("cluster_rows", "Cluster Rows", TRUE),
            checkboxInput("cluster_columns", "Cluster Columns", TRUE)
          )
        ),
        accordion_panel(
          "Axis controls",
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",  # Hide for Heatmap
            numericInput("y_min", "Y-axis minimum value:", 0, min = 0),
            numericInput("y_max", "Y-axis maximum value:", 20),
            conditionalPanel(
              condition = "input.advancedOptions == true",
              numericInput("breaky", "Y-axis Tick Interval:", 10)
            ),
          ),
          textInput("text_xaxis", "X-axis label:", value = ""),
          textInput("text_yaxis", "Y-axis label:", value = "Log2 Normalized Counts"),
          numericInput("textlab_size_axis", "Text size for labels on axis:", 14)
        ),
        accordion_panel(
          "Title and Legend controls",
          textInput("title", "Plot title:", value = ""),
          textInput("subtitle", "Plot subtitle:", value = ""),
          numericInput("textlab_size_title", "Text size for Title:", 23),
          numericInput("textlab_size_subtitle", "Text size for subtitle:", 18),
          textInput("legend_title", "Legend title:", value = "Legend"),
          #uiOutput("legendPositionUI"),
          conditionalPanel(
            condition = "input.plot_type == 'Heatmap'",
            selectInput("legendPositionHeat", "Legend Position (Heatmap):",
                        choices = c("topright", "bottomleft","none")),
          ),
          
          conditionalPanel(
            condition = "input.plot_type != 'Heatmap'",
            selectInput("legendPosition", "Legend Position:",
                        choices = c("right", "left", "top", "bottom","none")),
          ),
          numericInput("textlab_size_legend", "Text size for legend:", 14),
          numericInput("textlab_size_legend_content", "Text size for legend contents:", 14),
          colourInput("textlab_color_axis", "Text color for labels:", value = "black")
        ),
        accordion_panel(
          "Export plot",
          textInput("file_name", "Output file name for the plot:", value = ""),
          radioButtons("colormodel", "Color Model", choices = c("RGB", "CMYK")),
          uiOutput("file_type_ui"),
          numericInput("width", "Width of plot:", 11),
          numericInput("height", "Height of plot:", 8),
          selectInput("units", "Units :", choices = c("in", "cm", "mm", "px")),
          conditionalPanel(
            condition = "input.advancedOptions == true",
            numericInput("dpi", "DPI:", 300),
            numericInput("zoom", "Zoom:", 1),
            
          ),
          downloadButton("downloadPlot", "Download Plot")
        ),
        accordion_panel(
          "Save/Load Settings",
          
          # Add a description to explain the functionality
          p("This feature allows you to save all your favorite options, so you can easily reload them in the future. It will not save the file inputted, but will save all modifications."),
          
          # File input for loading saved settings
          fileInput("loadSettings", 
                    label = "Load Settings from a File:", 
                    accept = ".csv"),
          
          # Text input for naming the saved settings file
          textInput("saveFilename", 
                    label = "Filename to Save Current Settings:", 
                    value = "settings.csv"),
          
          # Button to save current settings
          actionButton("saveSettings", 
                       label = "Save Current Settings to File")
        )
      )
    ),
    mainPanel(
      div(
        id = "plotContainer",
        style = "height: calc(100vh - 150px); overflow-y: auto;",
        imageOutput("plot", height = "100%")
      )
    )
    
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2)  # 30 MB
  
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
    
    # Update the attributeColumn first
    if ("attributeColumn" %in% loadedData$variable) {
      attribute_column_value <- loadedData$value[loadedData$variable == "attributeColumn"]
      updateSelectInput(session, "attributeColumn", selected = attribute_column_value)
    }
    
    # Delay the update of selectedValues to ensure attributeColumn has been updated
    observe({
      req(input$attributeColumn)
      selected_values <- loadedData$value[loadedData$variable == "selectedValues"]
      if (length(selected_values) > 0) {
        selected_values <- strsplit(selected_values, ",")[[1]]
        updateCheckboxGroupInput(session, "selectedValues", selected = selected_values)
      }
    })
    
    # Delay the update of selected_gene to ensure data and attributes are fully initialized
    observe({
      req(input$attributeColumn, input$selectedValues)
      selected_genes <- loadedData$value[loadedData$variable == "selected_gene"]
      if (length(selected_genes) > 0) {
        selected_genes <- strsplit(selected_genes, ",")[[1]]
        updateSelectizeInput(session, "selected_gene", selected = selected_genes)
      }
    })
    
    # Update the rest of the inputs
    lapply(seq_len(nrow(loadedData)), function(i) {
      var_name <- loadedData$variable[i]
      var_value <- loadedData$value[i]
      
      if (var_name != "attributeColumn" && var_name != "selectedValues" && var_name != "selected_gene" && var_name %in% names(input)) {
        if (is.numeric(input[[var_name]])) {
          updateNumericInput(session, var_name, value = as.numeric(var_value))
        } else if (is.character(input[[var_name]])) {
          updateTextInput(session, var_name, value = var_value)
        } else if (is.logical(input[[var_name]])) {
          updateCheckboxInput(session, var_name, value = as.logical(var_value))
        } else if (is.factor(input[[var_name]])) {
          updateSelectInput(session, var_name, selected = var_value)
        }
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
  
  
  #clean and read expression matrix 
  data <- reactive({
    req(input$dataFile)
    ext <- tools::file_ext(input$dataFile$datapath)
    
    # Read CSV or TXT
    df <- if (ext == "txt") {
      read_delim(input$dataFile$datapath)
    } else if (ext == "csv") {
      read_csv(input$dataFile$datapath)
    } else {
      stop("Unsupported file type")
    }
    
    # Clean column names (spaces -> _, lower case, etc.)
    df <- janitor::clean_names(df)
    
    # Try to detect the gene column automatically
    gene_col <- names(df)[grepl("symbol|gene|gene_id|hgnc|ensembl|id",
                                names(df), ignore.case = TRUE)][1]
    
    # Fallback: assume first column is the gene column
    if (is.na(gene_col) || gene_col == "") {
      gene_col <- names(df)[1]
    }
    
    # Rename chosen gene column to 'Symbol' so the rest of the code can stay the same
    df <- df %>%
      rename(Symbol = all_of(gene_col)) %>%
      mutate(across(where(is.numeric), round, 4))
    
    df
  })
  
  #attribute df creation
  attributes <- reactive({
    req(input$attributeFile)
    ext <- tools::file_ext(input$attributeFile$datapath)
    
    attri <- if (ext == "txt") {
      readr::read_delim(input$attributeFile$datapath)
    } else if (ext == "csv") {
      readr::read_csv(input$attributeFile$datapath)
    } else {
      stop("Unsupported file type")
    }
    
    # Clean column names
    attri <- janitor::clean_names(attri)
    
    # Guess which column holds sample IDs
    sample_col <- names(attri)[grepl("sample|sample_name|run|accession|library",
                                     names(attri), ignore.case = TRUE)][1]
    
    if (is.na(sample_col) || sample_col == "") {
      sample_col <- names(attri)[1]
    }
    
    # Rename that column to 'sample_name'
    attri <- attri %>%
      dplyr::rename(sample_name = !!sample_col)
    
    # 🔑 Make sample_name values match the cleaned colnames from data():
    # e.g., "Sample1" -> "sample1", "Sample 1" -> "sample_1"
    attri <- attri %>%
      dplyr::mutate(sample_name = janitor::make_clean_names(as.character(sample_name)))
    
    attri
  })
  
  
  
  #selecting the gene
  observe({
    req(data())
    genes <- unique(data()$Symbol)
    names(genes) <- genes
    updateSelectizeInput(session, "selected_gene", choices = genes, server = TRUE)
  })
  
  # Updating the title to the gene(s) selected with a limit of 5 genes
  observeEvent(input$selected_gene, {
    selected_genes <- input$selected_gene
    
    if (length(selected_genes) > 5) {
      displayed_genes <- paste(selected_genes[1:5], collapse = ", ")
      title_text <- paste("Gene Expression for", displayed_genes, "...")
    } else {
      title_text <- paste("Gene Expression for", paste(selected_genes, collapse = ", "))
    }
    
    updateTextInput(session, "title", value = title_text)
  })
  
  #only display attribute columns with 2 or more distinct values
  observe({
    req(attributes())
    
    # Filter columns with at least 2 distinct values
    valid_columns <- names(attributes())[sapply(attributes(), function(col) length(unique(col)) >= 2)]
    
    updateSelectInput(session, "attributeColumn", choices = valid_columns)
  })
  
  #choices for the attribute column
  output$attributeColumnSelector <- renderUI({
    req(attributes())
    selectInput("attributeColumn", "Select Attribute Column:", choices = names(attributes()))
  })
  
  #selecting the attribute value
  output$attributeValueSelector <- renderUI({
    req(input$attributeColumn)
    attribute_values <- unique(attributes()[[input$attributeColumn]])
    
    if (length(attribute_values) > 6) {
      div(
        style = "height: 150px; overflow-y: auto;",
        checkboxGroupInput("selectedValues", "Select Attribute Values:", choices = attribute_values)
      )
    } else {
      checkboxGroupInput("selectedValues", "Select Attribute Values:", choices = attribute_values)
    }
  })
  
  #Group By selector
  output$groupByColumnSelector <- renderUI({
    req(attributes(), input$attributeColumn)
    
    # Filter columns with at least 2 distinct values and exclude the selected attribute column
    valid_columns <- names(attributes())[sapply(attributes(), function(col) length(unique(col)) >= 2)]
    
    # Remove the selected attribute column from the list of valid columns
    valid_columns <- setdiff(valid_columns, input$attributeColumn)
    
    selectInput("groupByColumn", "Group by:", choices = c("", valid_columns), selected = "")
  })
  
  #updates the attribute values based on the current selected attribute column
  observeEvent(input$attributeColumn, {
    updateCheckboxGroupInput(session, "selectedValues", choices = unique(attributes()[[input$attributeColumn]]))
    updateCheckboxGroupInput(session, "selectedValues", selected = character(0))
  })
  
  #update x-axis text to match selected column
  observe({
    updateTextInput(session, "text_xaxis", value = input$attributeColumn)
  })
  
  #plot data IMPORTANT, this is the df that is actually used
  plot_data <- reactive({
    req(data(), attributes(), input$selected_gene, input$selectedValues)
    
    attribute_column <- input$attributeColumn
    selected_samples <- attributes()$sample_name[attributes()[[attribute_column]] %in% input$selectedValues]
    selected_attributes <- attributes() %>% filter(sample_name %in% selected_samples)
    
    if (input$groupByColumn != "") {
      selected_attributes <- selected_attributes %>%
        mutate(Group = paste(selected_attributes[[attribute_column]], selected_attributes[[input$groupByColumn]], sep = "_")) %>%
        arrange(selected_attributes[[attribute_column]], selected_attributes[[input$groupByColumn]])
    } else {
      selected_attributes <- selected_attributes %>%
        mutate(Group = selected_attributes[[attribute_column]])
    }
    
    selected_data <- data() %>% filter(Symbol %in% input$selected_gene)
    genes <- input$selected_gene
    
    long_data <- selected_data %>%
      pivot_longer(cols = -Symbol, names_to = "sample_name", values_to = "NormalizedCounts") %>%
      left_join(selected_attributes, by = "sample_name") 
    
    # Create Broad_Group by removing the time course information from Group
    long_data <- long_data %>%
      mutate(Broad_Group = sub("_\\d+ hr", "", Group))  # Adjust regex to fit your time course format
    
    long_data <- long_data %>%
      mutate(Gene_Attribute = if (length(genes) >= 2) {
        paste(Group, Symbol, sep = " - ")
      } else {
        Group  # Use only Group when one gene is selected
      })
    
    # Create the Sample_Attribute column for the bar plot
    long_data <- long_data %>%
      mutate(Sample_Attribute = paste(sample_name, Symbol, sep = " - "))  # Concatenating sample_name and Symbol
    
    # Filter out rows with NA in the 'Gene_Attribute' or 'Group' columns
    long_data <- long_data %>%
      filter(!is.na(Gene_Attribute) & !is.na(Group))
    
    shiny::validate(
      need(nrow(long_data) > 0, "No data available after filtering. Please check your data.")
    )
    
    long_data
  })
  
  #calculating y min and max
  observeEvent(c(input$attributeColumn, input$selectedValues, input$selected_gene), {
    long_data <- plot_data()
    if (!is.null(long_data) && nrow(long_data) > 0 && !all(is.na(long_data$NormalizedCounts))) {
      y_min_value <- floor(min(long_data$NormalizedCounts, na.rm = TRUE))
      y_max_value <- ceiling(max(long_data$NormalizedCounts, na.rm = TRUE))
      
      updateNumericInput(session, "y_min", value = y_min_value)
      updateNumericInput(session, "y_max", value = y_max_value)
    }
  })
  
  
  # Color picker for boxplots, bar graphs, and heatmaps
  output$colorPicker <- renderUI({
    req(plot_data(), input$attributeColumn)
    long_data <- plot_data()
    
    # Get unique group values
    group_values <- unique(long_data$Broad_Group)
    
    # Ensure that n is at least 3
    n_colors <- max(3, length(group_values))
    
    # Define a base color palette, ensuring it has enough colors
    base_palette <- brewer.pal(min(n_colors, 9), "Set1")  # Get up to 9 colors from Set1
    
    # Generate the required number of colors using colorRampPalette
    color_palette <- colorRampPalette(base_palette)(n_colors)
    
    # Create color pickers for each group
    lapply(seq_along(group_values), function(i) {
      group_value <- group_values[i]
      sanitized_name <- make.names(group_value)
      colourInput(
        paste0("color_", sanitized_name), 
        label = paste("Color for", group_value), 
        value = color_palette[i]
      )
    })
  })
  
  
  
  # In the server function, add this to update the labels based on the selected plot type
  
  observe({
    if (input$plot_type == "Heatmap") {
      updateTextInput(session, "text_xaxis", value = "Samples")
      updateTextInput(session, "text_yaxis", value = "Genes")
    }
  })
  
  # # UI for selecting the legend position
  # output$legendPositionUI <- renderUI({
  #   if (input$plot_type == "Heatmap") {
  #     selectInput("legendPosition", "Legend Position:", choices = c("topright", "bottomleft"))
  #   } else {
  #     selectInput("legendPosition", "Legend Position:", choices = c("right", "left", "top", "bottom"))
  #   }
  # })
  # 
  # observeEvent(input$plot_type, {
  #   if (input$plot_type == "Heatmap") {
  #     updateSelectInput(session, "legendPosition", selected = "topright")
  #   } else {
  #     # Optionally, set a default legend position for other plot types
  #     updateSelectInput(session, "legendPosition", selected = "right")
  #   }
  # })
  # 
  
  
  # Generate UI for value order selection based on Sample_Attribute for Bar Graph
  output$value_order_ui_barplot <- renderUI({
    req(plot_data(), input$plot_type == "Bar Graph")
    
    # Retrieve the filtered data based on the selected gene
    long_data <- plot_data()
    
    # Extract the unique Sample_Attribute values
    sample_values <- unique(long_data %>% pull(Sample_Attribute))
    
    tags$div(
      class = "accordion", id = "accordionBarPlot",
      tags$div(
        class = "accordion-item",
        tags$h2(
          class = "accordion-header", id = "headingBarPlot",
          tags$button(
            class = "accordion-button collapsed", type = "button", 
            "data-bs-toggle" = "collapse", "data-bs-target" = "#collapseBarPlot",
            "aria-expanded" = "false", "aria-controls" = "collapseBarPlot",
            "Click to reorder samples"
          )
        ),
        tags$div(
          id = "collapseBarPlot", class = "accordion-collapse collapse", 
          "aria-labelledby" = "headingBarPlot", "data-bs-parent" = "#accordionBarPlot",
          tags$div(
            class = "accordion-body",
            rank_list(
              input_id = "value_order_barplot",
              text = "Drag to reorder samples",
              labels = sample_values
            )
          )
        )
      )
    )
  })
  
  
  
  #Generate UI for value order selection based on chosen attribute
  output$value_order_ui <- renderUI({
    req(plot_data(), input$attributeColumn)
    
    # Retrieve the filtered data based on the selected gene
    long_data <- plot_data()
    
    # Extract the unique Gene_Attribute values for the selected gene
    attribute_values <- unique(long_data %>% pull(Gene_Attribute))
    
    tags$div(
      class = "accordion", id = "accordionExample",
      tags$div(
        class = "accordion-item",
        tags$h2(
          class = "accordion-header", id = "headingOne",
          tags$button(
            class = "accordion-button collapsed", type = "button", 
            "data-bs-toggle" = "collapse", "data-bs-target" = "#collapseOne",
            "aria-expanded" = "false", "aria-controls" = "collapseOne",
            "Click to reorder values"
          )
        ),
        tags$div(
          id = "collapseOne", class = "accordion-collapse collapse", 
          "aria-labelledby" = "headingOne", "data-bs-parent" = "#accordionExample",
          tags$div(
            class = "accordion-body",
            rank_list(
              input_id = "value_order",
              text = "Drag to reorder values",
              labels = attribute_values
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$plot_type, {
    
    if(input$plot_type == "Boxplot"){
      updateTextInput(session, "file_name", value = "Boxplot")
    }else if (input$plot_type == "Bar Graph"){
      updateTextInput(session, "file_name", value = "Bar Graph")
    }else{
      updateTextInput(session, "file_name", value = "Heatmap")
    }
    
  })
  
  
  
  #generatin the image for the mainpanel
  output$plot <- renderImage({
    long_data <- plot_data()
    legendPosition <- input$legendPosition
    if (nrow(long_data) == 0) {
      showNotification("No data available for plotting. Check your data selection or filtering criteria.", type = "error")
      return(NULL)
    }
    
    if (all(is.na(long_data$NormalizedCounts))) {
      showNotification("NormalizedCounts contains only NA values. Please check the data for the selected gene.", type = "error")
      return(NULL)
    }
    
    if (input$plot_type == "Heatmap") {
      
      # Combine all data associated with the same Symbol
      heatmap_data <- plot_data() %>%
        group_by(Symbol, sample_name) %>%
        summarise(NormalizedCounts = mean(NormalizedCounts, na.rm = TRUE)) %>%
        spread(key = "sample_name", value = NormalizedCounts)
      
      # Convert to a proper data frame and ensure it's numeric
      heatmap_data <- as.data.frame(heatmap_data)
      rownames(heatmap_data) <- heatmap_data$Symbol
      heatmap_data <- heatmap_data[, -1]  # Remove the 'Symbol' column
      
      view(heatmap_data)
      
      # Check and handle NA values
      if (any(is.na(heatmap_data))) {
        showNotification("NA values detected in heatmap data. Handling NA values by replacing them with 0.", type = "warning")
        heatmap_data[is.na(heatmap_data)] <- 0
      }
      
      # Create annotation data frame for the selected attribute values
      annotation_col <- plot_data() %>%
        select(sample_name, Group) %>%
        unique() %>%
        column_to_rownames(var = "sample_name")  # Set sample_name as row names
      
      
      
      # Ensure the order of columns in heatmap_data matches annotation_col
      annotation_col <- annotation_col[match(colnames(heatmap_data), rownames(annotation_col)), , drop = FALSE]
      
      # Ensure annotation_col is a factor or appropriate type for annotation
      
      annotation_col$Group <- factor(annotation_col$Group)
      view(annotation_col)
      
      # # Define colors for the heatmap
      # color_palette <- colorRampPalette(brewer.pal(9, input$color_scheme))(100)
      
      # Define colors for the heatmap
      color_palette <- colorRampPalette(brewer.pal(9, input$color_scheme))(100)
      
      
      # Extract unique Broad_Group values
      broad_groups <- unique(long_data$Broad_Group)
      # Retrieve selected colors from the color pickers, based on Broad_Group
      selected_colors <- sapply(broad_groups, function(Broad_Group) {
        input[[paste0("color_", make.names(Broad_Group))]]
      })
      names(selected_colors) <- broad_groups
      
      ColSideColors <- selected_colors[as.character(annotation_col$Group)]
      
      # Save the heatmap to a temporary file
      tmpfile <- tempfile(fileext = ".png")
      png(tmpfile, width = input$width, height = input$height, units = input$units, res = input$dpi)  # Open a PNG device to save the plot
      
      # Apply size settings
      title_size <- input$textlab_size_title
      subtitle_size <- input$textlab_size_subtitle
      legend_title_size <- input$textlab_size_legend
      legend_content_size <- input$textlab_size_legend_content
      axis_label_size <- input$textlab_size_axis
      
      # lwid=c(0.5,5) #make column of dendrogram and key very small and other colum very big 
      # lhei=c(3,3) #make row of key and other dendrogram very small and other row big. 
      
      heatmap.2(
        as.matrix(heatmap_data),
        Colv = if(input$cluster_columns) TRUE else FALSE,  # Cluster columns based on user input
        Rowv = if(input$cluster_rows) TRUE else FALSE,     # Cluster rows based on user input
        col = color_palette,               # Use the color palette
        ColSideColors = ColSideColors,     # Color columns based on groups
        labRow = rownames(heatmap_data),   # Label rows with gene symbols
        labCol = colnames(heatmap_data),   # Label columns with sample names
        scale = "none",                    # No scaling applied
        margins = c(10, 13),               # Adjust margins for better readability
        cexCol = axis_label_size / 14,     # Adjust the size of the column labels
        cexRow = axis_label_size / 14,     # Adjust the size of the row labels
        trace = "none",                    # Remove traces inside the heatmap cells
        xlab = input$text_xaxis,
        ylab = input$text_yaxis,
        main = NULL,
        # lwid = lwid,
        # lhei = lhei
      )
      
      # # Manually add the title with a specific size
      # title(
      #   main = input$title,
      #   cex.main = input$textlab_size_title / 14,  # Adjust the size of the title
      #   line = 2
      # )
      
      if (input$legendPositionHeat != "none") {
        legend(
          x = input$legendPositionHeat,
          legend = names(selected_colors), # Labels for each group
          col = selected_colors, # Colors for each group
          pch = 15, # Square symbols
          pt.cex = 1.5, # Size of the symbols
          cex = legend_content_size / 14, # Size of the legend text
          title = input$legend_title, # Title of the legend
          title.cex = legend_title_size / 14 # Adjust legend title size
        )
      }
      
      
      #   bty = "n", # No box around the legend
      dev.off()  # Close the PNG devices
      
      ###Just creating the correct temp file
      
      
      
      # Determine the dimensions for the output image
      width_px_max <- input$dimension[1]
      height_px_max <- input$dimension[2]
      ratio_export <- input$width / input$height
      
      new_height_px <- height_px_max
      new_width_px <- height_px_max * ratio_export
      if (new_width_px > width_px_max) {
        new_width_px <- width_px_max
        new_height_px <- width_px_max / ratio_export
      }
      
      # Return the temporary file path and dimensions for the heatmap
      return(list(
        src = tmpfile,
        contentType = "image/png",
        width = min(input$width * input$dpi, new_width_px * input$zoom),
        height = min(input$height * input$dpi, new_height_px * input$zoom)
      ))
    }else {
      # Grouping data and calculating stats
      long_data <- long_data %>%
        group_by(Gene_Attribute) %>%
        mutate(
          Q1 = quantile(NormalizedCounts, 0.25, na.rm = TRUE),
          Q3 = quantile(NormalizedCounts, 0.75, na.rm = TRUE),
          Min = min(NormalizedCounts, na.rm = TRUE),
          Max = max(NormalizedCounts, na.rm = TRUE),
          SD = sd(NormalizedCounts, na.rm = TRUE)
        )
      
      
      # Extract unique Broad_Group values
      broad_groups <- unique(long_data$Broad_Group)
      
      # Retrieve selected colors from the color pickers, based on Broad_Group
      selected_colors <- sapply(broad_groups, function(Broad_Group) {
        input[[paste0("color_", make.names(Broad_Group))]]
      })
      names(selected_colors) <- broad_groups
      
      # Apply different ordering and plotting logic based on plot type
      if (input$plot_type == "Boxplot") {
        long_data <- long_data %>%
          arrange(Gene_Attribute, Group)
        # Set the Gene_Attribute column as a factor with levels specified by the user
        long_data$Gene_Attribute <- factor(long_data$Gene_Attribute, levels = input$value_order)
        
        # Generate the plot
        p <- ggplot(data = long_data, aes(x = Gene_Attribute, y = NormalizedCounts, fill = Broad_Group)) +
          geom_boxplot(outlier.shape = NA, color = "darkgray", alpha = 0.75) +
          stat_boxplot(geom = "errorbar", width = 0.5, color = "darkgray") +
          geom_jitter(width = 0.2, size = 1.5, color = "black") +
          labs(x = input$text_xaxis, y = input$text_yaxis, title = input$title, subtitle = input$subtitle, fill = input$legend_title) +
          theme_bw() +
          theme(
            axis.text.x = if (input$remove_x_axis_labels) element_blank() else element_text(angle = 90, hjust = 1),
            axis.ticks.x = if (input$remove_x_axis_labels) element_blank() else element_line(),
            text = element_text(color = input$textlab_color_axis),
            legend.title = element_text(size = input$textlab_size_legend),
            legend.text = element_text(size = input$textlab_size_legend_content),
            plot.title = element_text(size = input$textlab_size_title),
            plot.subtitle = element_text(size = input$textlab_size_subtitle),
            axis.title = element_text(size = input$textlab_size_axis),
            axis.text = element_text(size = input$textlab_size_axis),
            legend.position =  input$legendPosition 
          ) +
          scale_fill_manual(values = selected_colors, name = input$legend_title) +
          guides(fill = guide_legend(title = input$legend_title)) +
          ylim(input$y_min, input$y_max)
        
        # Conditionally apply faceting based on the selected groupByColumn
        if (input$groupByColumn != "") {
          facet_formula <- as.formula(paste(". ~", input$groupByColumn))
          p <- p + facet_wrap(facet_formula, scales = "free")
        }
        
      } else if (input$plot_type == "Bar Graph") {
        long_data <- long_data %>%
          arrange(Sample_Attribute)
        
        # Set the Sample_Attribute column as a factor with levels specified by the user
        long_data$Sample_Attribute <- factor(long_data$Sample_Attribute, levels = input$value_order_barplot)
        
        # Generate the plot
        p <- ggplot(data = long_data, aes(x = Sample_Attribute, y = NormalizedCounts, fill = Broad_Group)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = input$text_xaxis, y = input$text_yaxis, title = input$title, subtitle = input$subtitle, fill = input$legend_title) +
          theme_bw() +
          theme(
            axis.text.x = if (input$remove_x_axis_labels) element_blank() else element_text(angle = 90, hjust = 1),
            axis.ticks.x = if (input$remove_x_axis_labels) element_blank() else element_line(),
            text = element_text(color = input$textlab_color_axis),
            legend.title = element_text(size = input$textlab_size_legend),
            legend.text = element_text(size = input$textlab_size_legend_content),
            plot.title = element_text(size = input$textlab_size_title),
            plot.subtitle = element_text(size = input$textlab_size_subtitle),
            axis.title = element_text(size = input$textlab_size_axis),
            axis.text = element_text(size = input$textlab_size_axis),
            legend.position = input$legendPosition
          ) +
          scale_fill_manual(values = selected_colors, name = input$legend_title) +
          guides(fill = guide_legend(title = input$legend_title)) +
          ylim(0, input$y_max)
        
        # Add error bars if selected
        if (input$include_error_bars) {
          p <- p + geom_errorbar(aes(ymin = NormalizedCounts - SD, ymax = NormalizedCounts + SD),
                                 width = 0.25,  # Adjust the width of the error bars
                                 position = position_dodge(width = 0.9))  # Ensure the error bars are positioned correctly
        }
        
        # Conditionally apply faceting based on the selected groupByColumn
        if (input$groupByColumn != "") {
          facet_formula <- as.formula(paste(". ~", input$groupByColumn))
          p <- p + facet_wrap(facet_formula, scales = "free")
        }
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
      ggsave(tmpfile, plot = p, width = input$width, height = input$height, units = input$units, dpi = input$dpi)
      
      # Return the temporary file path and dimensions
      list(
        src = tmpfile,
        contentType = "image/png",
        width = min(input$width * input$dpi, new_width_px * input$zoom),
        height = min(input$height * input$dpi, new_height_px * input$zoom)
      )
    }
  },deleteFile=TRUE)
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$file_name, ".", input$file_type, sep = "")
    },
    content = function(file) {
      if (input$plot_type == "Heatmap") {
        if (input$colormodel == "CMYK" && input$file_type == "tiff") {
          # Step 1: Generate a TIFF in RGB colorspace
          rgb_tiff_file <- tempfile(fileext = ".tiff")
          tiff(rgb_tiff_file, width = input$width, height = input$height, units = input$units, res = input$dpi)
          
          # Reuse the same logic from the renderImage function to generate the heatmap
          long_data <- plot_data()
          if (nrow(long_data) == 0 || all(is.na(long_data$NormalizedCounts))) {
            return(NULL)
          }
          
          # Combine all data associated with the same Symbol
          heatmap_data <- plot_data() %>%
            group_by(Symbol, sample_name) %>%
            summarise(NormalizedCounts = mean(NormalizedCounts, na.rm = TRUE)) %>%
            spread(key = "sample_name", value = NormalizedCounts)
          
          # Convert to a proper data frame and ensure it's numeric
          heatmap_data <- as.data.frame(heatmap_data)
          rownames(heatmap_data) <- heatmap_data$Symbol
          heatmap_data <- heatmap_data[, -1]  # Remove the 'Symbol' column
          
          view(heatmap_data)
          
          # Check and handle NA values
          if (any(is.na(heatmap_data))) {
            showNotification("NA values detected in heatmap data. Handling NA values by replacing them with 0.", type = "warning")
            heatmap_data[is.na(heatmap_data)] <- 0
          }
          
          # Create annotation data frame for the selected attribute values
          annotation_col <- plot_data() %>%
            select(sample_name, Group) %>%
            unique() %>%
            column_to_rownames(var = "sample_name")  # Set sample_name as row names
          
          
          
          # Ensure the order of columns in heatmap_data matches annotation_col
          annotation_col <- annotation_col[match(colnames(heatmap_data), rownames(annotation_col)), , drop = FALSE]
          
          # Ensure annotation_col is a factor or appropriate type for annotation
          
          annotation_col$Group <- factor(annotation_col$Group)
          view(annotation_col)
          
          # Define colors for the heatmap
          color_palette <- colorRampPalette(brewer.pal(9, input$color_scheme))(100)
          
          # Extract unique Broad_Group values
          broad_groups <- unique(long_data$Broad_Group)
          # Retrieve selected colors from the color pickers, based on Broad_Group
          selected_colors <- sapply(broad_groups, function(Broad_Group) {
            input[[paste0("color_", make.names(Broad_Group))]]
          })
          names(selected_colors) <- broad_groups
          
          ColSideColors <- selected_colors[as.character(annotation_col$Group)]
          
          # Apply size settings
          title_size <- input$textlab_size_title
          subtitle_size <- input$textlab_size_subtitle
          legend_title_size <- input$textlab_size_legend
          legend_content_size <- input$textlab_size_legend_content
          axis_label_size <- input$textlab_size_axis
          
          # lwid=c(0.5,5) #make column of dendrogram and key very small and other colum very big 
          # lhei=c(3,3) #make row of key and other dendrogram very small and other row big. 
          
          heatmap.2(
            as.matrix(heatmap_data),
            Colv = if(input$cluster_columns) TRUE else FALSE,  # Cluster columns based on user input
            Rowv = if(input$cluster_rows) TRUE else FALSE,     # Cluster rows based on user input
            col = color_palette,               # Use the color palette
            ColSideColors = ColSideColors,     # Color columns based on groups
            labRow = rownames(heatmap_data),   # Label rows with gene symbols
            labCol = colnames(heatmap_data),   # Label columns with sample names
            scale = "none",                    # No scaling applied
            margins = c(10, 13),               # Adjust margins for better readability
            cexCol = axis_label_size / 14,     # Adjust the size of the column labels
            cexRow = axis_label_size / 14,     # Adjust the size of the row labels
            trace = "none",                    # Remove traces inside the heatmap cells
            xlab = input$text_xaxis,
            ylab = input$text_yaxis,
            main = NULL,
            # lwid = lwid,
            # lhei = lhei
          )
          
          # # Manually add the title with a specific size
          # title(
          #   main = input$title,
          #   cex.main = input$textlab_size_title / 14,  # Adjust the size of the title
          #   line = 2
          # )
          
          if (input$legendPositionHeat != "none") {
            legend(
              x = input$legendPositionHeat,
              legend = names(selected_colors), # Labels for each group
              col = selected_colors, # Colors for each group
              pch = 15, # Square symbols
              pt.cex = 1.5, # Size of the symbols
              cex = legend_content_size / 14, # Size of the legend text
              title = input$legend_title, # Title of the legend
              title.cex = legend_title_size / 14 # Adjust legend title size
            )
          }
          
          
          #   bty = "n", # No box around the legend
          dev.off()  # Close the PNG devices
          
          # Step 2: Convert the TIFF to CMYK using the magick package
          # Convert the PNG to a CMYK TIFF using magick
          img <- magick::image_read(rgb_tiff_file)
          img <- magick::image_convert(img, colorspace = "cmyk")
          magick::image_write(img, path = file, format = "tiff")
          
          
        } else if (input$colormodel == "CMYK") {
          # Handle other formats like PDF and EPS in CMYK
          if (input$file_type == "pdf") {
            pdf(file, width = input$width, height = input$height, colormodel = "cmyk")
          } else if (input$file_type == "eps") {
            postscript(file, width = input$width, height = input$height, colormodel = "cmyk")
          }
          
          # Reuse the same logic from the renderImage function to generate the heatmap
          long_data <- plot_data()
          if (nrow(long_data) == 0 || all(is.na(long_data$NormalizedCounts))) {
            return(NULL)
          }
          
          # Combine all data associated with the same Symbol
          heatmap_data <- plot_data() %>%
            group_by(Symbol, sample_name) %>%
            summarise(NormalizedCounts = mean(NormalizedCounts, na.rm = TRUE)) %>%
            spread(key = "sample_name", value = NormalizedCounts)
          
          # Convert to a proper data frame and ensure it's numeric
          heatmap_data <- as.data.frame(heatmap_data)
          rownames(heatmap_data) <- heatmap_data$Symbol
          heatmap_data <- heatmap_data[, -1]  # Remove the 'Symbol' column
          
          view(heatmap_data)
          
          # Check and handle NA values
          if (any(is.na(heatmap_data))) {
            showNotification("NA values detected in heatmap data. Handling NA values by replacing them with 0.", type = "warning")
            heatmap_data[is.na(heatmap_data)] <- 0
          }
          
          # Create annotation data frame for the selected attribute values
          annotation_col <- plot_data() %>%
            select(sample_name, Group) %>%
            unique() %>%
            column_to_rownames(var = "sample_name")  # Set sample_name as row names
          
          
          
          # Ensure the order of columns in heatmap_data matches annotation_col
          annotation_col <- annotation_col[match(colnames(heatmap_data), rownames(annotation_col)), , drop = FALSE]
          
          # Ensure annotation_col is a factor or appropriate type for annotation
          
          annotation_col$Group <- factor(annotation_col$Group)
          view(annotation_col)
          
          # Define colors for the heatmap
          color_palette <- colorRampPalette(brewer.pal(9, input$color_scheme))(100)
          
          # Extract unique Broad_Group values
          broad_groups <- unique(long_data$Broad_Group)
          # Retrieve selected colors from the color pickers, based on Broad_Group
          selected_colors <- sapply(broad_groups, function(Broad_Group) {
            input[[paste0("color_", make.names(Broad_Group))]]
          })
          names(selected_colors) <- broad_groups
          
          ColSideColors <- selected_colors[as.character(annotation_col$Group)]
          
          # Save the heatmap to a temporary file
          tmpfile <- tempfile(fileext = ".png")
          png(tmpfile, width = input$width, height = input$height, units = input$units, res = input$dpi)  # Open a PNG device to save the plot
          
          # Apply size settings
          title_size <- input$textlab_size_title
          subtitle_size <- input$textlab_size_subtitle
          legend_title_size <- input$textlab_size_legend
          legend_content_size <- input$textlab_size_legend_content
          axis_label_size <- input$textlab_size_axis
          
          # lwid=c(0.5,5) #make column of dendrogram and key very small and other colum very big 
          # lhei=c(3,3) #make row of key and other dendrogram very small and other row big. 
          
          heatmap.2(
            as.matrix(heatmap_data),
            Colv = if(input$cluster_columns) TRUE else FALSE,  # Cluster columns based on user input
            Rowv = if(input$cluster_rows) TRUE else FALSE,     # Cluster rows based on user input
            col = color_palette,               # Use the color palette
            ColSideColors = ColSideColors,     # Color columns based on groups
            labRow = rownames(heatmap_data),   # Label rows with gene symbols
            labCol = colnames(heatmap_data),   # Label columns with sample names
            scale = "none",                    # No scaling applied
            margins = c(10, 13),               # Adjust margins for better readability
            cexCol = axis_label_size / 14,     # Adjust the size of the column labels
            cexRow = axis_label_size / 14,     # Adjust the size of the row labels
            trace = "none",                    # Remove traces inside the heatmap cells
            xlab = input$text_xaxis,
            ylab = input$text_yaxis,
            main = NULL,
            # lwid = lwid,
            # lhei = lhei
          )
          
          # # Manually add the title with a specific size
          # title(
          #   main = input$title,
          #   cex.main = input$textlab_size_title / 14,  # Adjust the size of the title
          #   line = 2
          # )
          
          if (input$legendPositionHeat != "none") {
            legend(
              x = input$legendPositionHeat,
              legend = names(selected_colors), # Labels for each group
              col = selected_colors, # Colors for each group
              pch = 15, # Square symbols
              pt.cex = 1.5, # Size of the symbols
              cex = legend_content_size / 14, # Size of the legend text
              title = input$legend_title, # Title of the legend
              title.cex = legend_title_size / 14 # Adjust legend title size
            )
          }
          
          
          #   bty = "n", # No box around the legend
          dev.off()  # Close the PNG devices
          
        } else {
          if (input$file_type == "png") {
            png(file, width = input$width, height = input$height, units = input$units, res = input$dpi)
          } else if (input$file_type == "jpeg") {
            jpeg(file, width = input$width, height = input$height, units = input$units, res = input$dpi)
          } else if (input$file_type == "tiff") {
            tiff(file, width = input$width, height = input$height, units = input$units, res = input$dpi)
          } else if (input$file_type == "pdf") {
            pdf(file, width = input$width, height = input$height)
          } else if (input$file_type == "eps") {
            postscript(file, width = input$width, height = input$height)
          } else if (input$file_type == "svg") {
            Cairo::CairoSVG(file, width = input$width, height = input$height)
          } else if (input$file_type == "ps") {
            postscript(file, width = input$width, height = input$height)
          } else if (input$file_type == "wmf") {
            win.metafile(file, width = input$width, height = input$height)
          }
          
          # Reuse the same logic from the renderImage function to generate the heatmap
          long_data <- plot_data()
          if (nrow(long_data) == 0 || all(is.na(long_data$NormalizedCounts))) {
            return(NULL)
          }
          
          # Combine all data associated with the same Symbol
          heatmap_data <- plot_data() %>%
            group_by(Symbol, sample_name) %>%
            summarise(NormalizedCounts = mean(NormalizedCounts, na.rm = TRUE)) %>%
            spread(key = "sample_name", value = NormalizedCounts)
          
          # Convert to a proper data frame and ensure it's numeric
          heatmap_data <- as.data.frame(heatmap_data)
          rownames(heatmap_data) <- heatmap_data$Symbol
          heatmap_data <- heatmap_data[, -1]  # Remove the 'Symbol' column
          
          view(heatmap_data)
          
          # Check and handle NA values
          if (any(is.na(heatmap_data))) {
            showNotification("NA values detected in heatmap data. Handling NA values by replacing them with 0.", type = "warning")
            heatmap_data[is.na(heatmap_data)] <- 0
          }
          
          # Create annotation data frame for the selected attribute values
          annotation_col <- plot_data() %>%
            select(sample_name, Group) %>%
            unique() %>%
            column_to_rownames(var = "sample_name")  # Set sample_name as row names
          
          
          
          # Ensure the order of columns in heatmap_data matches annotation_col
          annotation_col <- annotation_col[match(colnames(heatmap_data), rownames(annotation_col)), , drop = FALSE]
          
          # Ensure annotation_col is a factor or appropriate type for annotation
          
          annotation_col$Group <- factor(annotation_col$Group)
          view(annotation_col)
          
          # Define colors for the heatmap
          color_palette <- colorRampPalette(brewer.pal(9, input$color_scheme))(100)
          
          # Extract unique Broad_Group values
          broad_groups <- unique(long_data$Broad_Group)
          # Retrieve selected colors from the color pickers, based on Broad_Group
          selected_colors <- sapply(broad_groups, function(Broad_Group) {
            input[[paste0("color_", make.names(Broad_Group))]]
          })
          names(selected_colors) <- broad_groups
          
          ColSideColors <- selected_colors[as.character(annotation_col$Group)]
          
          # # Save the heatmap to a temporary file
          # tmpfile <- tempfile(fileext = ".png")
          # png(tmpfile, width = input$width, height = input$height, units = input$units, res = input$dpi)  # Open a PNG device to save the plot
          
          # Apply size settings
          title_size <- input$textlab_size_title
          subtitle_size <- input$textlab_size_subtitle
          legend_title_size <- input$textlab_size_legend
          legend_content_size <- input$textlab_size_legend_content
          axis_label_size <- input$textlab_size_axis
          
          # lwid=c(0.5,5) #make column of dendrogram and key very small and other colum very big 
          # lhei=c(3,3) #make row of key and other dendrogram very small and other row big. 
          
          heatmap.2(
            as.matrix(heatmap_data),
            Colv = if(input$cluster_columns) TRUE else FALSE,  # Cluster columns based on user input
            Rowv = if(input$cluster_rows) TRUE else FALSE,     # Cluster rows based on user input
            col = color_palette,               # Use the color palette
            ColSideColors = ColSideColors,     # Color columns based on groups
            labRow = rownames(heatmap_data),   # Label rows with gene symbols
            labCol = colnames(heatmap_data),   # Label columns with sample names
            scale = "none",                    # No scaling applied
            margins = c(10, 13),               # Adjust margins for better readability
            cexCol = axis_label_size / 14,     # Adjust the size of the column labels
            cexRow = axis_label_size / 14,     # Adjust the size of the row labels
            trace = "none",                    # Remove traces inside the heatmap cells
            xlab = input$text_xaxis,
            ylab = input$text_yaxis,
            main = NULL,
            # lwid = lwid,
            # lhei = lhei
          )
          
          # # Manually add the title with a specific size
          # title(
          #   main = input$title,
          #   cex.main = input$textlab_size_title / 14,  # Adjust the size of the title
          #   line = 2
          # )
          
          if (input$legendPositionHeat != "none") {
            legend(
              x = input$legendPositionHeat,
              legend = names(selected_colors), # Labels for each group
              col = selected_colors, # Colors for each group
              pch = 15, # Square symbols
              pt.cex = 1.5, # Size of the symbols
              cex = legend_content_size / 14, # Size of the legend text
              title = input$legend_title, # Title of the legend
              title.cex = legend_title_size / 14 # Adjust legend title size
            )
          }
          
          
          #   bty = "n", # No box a round the legend
          dev.off()  # Close the PNG devices
        }
      } else {
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
    }
  )
  
}

shinyApp(ui = ui, server = server)
