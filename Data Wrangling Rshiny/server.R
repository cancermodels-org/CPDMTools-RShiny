source("global.R")


server <- function(input, output, session) {
  
  # download data wrangling sample dataset
  output$data_wrangling_sample_dataset <- downloadHandler(
    filename = function() {
      "Sample_Data_Wrangling.zip"
    },
    content = function(file) {
      source_directory <- "data_wrangling_sample_data"
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      
      #switch to the directory containing the folder to zip
      parent_dir <- dirname(source_directory)
      target_dir <- basename(source_directory)
      setwd(parent_dir)
      files_to_zip <- list.files(target_dir, recursive = TRUE)
      
      zip::zip(zipfile = file, files = file.path(target_dir, files_to_zip)) #creat zip folder
    },
    contentType = "application/zip"
  )
  
  
  # 1 labguru
  plate_data <- reactiveVal(NULL)
  #function to get the max row and column values
  get_max_row_col <- function(data) {
    max_row_letter <- max(data$row)  
    max_col_number <- max(data$column)
    list(max_row = max_row_letter, max_col = max_col_number)
  }
  
  observeEvent(input$import_labguru, {
    req(input$labguru_file)
    plate_data(labguru_plate_prep(input$labguru_file$datapath))
    max_vals <- get_max_row_col(plate_data()) #get max row and column
    row_letters <- unique(plate_data()$row)
    row_letters <- row_letters[order(row_letters)]
    
    #model name inputs
    updateSelectInput(session, "model_name", 
                      choices = unique(plate_data()$inventory_item_name), 
                      selected = unique(plate_data()$inventory_item_name)[1])
    updateTextInput(session, "model_name_text", 
                    value = unique(plate_data()$inventory_item_name)[1])
    updateSelectInput(session, "row_min", choices = row_letters, selected = row_letters[1])
    updateSelectInput(session, "row_max", choices = row_letters, selected = tail(row_letters, 1))
    updateSliderInput(session, "column_range", 
                      min = 1, max = max_vals$max_col, 
                      value = c(1, max_vals$max_col))
  })
  # Filtered Data
  filtered_data <- reactive({
    if (is.null(plate_data())) {
      return(NULL)
    }
    req(plate_data())
    
    if (input$filter_type == "Labguru Model Name") {
      plate_data() %>%
        filter(inventory_item_name == input$model_name)
    } else {
      row_letters <- unique(plate_data()$row)
      row_letters <- row_letters[order(row_letters)]
      selected_rows <- row_letters[which(row_letters == input$row_min):which(row_letters == input$row_max)]
      
      plate_data() %>%
        filter(row %in% selected_rows, 
               column >= input$column_range[1] & column <= input$column_range[2])
    }
  })
  
  #render DT
  output$plate_map_table <- renderDT({
    # req(filtered_data(), options = list(scrollX = TRUE))
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  #download Data
  output$export_data <- downloadHandler(
    filename = function() {
      file_name_lower <- tolower(input$labguru_file$name)
      #extracted_prefix <- strsplit(file_name_lower, "CPDM")[[1]][1]
      extracted_prefix <- str_extract(file_name_lower, ".*?(?=cpdm)")
      if (input$filter_type == "Labguru Model Name") {
        paste0(extracted_prefix, tolower(input$model_name), "_labguru_prep.xlsx")
      } else {
        paste0(extracted_prefix, tolower(input$model_name_text), "_labguru_prep.xlsx")
      }
    },
    content = function(file) {
      req(filtered_data())
      write.xlsx(filtered_data(), file)
    }
  )
  
  # 2 Tecan data
  tecan_data <- reactiveVal(NULL)
  #get max row (letter) and column values
  get_max_row_col <- function(data) {
    max_row_letter <- max(data$row)   # Max row value (letter)
    max_col_number <- max(data$column) # Max column value (numeric)
    list(max_row = max_row_letter, max_col = max_col_number)
  }
  #check if Labguru plate is present
  labguru_present <- reactive({
    !is.null(plate_data())
  })
  
  output$labguru_present <- reactive({
    labguru_present()
  })
  outputOptions(output, "labguru_present", suspendWhenHidden = FALSE)
  
  observeEvent(input$import_tecan, {
    req(input$tecan_file)
    tryCatch({
      if (input$drugging_type == "Monotherapy") {
        data <- tecan_report_prep_mono(
          file_path = input$tecan_file$datapath,
          remove_na = TRUE
        )
      } else if (input$drugging_type == "Synergy") {
        data <- tecan_report_prep_syn(
          file_path = input$tecan_file$datapath,
          remove_na = TRUE
        )
      } else {
        stop("Invalid drugging type selected.")
      }
      tecan_data(data)
      max_vals <- get_max_row_col(tecan_data()) # Get max row and column values
      updateSliderInput(session, "column_number_range", 
                        min = 1, max = max_vals$max_col, 
                        value = c(1, max_vals$max_col))
      # Update the selectInput for row letters
      available_rows <- unique(data$row)
      available_rows <- available_rows[available_rows %in% LETTERS[1:16]]
      updateSelectInput(session, "min_letter_range", choices = available_rows, selected = available_rows[1])
      updateSelectInput(session, "max_letter_range", choices = available_rows, selected = tail(available_rows, 1))
      #plate range slider input
      max_plate <- max(data$plate)
      updateSliderInput(session, "plate_range", 
                        min = 1, max = max_plate, 
                        value = c(1, max_plate))
      
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while processing the Tecan report:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Reactive filtered
  filtered_tecan_data <- reactive({
    
    if (is.null(tecan_data())) {
      return(NULL)
    }
    req(tecan_data())
    data <- tecan_data()
    # Filter based on plate range
    data <- data[data$plate >= input$plate_range[1] & data$plate <= input$plate_range[2], ]
    
    data <- data[data$column >= input$column_number_range[1] & data$column <= input$column_number_range[2], ]  # Filter based on column number range
    min_row <- input$min_letter_range
    max_row <- input$max_letter_range
    data <- data[data$row >= min_row & data$row <= max_row, ]
    
    data
  })
  # Render table 
  output$tecan_plate_map_table <- renderDT({
    req(filtered_tecan_data())
    datatable(filtered_tecan_data(), options = list(scrollX = TRUE))
  })
  
  # Download DT
  output$export_tecan <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$tecan_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(filtered_tecan_data())
      write.xlsx(filtered_tecan_data(), file)
    }
  )
  # 3 growth data  
  growth_data <- reactiveVal(NULL)
  
  observeEvent(input$import_growth, {
    req(input$growth_file)
    data <- CPDMTools::growth_data_prep(file_path = input$growth_file$datapath, imaging_equipment = input$imaging_type)
    
    growth_data(data)
    time_choices <- unique(data$time) #time point
    default_selection <- if (is.null(input$tecan_file)) "No Treatment" else min(time_choices)
    updateSelectInput(session, "time_point", 
                      choices = c("No Treatment", time_choices), 
                      selected = default_selection)
  })
  output$growth_data_table <- renderDT({
    req(growth_data())
    datatable(growth_data())
  })
  
  observeEvent(input$time_point, {
    req(growth_data())
    data <- growth_data()
    selected_time <- as.numeric(input$time_point)
    
    if (!is.na(selected_time) && input$time_point != "No Treatment") {
      # treatment_period_yn update based on selected time point
      data$treatment_period_yn <- ifelse(data$time >= selected_time, "Yes", "No")
    } else {
      data$treatment_period_yn <- "No"
    }
    growth_data(data)
  })
  # download growth data dt  
  output$export_growth_data <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$growth_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(growth_data())
      write.xlsx(growth_data(), file)
    }
  )
  ### 4 CTG image data 
  
  ctg_data <- reactiveVal(NULL)
  observeEvent(input$import_ctg, {
    req(input$ctg_file)
    equipment <- input$ctg_type
    tryCatch({
      data <- CPDMTools::ctg_prep(file_path = input$ctg_file$datapath, equipment = equipment)
      ctg_data(data)
      
      if (equipment == "SpectraMAX iD3") {
        #slid and select options for "SpectraMAX iD3" DATA
        updateSliderInput(session, "plate_number_range",
                          min = min(data$ctg_plate_number, na.rm = TRUE),
                          max = max(data$ctg_plate_number, na.rm = TRUE),
                          value = c(min(data$ctg_plate_number, na.rm = TRUE), max(data$ctg_plate_number, na.rm = TRUE)))
        
        updateSelectInput(session, "plate_name",
                          choices = c("All Plates", unique(data$ctg_plate_name)),
                          selected = "All Plates")
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  # Reactive expression for filtered CTG data
  filtered_ctg_data <- reactive({
    req(ctg_data())
    data <- ctg_data()
    if (input$ctg_type == "SpectraMAX iD3") {
      if (input$filter_data_by == "CTG Plate Number Range") {
        data <- data[data$ctg_plate_number >= input$plate_number_range[1] &
                       data$ctg_plate_number <= input$plate_number_range[2], ]
      } else if (input$filter_data_by == "CTG Plate Name" && input$plate_name != "All Plates") {
        data <- data[data$ctg_plate_name == input$plate_name, ]
      }
    }
    data 
  })
  #filtered DT
  output$ctg_data_table <- renderDT({
    req(filtered_ctg_data())
    datatable(filtered_ctg_data(), options = list(scrollX = TRUE))
  })
  
  #export data
  output$export_ctg_data <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$ctg_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(filtered_ctg_data())
      write.xlsx(filtered_ctg_data(), file)
    }
  )
  ## 5 join table 
  joined_data <- reactiveVal(NULL)
  observe({
    if (is.null(tecan_data())) {
      updateRadioButtons(session, "control_location", selected = "Well Annotation")
    } else {
      updateRadioButtons(session, "control_location", selected = "Treatment Name")
    }
  })
  
  update_control_dropdowns <- function() {
    data <- joined_data()
    if (!is.null(data)) {
      control_var <- if (input$control_location == "Treatment Name") "treatment_name" else "well_annotation"
      unique_values <- unique(data[[control_var]])
      choices <- c("None", unique_values)
      
      # Default selection for media control
      default_media_control <- if (any(grepl("Media", unique_values, ignore.case = TRUE))) {
        "Media Only"
      } else {
        "None"
      }
      updateSelectInput(session, "media_control", choices = choices, selected = default_media_control)
      updateSelectInput(session, "negative_control", choices = choices, selected = ifelse(any(grepl("DMSO 0.5%", unique_values)), "DMSO 0.5%", "None"))
      updateSelectInput(session, "positive_control", choices = choices, selected = ifelse(any(grepl("DMSO 10%", unique_values)), "DMSO 10%", "None"))
    }
  }
  # Observe changes in control_location to update dropdowns
  observeEvent(input$control_location, {
    update_control_dropdowns()
  })
  #joining datasets
  #data type is currently selected
  observeEvent(input$join_datasets, {
    ctg_data_to_join <- if (input$data_file_type == "CTG") filtered_ctg_data() else NULL
    growth_data_to_join <- if (input$data_file_type == "Imaging") growth_data() else NULL
    #join datasets
    joined_data(plate_data_join(
      labguru_plate_data_frame = if (!is.null(filtered_data())) filtered_data() else NULL,
      tecan_plate_data_frame = if (!is.null(filtered_tecan_data())) filtered_tecan_data() else NULL,
      growth_data_frame = if (!is.null(growth_data())) growth_data() else NULL,
      #ctg_data_frame = if (!is.null(filtered_ctg_data())) filtered_ctg_data() else NULL
      ctg_data_frame = ctg_data_to_join
    ))
    update_control_dropdowns()
  })
  ##### transfer button
  output$showTransferButton <- reactive({
    data <- joined_data()
    !is.null(data) && any(grepl("-", data$well_annotation))
  })
  outputOptions(output, "showTransferButton", suspendWhenHidden = FALSE)
  
  #well_annotate_transfer function
  observeEvent(input$transfer_annotations, {
    data <- joined_data()
    if (!is.null(data)) {
      data <- well_annotate_transfer(data)
      joined_data(data)
    }
  })
  output$joined_data_table <- renderDT({
    req(joined_data())
    datatable(joined_data(), options = list(scrollX = TRUE))
  })
  
  ####
  observeEvent(input$update_controls, {
    data <- joined_data()
    if (!is.null(data)) {
      control_var <- if (input$control_location == "Treatment Name") "treatment_name" else "well_annotation"
      if (control_var %in% names(data)) {
        data$treatment_type <- ifelse(data[[control_var]] == input$media_control, "Media Control",
                                      ifelse(data[[control_var]] == input$negative_control, "Negative Control",
                                             ifelse(data[[control_var]] == input$positive_control, "Monotherapy", data$treatment_type)))
        if (input$positive_control_concentration == "Yes") {
          # Identify the maximum concentration for the selected positive control
          max_concentration_data <- data %>%
            filter(data[[control_var]] == input$positive_control) %>%
            group_by(!!sym(control_var)) %>%
            summarize(max_concentration = max(concentration, na.rm = TRUE)) %>%
            ungroup()
          data <- data %>%
            left_join(max_concentration_data, by = control_var) %>%
            mutate(treatment_type = ifelse(data[[control_var]] == input$positive_control & concentration == max_concentration, 
                                           "Positive Control", treatment_type)) %>%
            select(-max_concentration)
        } else {
          #if "No" is selected, show all concentrations as Positive Control
          data <- data %>%
            mutate(treatment_type = ifelse(data[[control_var]] == input$positive_control, "Positive Control", treatment_type))
        }
        joined_data(data)
      }
    }
  })
  
  output$export_joined_data <- downloadHandler(
    filename = function() {
      plate_num <- if (!is.null(input$labguru_file)) { 
        tolower(str_extract(basename(input$labguru_file$name), "(?i)^plate_(\\d+)")) 
      } else {
        "tecan"
      }
      labguru_name <- if (input$filter_type == "Labguru Model Name") { #labguru_name
        tolower(input$model_name)
      } else {
        tolower(input$model_name_text) #model_name_text #"custom_labguru_name" 
      }
      #data type
      data_type <- if (input$data_file_type == "Imaging") "growth" else "ctg"
      drugging_suffix <- if (input$drugging_type == "Synergy") "_syn" else "" # if tecal is Synergy, "_syn" suffix
      #filename based on the conditions
      if (!is.null(plate_data())) {
        if (input$tecan_data == "Yes") { # if Labguru Plate Map is present
          paste0(plate_num, "_", labguru_name, "_", data_type, drugging_suffix, "_joined.xlsx")
        } else {
          paste0(plate_num, "_", labguru_name, "_", data_type, "_joined.xlsx")
        }
      } else { #No Labguru Plate Map
        paste0("tecan_", data_type, drugging_suffix, "_joined.xlsx")
      }
    },
    # metadata
    content = function(file) {
      req(joined_data())  
      data_type = if (input$data_file_type == "Imaging") "growth" else "ctg"
      metadata <- data.frame(
        data_type = data_type,
        growth_metric_units = if (data_type == "growth") input$growth_metric else NA,
        #time_units = input$time_unit,
        time_units = if (data_type == "growth") input$time_unit else NA,
        r_version = R.version.string,
        date = Sys.Date()
      )
      #remove the growth_metric_units if data type is ctg 
      if (data_type != "growth") {
        metadata$growth_metric_units <- NULL
      }
      if (data_type != "growth") {
        metadata$time_units <- NULL
      }
      
      write_xlsx(list("Joined Data" = joined_data(), "Metadata" = metadata), path = file)
    }
  )
  ####### Data QC RShiny Server Logic #######
  rv <- reactiveValues(
    input_data_qc   = NULL,  #imported data(growth or CTG) for QC
    updated_data = NULL,  # “locked‐in” data after rounding + palette
    file_name_qc    = NULL,
    concentrations_locked = FALSE,
    normalized = FALSE
  )
  # Sample dataset download handler
  output$sample_dataset <- downloadHandler(
    filename = function() {
      "Sample_data.zip"
    },
    content = function(file) {
      source_files <- c(
        "www/endpoint_assay_qc_input.xlsx",
        "www/growth_assay_qc_input.xlsx"
      )
      temp_dir <- tempdir()
      temp_files <- file.path(temp_dir, basename(source_files))
      if (!all(file.copy(source_files, temp_files, overwrite = TRUE))) {
        stop("Failed to copy files.")
      }
      
      zip::zip(zipfile = file, files = temp_files, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
  
  # Reactive expression for rounded data
  rounded_data <- reactive({
    req(rv$input_data_qc)
    if (!"concentration" %in% names(rv$input_data_qc)) return(NULL)
    CPDMTools::round_concentration(
      data_frame     = rv$input_data_qc,
      round_by       = input$round_by,
      use_nearest_10 = as.logical(input$use_nearest_10)
    )
  })
  
  # Observe changes in the data type selection
  observeEvent(input$qc_data_type, {
    if (input$qc_data_type == "Growth Data") {
      showTab("main_tabs", "tab_growth_qc")
      hideTab("main_tabs", "tab_ctg_controls")
      hideTab("main_tabs", "tab_ctg_qc")
      
    } else if (input$qc_data_type == "End-Point Assay Data") {
      showTab("main_tabs", "tab_ctg_controls")
      showTab("main_tabs", "tab_ctg_qc")
      hideTab("main_tabs", "tab_growth_qc")
    }
  })
  
  
  # Import handler for Growth Data (QC)
  observeEvent(input$qc_growth_file, {
    req(input$qc_growth_file)
    ext <- tools::file_ext(input$qc_growth_file$name)
    rv$file_name_qc <- tools::file_path_sans_ext(input$qc_growth_file$name)
    
    df <- switch(ext,
                 csv  = read.csv(input$qc_growth_file$datapath),
                 txt  = read.delim(input$qc_growth_file$datapath),
                 xlsx = readxl::read_excel(input$qc_growth_file$datapath)
    )
    
    rv$input_data_qc <- df
    rv$updated_data <- NULL
    
    # Debugging: Print the column names and structure of the imported data
    print(names(df))
    print(str(df))
    
    if ("concentration" %in% names(df)) {
      updateSelectInput(session, "concentration", choices = c("All Concentrations", unique(df$concentration)))
      updateCheckboxInput(session, "concentration_detected", value = TRUE)
    } else {
      updateCheckboxInput(session, "concentration_detected", value = FALSE)
    }
    
    updateSelectInput(session, "treatment_name_gd", choices = unique(df$treatment_name))
    
    
    showTab("main_tabs", "tab_rep_conc")
    updateTabsetPanel(session, "main_tabs", selected = "tab_rep_conc")
    
  })
  
  # Import handler for CTG Data (QC)
  observeEvent(input$ctg_file_qc, {
    req(input$ctg_file_qc)
    ext <- tools::file_ext(input$ctg_file_qc$name)
    rv$file_name_qc <- tools::file_path_sans_ext(input$ctg_file_qc$name)
    
    df <- switch(ext,
                 csv  = read.csv(input$ctg_file_qc$datapath),
                 txt  = read.delim(input$ctg_file_qc$datapath),
                 xlsx = readxl::read_excel(input$ctg_file_qc$datapath)
    )
    
    rv$input_data_qc <- df
    rv$updated_data <- NULL
    
    showTab("main_tabs", "tab_rep_conc")
    showTab("main_tabs", "tab_ctg_controls")
    showTab("main_tabs", "tab_ctg_qc")
    hideTab("main_tabs", "tab_updated")
    updateTabsetPanel(session, "main_tabs", selected = "tab_rep_conc")
    
    # Update treatment name choices based on imported data
    updateSelectInput(session, "treatment_name_qc", choices = unique(df$treatment_name[df$treatment_type == "Monotherapy"]))
  })
  
  # Replicates and Concentrations Table (QC)
  output$rep_conc_table <- renderDT({
    rd <- rounded_data()
    req(rd)
    summary_table <- rd %>%
      count(treatment_name, concentration) %>%
      arrange(treatment_name, concentration)
    datatable(summary_table, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Replicates and Concentration Rounding Table
  observeEvent(input$lock_round, {
    rd <- rounded_data()
    req(rd)
    rv$updated_data <- rd %>%
      mutate(
        outlier_auto_yn         = "No",
        outlier_auto_flag_reason = NA,
        outlier_manual_yn       = NA,
        outlier_manual_flag_reason = NA
      ) %>%
      CPDMTools::color_palette_mono()
    
    if (input$qc_data_type == "Growth Data") {
      showTab("main_tabs", "tab_growth_qc")
      showTab("main_tabs", "tab_updated")
      updateTabsetPanel(session, "main_tabs", selected = "tab_growth_qc")
    } else {
      showTab("main_tabs", "tab_ctg_controls")
      showTab("main_tabs", "tab_ctg_qc")
      showTab("main_tabs", "tab_updated")
      updateTabsetPanel(session, "main_tabs", selected = "tab_ctg_controls")
    }
  })
  
  # Positive Control UI (QC)
  output$positive_control_ui <- renderUI({
    req(rv$updated_data)
    has_pos <- "Positive Control" %in% rv$updated_data$treatment_type
    selectInput("use_positive_control", "Normalization Technique",
                choices = c("Negative and Positive Control" = TRUE, "Negative Control Only" = FALSE),
                selected = if (has_pos) TRUE else FALSE
    )
  })
  
  # Static ggplot version (QC)
  output$ctg_control_ggplot <- renderPlot({
    req(rv$updated_data)
    CPDMTools::ctg_qc_control_plot(
      data_frame      = rv$updated_data,
      show_outlier    = input$show_outlier,
      make_interactive = FALSE
    )
  })
  
  # Interactive plotly version (QC)
  output$ctg_control_plotly <- renderPlotly({
    req(rv$updated_data)
    CPDMTools::ctg_qc_control_plot(
      data_frame      = rv$updated_data,
      show_outlier    = input$show_outlier,
      make_interactive = TRUE
    )
  })
  
  # Normalize CTG Data (QC)
  observeEvent(input$normalize_ctg, {
    req(rv$updated_data)
    rv$updated_data <- CPDMTools::ctg_normalize(
      data_frame           = rv$updated_data,
      use_positive_control = as.logical(input$use_positive_control)
    )
    rv$normalized <- TRUE
    showTab("main_tabs", "tab_ctg_qc")
    showTab("main_tabs", "tab_updated")
    updateTabsetPanel(session, "main_tabs", selected = "tab_ctg_qc")
  })
  
  # Run/Update Outlier Detection
  observeEvent(input$run_update_outlier_detection, {
    req(rv$updated_data)
    
    if (!rv$normalized) {
      showNotification("Please normalize the data before running outlier detection.", type = "error")
      return()
    }
    #print(head(rv$updated_data))
    
    rv$updated_data <- CPDMTools::ctg_qc_mean_outlier(
      ctg_data = rv$updated_data,
      z_score_threshold = input$z_score_threshold
    )
    
    #print(head(rv$updated_data))
    
    rv$ctg_list <- CPDMTools::dr4pl_qc_fit_loop(
      ctg_data = rv$updated_data,
      concentration_unit = input$concentration_unit,
      method_init = input$method_init,
      method_robust = input$method_robust,
      lb_if_min_gt = input$lb_if_min_gt,
      ub_if_max_lt = input$ub_if_max_lt
    )
    
    #print ctg_list
    #print(rv$ctg_list)
    # Plot the data
    output$ctg_plot <- renderPlot({
      CPDMTools::ctg_qc_treat_plot(
        ctg_list = rv$ctg_list,
        treat_name = input$treatment_name_qc,
        show_dose_response_curve = input$show_dose_response_curve,
        make_interactive = FALSE
      )
    })
    
    output$ctg_plotly <- renderPlotly({
      plotly_object <- CPDMTools::ctg_qc_treat_plot(
        ctg_list = rv$ctg_list,
        treat_name = input$treatment_name_qc,
        show_dose_response_curve = input$show_dose_response_curve,
        make_interactive = TRUE
      )
      plotly_object %>% layout(dragmode = "select") %>% config(displayModeBar = TRUE)
    })
  })
  
  output$updated_data_table <- renderDT({
    datatable(rv$updated_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  #Mark as outlier
  observeEvent(input$mark_as_outlier, {
    req(rv$updated_data)
    selected_points <- input$plot_brush #plot_brush
    if (!is.null(selected_points)) {
      rv$updated_data <- rv$updated_data %>%
        mutate(outlier_manual_yn = ifelse(row_number() %in% selected_points$selected_, "Yes", outlier_manual_yn),
               outlier_manual_flag_reason = ifelse(row_number() %in% selected_points$selected_,
                                                   ifelse(input$outlier_reason == "Other", input$outlier_reason_other, input$outlier_reason),
                                                   outlier_manual_flag_reason))
    }
  })
  #Mark as not an outlier
  observeEvent(input$mark_as_not_outlier, {
    req(rv$updated_data)
    selected_points <- input$plot_brush
    if (!is.null(selected_points)) {
      rv$updated_data <- rv$updated_data %>%
        mutate(outlier_manual_yn = ifelse(row_number() %in% selected_points$selected_, "No", outlier_manual_yn))
    }
  })
  
  
  #Run/Update Outlier Detection for Growth Data
  renderGrowthPlot <- function() {
    req(rv$updated_data)
    filtered_data <- rv$updated_data %>%
      dplyr::filter(treatment_name == input$treatment_name_gd) %>%
      dplyr::filter(if (input$concentration != "All Concentrations") concentration == input$concentration else TRUE)
    
    output$growth_plot <- renderPlot({
      CPDMTools::growth_plot_qc_mono(
        data_frame = filtered_data,
        treatment_name = input$treatment_name_gd,
        show_outlier = input$show_outlier,
        show_only_outlier_wells = input$show_only_outlier_wells,
        make_interactive = FALSE,
        growth_metric_name = input$growth_metric_name,
        time_units = input$time_units,
        n_x_axis_breaks = 12,
        n_y_axis_breaks = 10
      )
    })
    
    output$growth_plotly <- renderPlotly({
      plotly_object <- CPDMTools::growth_plot_qc_mono(
        data_frame = filtered_data,
        treatment_name = input$treatment_name_gd,
        show_outlier = input$show_outlier,
        show_only_outlier_wells = input$show_only_outlier_wells,
        make_interactive = TRUE,
        growth_metric_name = input$growth_metric_name,
        time_units = input$time_units,
        n_x_axis_breaks = 12,
        n_y_axis_breaks = 10
      )
      plotly_object %>% layout(dragmode = "select") %>% config(displayModeBar = TRUE)
    })
  }
  
  observeEvent(input$treatment_name_gd, {
    renderGrowthPlot()
  })
  
  
  observeEvent(input$run_update_outlier_detection_gd, {
    req(rv$updated_data)
    rv$updated_data$growth_metric <- as.numeric(rv$updated_data$growth_metric)
    
    rv$updated_data <- CPDMTools::loess_outlier_fit(
      data_frame = rv$updated_data,
      span_value = input$span_value,
      residual_threshold = input$residual_threshold
    )
    
    renderGrowthPlot()
  })
  
  output$final_updated_data_table <- renderDT({
    req(rv$updated_data)
    if (input$qc_data_type == "End-Point Assay Data" && !is.null(rv$ctg_list)) {
      datatable(rv$ctg_list[[1]], options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(rv$updated_data, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Export Growth Data (QC)
  output$export_growth_data <- downloadHandler(
    filename = function() {
      req(rv$file_name_qc)
      paste0(rv$file_name_qc, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::growth_qc_output(
        data_frame            = rv$updated_data,
        #outlier_manual_only   = as.logical(input$outlier_manual_only),
        growthcurveme         = input$growthcurveme,
        lgrscore              = input$lgrscore,
        prism                 = input$prism
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )
  
  # Export CTG Data (QC)
  output$qc_export_ctg_data <- downloadHandler(
    filename = function() {
      req(rv$file_name_qc)
      paste0(rv$file_name_qc, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::ctg_qc_output(
        ctg_list            = list(rv$updated_data),
       # outlier_manual_only = as.logical(input$outlier_manual_only_ctg),
        prism               = input$prism_ctg
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )
  
  
}

shinyApp(ui, server)