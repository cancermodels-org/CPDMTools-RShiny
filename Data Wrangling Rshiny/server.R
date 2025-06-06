source("global.R")

server <- function(input, output, session) {
  ####### Data Wrangling RShiny Server Logic #######
  
  # 1. Labguru
  plate_data <- reactiveVal(NULL)
  get_max_row_col <- function(data) {
    max_row_letter <- max(data$row)
    max_col_number <- max(data$column)
    list(max_row = max_row_letter, max_col = max_col_number)
  }
  
  observeEvent(input$import_labguru, {
    req(input$labguru_file)
    plate_data(labguru_plate_prep(input$labguru_file$datapath))
    max_vals <- get_max_row_col(plate_data())
    row_letters <- unique(plate_data()$row)
    row_letters <- row_letters[order(row_letters)]
    
    updateSelectInput(session, "model_name",
                      choices = unique(plate_data()$inventory_item_name),
                      selected = unique(plate_data()$inventory_item_name)[1]
    )
    updateTextInput(session, "model_name_text",
                    value = unique(plate_data()$inventory_item_name)[1]
    )
    updateSelectInput(session, "row_min", choices = row_letters, selected = row_letters[1])
    updateSelectInput(session, "row_max", choices = row_letters, selected = tail(row_letters, 1))
    updateSliderInput(session, "column_range",
                      min = 1, max = max_vals$max_col,
                      value = c(1, max_vals$max_col)
    )
  })
  
  filtered_data <- reactive({
    if (is.null(plate_data())) return(NULL)
    if (input$filter_type == "Labguru Model Name") {
      plate_data() %>%
        filter(inventory_item_name == input$model_name)
    } else {
      row_letters <- unique(plate_data()$row)
      row_letters <- row_letters[order(row_letters)]
      selected_rows <- row_letters[which(row_letters == input$row_min):which(row_letters == input$row_max)]
      plate_data() %>%
        filter(
          row %in% selected_rows,
          column >= input$column_range[1] & column <= input$column_range[2]
        )
    }
  })
  
  output$plate_map_table <- renderDT({
    datatable(filtered_data(), options = list(scrollX = TRUE))
  })
  
  output$export_data <- downloadHandler(
    filename = function() {
      file_name_lower <- tolower(input$labguru_file$name)
      extracted_prefix <- str_extract(file_name_lower, ".*?(?=cpdm)")
      if (input$filter_type == "Labguru Model Name") {
        paste0(extracted_prefix, tolower(input$model_name), "_labguru_prep.xlsx")
      } else {
        paste0(extracted_prefix, tolower(input$model_name_text), "_labguru_prep.xlsx")
      }
    },
    content = function(file) {
      req(filtered_data())
      writexl::write_xlsx(filtered_data(), path = file)
    }
  )
  
  
  # 2. Tecan
  tecan_data <- reactiveVal(NULL)
  get_max_row_col <- function(data) {
    max_row_letter <- max(data$row)
    max_col_number <- max(data$column)
    list(max_row = max_row_letter, max_col = max_col_number)
  }
  
  labguru_present <- reactive({
    !is.null(plate_data())
  })
  output$labguru_present <- reactive({ labguru_present() })
  outputOptions(output, "labguru_present", suspendWhenHidden = FALSE)
  
  observeEvent(input$import_tecan, {
    req(input$tecan_file)
    tryCatch({
      if (input$drugging_type == "Monotherapy") {
        data <- tecan_report_prep_mono(
          file_path = input$tecan_file$datapath,
          remove_na = TRUE
        )
      } else {
        data <- tecan_report_prep_syn(
          file_path = input$tecan_file$datapath,
          remove_na = TRUE
        )
      }
      tecan_data(data)
      max_vals <- get_max_row_col(tecan_data())
      updateSliderInput(session, "column_number_range",
                        min = 1, max = max_vals$max_col,
                        value = c(1, max_vals$max_col)
      )
      available_rows <- unique(data$row)
      available_rows <- available_rows[available_rows %in% LETTERS[1:16]]
      updateSelectInput(session, "min_letter_range", choices = available_rows, selected = available_rows[1])
      updateSelectInput(session, "max_letter_range", choices = available_rows, selected = tail(available_rows, 1))
      max_plate <- max(data$plate)
      updateSliderInput(session, "plate_range",
                        min = 1, max = max_plate,
                        value = c(1, max_plate)
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred while processing the Tecan report:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  filtered_tecan_data <- reactive({
    if (is.null(tecan_data())) return(NULL)
    req(tecan_data())
    data <- tecan_data()
    data <- data[data$plate >= input$plate_range[1] & data$plate <= input$plate_range[2], ]
    data <- data[data$column >= input$column_number_range[1] & data$column <= input$column_number_range[2], ]
    min_row <- input$min_letter_range
    max_row <- input$max_letter_range
    data <- data[data$row >= min_row & data$row <= max_row, ]
    data
  })
  
  output$tecan_plate_map_table <- renderDT({
    req(filtered_tecan_data())
    datatable(filtered_tecan_data(), options = list(scrollX = TRUE))
  })
  
  output$export_tecan <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$tecan_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(filtered_tecan_data())
      writexl::write_xlsx(filtered_tecan_data(), path = file)
    }
  )
  
  
  # 3. Growth Data Wrangling
  growth_data <- reactiveVal(NULL)
  
  observeEvent(input$import_growth, {
    req(input$growth_file)
    data <- CPDMTools::growth_data_prep(
      file_path = input$growth_file$datapath,
      imaging_equipment = input$imaging_type
    )
    growth_data(data)
    time_choices <- unique(data$time)
    default_selection <- if (is.null(input$tecan_file)) "No Treatment" else min(time_choices)
    updateSelectInput(session, "time_point",
                      choices = c("No Treatment", time_choices),
                      selected = default_selection
    )
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
      data$treatment_period_yn <- ifelse(data$time >= selected_time, "Yes", "No")
    } else {
      data$treatment_period_yn <- "No"
    }
    growth_data(data)
  })
  
  output$export_growth_data <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$growth_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(growth_data())
      writexl::write_xlsx(growth_data(), path = file)
    }
  )
  
  
  # 4. CTG Image Data Wrangling
  ctg_data <- reactiveVal(NULL)
  
  observeEvent(input$import_ctg, {
    req(input$ctg_file)
    equipment <- input$ctg_type
    tryCatch({
      data <- CPDMTools::ctg_prep(
        file_path = input$ctg_file$datapath,
        equipment = equipment
      )
      ctg_data(data)
      
      if (equipment == "SpectraMAX iD3") {
        updateSliderInput(session, "plate_number_range",
                          min = min(data$ctg_plate_number, na.rm = TRUE),
                          max = max(data$ctg_plate_number, na.rm = TRUE),
                          value = c(min(data$ctg_plate_number, na.rm = TRUE), max(data$ctg_plate_number, na.rm = TRUE))
        )
        updateSelectInput(session, "plate_name",
                          choices = c("All Plates", unique(data$ctg_plate_name)),
                          selected = "All Plates"
        )
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("An error occurred:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
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
  
  output$ctg_data_table <- renderDT({
    req(filtered_ctg_data())
    datatable(filtered_ctg_data(), options = list(scrollX = TRUE))
  })
  
  output$export_ctg_data <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$ctg_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(filtered_ctg_data())
      writexl::write_xlsx(filtered_ctg_data(), path = file)
    }
  )
  
  
  # 5. Joined Data
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
  
  observeEvent(input$control_location, {
    update_control_dropdowns()
  })
  
  observeEvent(input$join_datasets, {
    ctg_data_to_join <- if (input$data_file_type == "CTG") filtered_ctg_data() else NULL
    growth_data_to_join <- if (input$data_file_type == "Imaging") growth_data() else NULL
    
    joined_data(plate_data_join(
      labguru_plate_data_frame = if (!is.null(filtered_data())) filtered_data() else NULL,
      tecan_plate_data_frame   = if (!is.null(filtered_tecan_data())) filtered_tecan_data() else NULL,
      growth_data_frame        = if (!is.null(growth_data())) growth_data() else NULL,
      ctg_data_frame           = ctg_data_to_join
    ))
    update_control_dropdowns()
  })
  
  output$showTransferButton <- reactive({
    data <- joined_data()
    !is.null(data) && any(grepl("-", data$well_annotation))
  })
  outputOptions(output, "showTransferButton", suspendWhenHidden = FALSE)
  
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
  
  observeEvent(input$update_controls, {
    data <- joined_data()
    if (!is.null(data)) {
      control_var <- if (input$control_location == "Treatment Name") "treatment_name" else "well_annotation"
      if (control_var %in% names(data)) {
        data$treatment_type <- ifelse(
          data[[control_var]] == input$media_control, "Media Control",
          ifelse(
            data[[control_var]] == input$negative_control, "Negative Control",
            ifelse(data[[control_var]] == input$positive_control, "Monotherapy", data$treatment_type)
          )
        )
        
        if (input$positive_control_concentration == "Yes") {
          max_concentration_data <- data %>%
            filter(data[[control_var]] == input$positive_control) %>%
            group_by(!!sym(control_var)) %>%
            summarize(max_concentration = max(concentration, na.rm = TRUE)) %>%
            ungroup()
          
          data <- data %>%
            left_join(max_concentration_data, by = control_var) %>%
            mutate(treatment_type = ifelse(
              data[[control_var]] == input$positive_control & concentration == max_concentration,
              "Positive Control", treatment_type
            )) %>%
            select(-max_concentration)
        } else {
          data <- data %>%
            mutate(treatment_type = ifelse(
              data[[control_var]] == input$positive_control, "Positive Control", treatment_type
            ))
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
      labguru_name <- if (input$filter_type == "Labguru Model Name") {
        tolower(input$model_name)
      } else {
        tolower(input$model_name_text)
      }
      data_type <- if (input$data_file_type == "Imaging") "growth" else "ctg"
      drugging_suffix <- if (input$drugging_type == "Synergy") "_syn" else ""
      if (!is.null(plate_data())) {
        if (input$tecan_data == "Yes") {
          paste0(plate_num, "_", labguru_name, "_", data_type, drugging_suffix, "_joined.xlsx")
        } else {
          paste0(plate_num, "_", labguru_name, "_", data_type, "_joined.xlsx")
        }
      } else {
        paste0("tecan_", data_type, drugging_suffix, "_joined.xlsx")
      }
    },
    content = function(file) {
      req(joined_data())
      data_type <- if (input$data_file_type == "Imaging") "growth" else "ctg"
      metadata <- data.frame(
        data_type = data_type,
        growth_metric_units = if (data_type == "growth") input$growth_metric else NA,
        time_units = input$time_unit,
        r_version = R.version.string,
        date = Sys.Date()
      )
      if (data_type != "growth") {
        metadata$growth_metric_units <- NULL
      }
      writexl::write_xlsx(
        list("Joined Data" = joined_data(), "Metadata" = metadata),
        path = file
      )
    }
  )
  
  
  ####### Data QC RShiny Server Logic #######
  
  rv <- reactiveValues(
    input_data   = NULL,  # raw imported data (growth or CTG) for QC
    updated_data = NULL,  # “locked‐in” data after rounding + palette
    file_name    = NULL
  )
  
  # 1. Reactive expression for rounded data
  rounded_data <- reactive({
    req(rv$input_data)
    if (!"concentration" %in% names(rv$input_data)) return(NULL)
    CPDMTools::round_concentration(
      data_frame     = rv$input_data,
      round_by       = input$round_by,
      use_nearest_10 = as.logical(input$use_nearest_10)
    )
  })
  
  # 2. Import handler for Growth Data (QC)
  observeEvent(input$growth_file, {
    req(input$growth_file)
    ext <- tools::file_ext(input$growth_file$name)
    rv$file_name <- tools::file_path_sans_ext(input$growth_file$name)
    
    df <- switch(ext,
                 csv  = read.csv(input$growth_file$datapath),
                 txt  = read.delim(input$growth_file$datapath),
                 xlsx = readxl::read_excel(input$growth_file$datapath)
    )
    
    rv$input_data <- df
    rv$updated_data <- NULL
    
    if ("concentration" %in% names(df)) {
      showTab("main_tabs", "tab_rep_conc")
      updateTabsetPanel(session, "main_tabs", selected = "tab_rep_conc")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "tab_growth_qc")
    }
    hideTab("main_tabs", "tab_updated")
  })
  
  # 3. Import handler for CTG Data (QC)
  observeEvent(input$ctg_file, {
    req(input$ctg_file)
    ext <- tools::file_ext(input$ctg_file$name)
    rv$file_name <- tools::file_path_sans_ext(input$ctg_file$name)
    
    df <- switch(ext,
                 csv  = read.csv(input$ctg_file$datapath),
                 txt  = read.delim(input$ctg_file$datapath),
                 xlsx = readxl::read_excel(input$ctg_file$datapath)
    )
    
    rv$input_data <- df
    rv$updated_data <- NULL
    
    showTab("main_tabs", "tab_rep_conc")
    showTab("main_tabs", "tab_ctg_controls")
    showTab("main_tabs", "tab_ctg_qc")
    hideTab("main_tabs", "tab_updated")
    updateTabsetPanel(session, "main_tabs", selected = "tab_rep_conc")
  })
  
  # 4. Replicates and Concentrations Table (QC)
  output$rep_conc_table <- renderDT({
    rd <- rounded_data()
    req(rd)
    summary_table <- rd %>%
      count(treatment_name, concentration) %>%
      arrange(treatment_name, concentration)
    datatable(summary_table, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 5. Lock In Rounded Concentrations (QC)
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
    
    if (input$data_type == "Growth Data") {
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
  
  # 6. Positive Control UI (QC)
  output$positive_control_ui <- renderUI({
    req(rv$updated_data)
    has_pos <- "Positive Control" %in% rv$updated_data$treatment_type
    selectInput("use_positive_control", "Normalization Technique",
                choices = c("Negative and Positive Control" = TRUE, "Negative Control Only" = FALSE),
                selected = if (has_pos) TRUE else FALSE
    )
  })
  
  # 7a. Static ggplot version (QC)
  output$ctg_control_ggplot <- renderPlot({
    req(rv$updated_data)
    CPDMTools::ctg_qc_control_plot(
      data_frame      = rv$updated_data,
      show_outlier    = input$show_outlier,
      make_interactive = FALSE
    )
  })
  
  # 7b. Interactive plotly version (QC)
  output$ctg_control_plotly <- renderPlotly({
    req(rv$updated_data)
    CPDMTools::ctg_qc_control_plot(
      data_frame      = rv$updated_data,
      show_outlier    = input$show_outlier,
      make_interactive = TRUE
    )
  })
  
  # 8. Normalize CTG Data (QC)
  observeEvent(input$normalize_ctg, {
    req(rv$updated_data)
    rv$updated_data <- CPDMTools::ctg_normalize(
      data_frame           = rv$updated_data,
      use_positive_control = as.logical(input$use_positive_control)
    )
    showTab("main_tabs", "tab_ctg_qc")
    showTab("main_tabs", "tab_updated")
    updateTabsetPanel(session, "main_tabs", selected = "tab_ctg_qc")
  })
  
  # 9. Export Growth Data (QC)
  output$export_growth_data <- downloadHandler(
    filename = function() {
      req(rv$file_name)
      paste0(rv$file_name, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::growth_qc_output(
        data_frame            = rv$updated_data,
        outlier_manual_only   = as.logical(input$outlier_manual_only),
        growthcurveme         = input$growthcurveme,
        lgrscore              = input$lgrscore,
        prism                 = input$prism
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )
  
  # 10. Export CTG Data (QC)
  output$export_ctg_data <- downloadHandler(
    filename = function() {
      req(rv$file_name)
      paste0(rv$file_name, "_qc.xlsx")
    },
    content = function(file) {
      req(rv$updated_data)
      output_list <- CPDMTools::ctg_qc_output(
        ctg_list            = list(rv$updated_data),
        outlier_manual_only = as.logical(input$outlier_manual_only_ctg),
        prism               = input$prism_ctg
      )
      writexl::write_xlsx(output_list, path = file)
    }
  )
}

shinyApp(ui, server)
