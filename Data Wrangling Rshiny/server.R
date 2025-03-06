source("global.R")
server <- function(input, output, session) {
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
    req(plate_data())
    
    if (input$filter_type == "Labguru Model Name") {
      plate_data() %>%
        filter(inventory_item_name == input$model_name)
    } else {
      row_letters <- unique(plate_data()$row)
      row_letters <- row_letters[order(row_letters)]
      selected_rows <- row_letters[which(row_letters == input$row_min):which(row_letters == input$row_max)]
      
      plate_data() %>%
        filter(inventory_item_name == input$model_name_text, 
               row %in% selected_rows, 
               column >= input$column_range[1] & column <= input$column_range[2])
    }
  })
  
  #render DT
  output$plate_map_table <- renderDT({
    datatable(filtered_data())
  })
  
  #download Data
  output$export_data <- downloadHandler(
    filename = function() {
      file_name_lower <- tolower(input$labguru_file$name)
      extracted_prefix <- strsplit(file_name_lower, "CPDM")[[1]][1]
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
    req(tecan_data())
    data <- tecan_data()
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
    data <- ctg_prep(file_path = input$ctg_file$datapath) # ctg_prep() function
    ctg_data(data)
  })
  
  output$ctg_data_table <- renderDT({ #ctg_data_table is output dt 
    req(ctg_data())
    datatable(ctg_data())
  })
  
  output$export_ctg_data <- downloadHandler(
    filename = function() {
      original_name <- tools::file_path_sans_ext(input$ctg_file$name)
      paste0(original_name, "_prep.xlsx")
    },
    content = function(file) {
      req(ctg_data())
      write.xlsx(ctg_data(), file)
    }
  )
  ## 5 join table 
  joined_data <- reactiveVal(NULL)
  
  # Observe file inputs, if plate_data() or tecan_data() is available AND growth_data() or ctg_data() 
  observe({
    enable_join_button <- (!is.null(plate_data()) || !is.null(tecan_data())) && (!is.null(growth_data()) || !is.null(ctg_data()))
    shinyjs::toggleState("join_datasets", condition = enable_join_button)
  })
  
  observeEvent(input$join_datasets, {
    print("Joining datasets...")
    joined_data(plate_data_join(
      labguru_plate_data_frame = if (!is.null(plate_data())) plate_data() else NULL,
      tecan_plate_data_frame = if (!is.null(tecan_data())) tecan_data() else NULL,
      growth_data_frame = if (!is.null(growth_data())) growth_data() else NULL,
      ctg_data_frame = if (!is.null(ctg_data())) ctg_data() else NULL
    ))
    # dropdowns based on joined data
    update_control_dropdowns()
  })
  
  update_control_dropdowns <- function() {
    data <- joined_data()
    if (!is.null(data)) {
      control_var <- if (input$control_location == "Treatment Name") "treatment_name" else "well_annotation"
      unique_values <- unique(data[[control_var]])
      choices <- c("None", unique_values)
      
      updateSelectInput(session, "media_control", choices = choices, selected = ifelse(any(grepl("Media", unique_values)), "Media", "None"))
      updateSelectInput(session, "negative_control", choices = choices, selected = ifelse(any(grepl("DMSO 0.5%", unique_values)), "DMSO 0.5%", "None"))
      updateSelectInput(session, "positive_control", choices = choices, selected = ifelse(any(grepl("DMSO 10%", unique_values)), "DMSO 10%", "None"))
    }
  }
  
  output$joined_data_table <- renderDT({
    req(joined_data())
    datatable(joined_data(), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$update_controls, {
    data <- joined_data()
    if (!is.null(data)) {
      control_var <- if (input$control_location == "Treatment Name") "treatment_name" else "well_annotation"
      if (control_var %in% names(data)) {
        data$treatment_type <- ifelse(data[[control_var]] == input$media_control, "Media Control",
                                      ifelse(data[[control_var]] == input$negative_control, "Negative Control",
                                             ifelse(data[[control_var]] == input$positive_control, "Positive Control",
                                                    ifelse(input$tecan_data == "Yes", "Monotherapy", "Combination Therapy"))))
        
        # Filter for max concentration if selected. for positive control
        if (input$positive_control_concentration == "Yes") {
          max_concentration <- data %>%
            filter(treatment_type == "Positive Control") %>%
            group_by(!!sym(control_var)) %>%
            summarize(max_concentration = max(concentration, na.rm = TRUE)) %>%
            pull(max_concentration)
          
          data <- data %>%
            filter(!(treatment_type == "Positive Control") | (concentration %in% max_concentration))
        }
        
        joined_data(data)
      }
    }
  })
  
  output$export_joined_data <- downloadHandler(
    filename = function() {
      # naming the file if Labguru is inported take "Plate_##" file name
      plate_num <- if (!is.null(input$labguru_file)) {
        tolower(gsub(".*(plate_\\d+).*", "\\1", input$labguru_file$name))
      } else {
        "tecan"
      }
      labguru_name <- if (input$filter_type == "Labguru Model Name") { #labguru_name
        tolower(input$model_name)
      } else {
        "custom_labguru_name"
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
      } else { # No Labguru Plate Map
        paste0("tecan_", data_type, drugging_suffix, "_joined.xlsx")
      }
    },
    # metadata in second sheet of joined data should have the following 
    content = function(file) {
      req(joined_data())  
      metadata <- data.frame(
        Growth_Metric_Units = input$growth_metric,
        Time_Units = input$time_unit,
        R_Version = R.version.string,
        Date = Sys.Date()
      )
      
      write_xlsx(list("Joined Data" = joined_data(), "Metadata" = metadata), path = file)
    }
  )
  ###
}

shinyApp(ui, server)