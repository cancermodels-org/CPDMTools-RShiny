source("global.R")

ui <- navbarPage(
  title = "CPDM Dashboard",
  theme = shinytheme("flatly"),
  id = "main_navbar",
  
  # Home Tab (Containing the Dashboard)
  tabPanel(
    "Data Wrangling RShiny",
    fluidPage(
      dashboardPage(
        dashboardHeader(title = "Data Wrangling RShiny"),
        dashboardSidebar(
          #SideBarPanels
          sidebarMenu(
            menuItem("Import Data", tabName = "import_data", icon = icon("upload")),
            menuItem("Labguru Plate Map", tabName = "labguru_tab"),
            menuItem("Tecan Report", tabName = "tecan_tab"),
            menuItem("Growth Data", tabName = "growth_tab"),
            menuItem("CTG", tabName = "ctg_tab"),
            menuItem("Joined Data", tabName = "joined_data_tab")
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(
              tabName = "import_data",
              fluidRow(
                #Data File Type
                box(
                  title = "Data File Type", status = "primary", solidHeader = TRUE, width = 6,
                  radioButtons("data_file_type", "Data File Type", choices = c("Imaging", "CTG"), selected = "Imaging"),
                  conditionalPanel(
                    condition = "input.data_file_type == 'Imaging'",
                    radioButtons("imaging_type", "Imaging File Type", choices = c("Incucyte", "Cytation"), selected = "Incucyte"),
                    fileInput("growth_file", "Import Growth File (.txt)"),
                    actionButton("import_growth", "Import File")
                  ),
                  conditionalPanel(
                    condition = "input.data_file_type == 'CTG'",
                    radioButtons("ctg_type", "CTG File Type", choices = c("GloMax Explorer", "SpectraMAX iD3"), selected = "GloMax Explorer"),
                    fileInput("ctg_file", "Import CTG File (.xlsx)"),
                    actionButton("import_ctg", "Import File")
                  )
                ),
                #Tecan Drugging Data
                box(
                  title = "Tecan Drugging Data", status = "primary", solidHeader = TRUE, width = 6,
                  radioButtons("tecan_data", "Tecan Drugging Data", choices = c("Yes", "No"), selected = "Yes"),
                  conditionalPanel(
                    condition = "input.tecan_data == 'Yes'",
                    radioButtons("drugging_type", "Drugging Type", choices = c("Monotherapy", "Synergy"), selected = "Monotherapy"),
                    fileInput("tecan_file", "Import Tecan Report File (.xlsx)"),
                    actionButton("import_tecan", "Import File"),
                    #downloadButton("data_wrangling_sample_dataset", "Download Sample Data"),
                  )
                )
              ),
              #Labguru Plate Map
              fluidRow(
                box(
                  title = "Labguru Plate Map", status = "primary", solidHeader = TRUE, width = 6,
                  fileInput("labguru_file", "Import Labguru Plate Map (.xlsx)"),
                  actionButton("import_labguru", "Import File")
                ),
                #Join Datasets
                box(
                  title = "Join Datasets", status = "primary", solidHeader = TRUE, width = 6,
                  actionButton("join_datasets", "Join Datasets")
                ),
                box(
                  title = "Sample Datasets", status = "primary", solidHeader = TRUE, width = 6,
                  downloadButton("data_wrangling_sample_dataset", "Download Sample Data")
                )
              )
            ),
            # TabPanels for each data start here 
            # 1 labguru
            tabItem(
              tabName = "labguru_tab",
              fluidRow(
                box(
                  title = "Filter Data By", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons("filter_type", "Filter Data By", choices = c("Labguru Model Name", "Well Range"), selected = "Labguru Model Name"),
                  conditionalPanel(
                    condition = "input.filter_type == 'Labguru Model Name'",
                    selectInput("model_name", "Select Model Name", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.filter_type == 'Well Range'",
                    textInput("model_name_text", "Enter Labguru Model Name", value = ""),
                    fluidRow(
                      column(6, selectInput("row_min", "Select Min Row Letter", choices = NULL, selected = NULL)),
                      column(6, selectInput("row_max", "Select Max Row Letter", choices = NULL, selected = NULL))
                    ),
                    
                    sliderInput("column_range", "Column Number Range", min = 1, max = 1, value = c(1, 1), step = 1, ticks = FALSE)
                    
                  ),
                  downloadButton("export_data", "Export Prepared Labguru Plate Map")
                ),
                box(
                  title = "Plate Map", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("plate_map_table")
                )
              )
            ),
            # 2 Tecan report
            tabItem(
              tabName = "tecan_tab",
              fluidRow(
                box(
                  status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6, 
                           sliderInput("plate_range", "Plate Number Range", min = 1, max = 1, value = c(1, 1), step = 1, ticks = FALSE))
                  ),
                  conditionalPanel(
                    condition = "output.labguru_present == false",
                    p("Warning: When Labguru plate map is not present, any well within the filtered row and column range of the Tecan plate map that did NOT contain dispensing data will be converted to Media Controls when joined with Imaging or CTG data (assumes your cells are plated in a complete rectangular matrix)."),
                    
                    sliderInput("column_number_range", "Column Number Range", min = 1, max = 1, value = c(1, 1), step = 1, ticks = FALSE, animate = FALSE), 
                    fluidRow( column(6, selectInput("min_letter_range", "Select Min Row Letter", choices = NULL, selected = NULL)),
                              column(6, selectInput("max_letter_range", "Select Max Row Letter", choices = NULL, selected = NULL))
                    ),
                  ),
                  fluidRow(
                    column(6, selectInput("concentration_units", "Concentration Units of Tecan Report", 
                                          choices = c("Molar (M)", "Millimolar (mM)", "Micromolar (µM)", 
                                                      "Nanomolar (nM)", "Picomolar (pM)"), 
                                          selected = "Micromolar (µM)"))
                  ),
                  downloadButton("export_tecan", "Export Prepared Tecan Report")
                ), 
                
                box(title = "Filter Options", status = "primary", solidHeader = TRUE, width = 12, 
                    DTOutput("tecan_plate_map_table")
                    
                )
                
              )
            ),
            # 3 Growth data 
            tabItem(
              tabName = "growth_tab",
              fluidRow(
                box(
                  title = "Select Time Point Before Treatment", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("time_point", "Please Select First Time Point Recorded Right Before Treatment Was Given", choices = NULL)
                ),
                box(
                  title = "Select Growth Metric Type", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("growth_metric", "Select Growth Metric Type of Growth File", 
                              choices = c("Confluency (%)", "Object Sum Area (um2)", "Largest Object Area (um2)", "Relative Fluorescence Intensity (AU)"), 
                              selected = "Confluency (%)")
                )
              ),
              fluidRow(
                box(
                  title = "Select Time Unit", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("time_unit", "Select Time Unit of Growth File", 
                              choices = c("Minutes", "Hours", "Days"), 
                              selected = "Hours")
                ),
                box(
                  title = "Export Data", status = "primary", solidHeader = TRUE, width = 6,
                  downloadButton("export_growth_data", "Export Prepared Growth Metric File")
                )
              ),
              fluidRow(
                box(
                  title = "Growth Data Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("growth_data_table")
                )
              )
            ),
            # CTG report
            tabItem(
              tabName = "ctg_tab",
              fluidRow(
                box(title = "Filter Data By", status = "primary", solidHeader = TRUE, width = 12,
                    conditionalPanel(
                      condition = "input.ctg_type == 'SpectraMAX iD3'",
                      radioButtons("filter_data_by", "Filter Data By",
                                   choices = c("CTG Plate Number Range", "CTG Plate Name"),
                                   selected = "CTG Plate Number Range"),
                      conditionalPanel(
                        condition = "input.filter_data_by == 'CTG Plate Number Range'",
                        sliderInput("plate_number_range", "Select CTG Plate Number Range", min = 1, max = 1, value = c(1, 1), step = 1, ticks = FALSE)
                      ),
                      conditionalPanel(
                        condition = "input.filter_data_by == 'CTG Plate Name'",
                        selectInput("plate_name", "Select CTG Plate Name", choices = NULL)
                      )
                    ),
                    downloadButton("export_ctg_data", "Export Prepared CTG Data")
                ),
                box(
                  title = "CTG Data Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("ctg_data_table")
                )
              )
            ),
            # Join Data
            tabItem(
              tabName = "joined_data_tab",
              ## add transfer_annotation
              fluidRow(
                box(
                  title = "Transfer Well Annotations", status = "primary", solidHeader = TRUE, width = 12,
                  conditionalPanel(condition = "output.showTransferButton",
                                   actionButton("transfer_annotations", "Transfer Well Annotations Separated by '-'"),
                                   br(), 
                                   helpText("This button will take well annotations separated by a '-' such as 'Drug A - 0.05' 
                                             and transfer the data such that 'Drug A' will go to the treatment_name column, 
                                              0.05 will go to the concentration column, and the treatment_type will be updated to 'Monotherapy'. 
                                              Well annotations must follow the convention 'Drug Name - Concentration' and the concentration unit 
                                              should be consistent between the Labguru plate map and the Tecan data if applicable.")
                                   
                  )
                )
              ),
              ###
              fluidRow(
                box(
                  title = "Control Variables", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons("control_location", "Select Location of Control Variables",
                               choices = c("Treatment Name", "Well Annotation"),
                               selected = "Treatment Name"), 
                  selectInput("media_control", "Please Select the Media Control", choices = NULL),
                  selectInput("negative_control", "Please Select the Negative Control", choices = NULL),
                  selectInput("positive_control", "Please Select the Positive Control", choices = NULL),
                  
                  radioButtons("positive_control_concentration", "Mark Highest Concentration as Positive Control", 
                               choiceValues=list("No","Yes"),
                               choiceNames=list("No","Yes"), 
                               selected ="Yes", inline=TRUE),
                  
                  actionButton("update_controls", "Update Controls")
                )
              ),
              fluidRow(
                box(
                  title = "Export Data", status = "primary", solidHeader = TRUE, width = 12,
                  downloadButton("export_joined_data", "Export Data")
                )
              ),
              fluidRow(
                box(
                  title = "Joined Data Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("joined_data_table")
                )
              )
              
            )
            
          ))))), 
  
  # Tab 2: Data QC RShiny (Integrated UI from previous “CPDM Data Quality Control App”)
  tabPanel(
    "Data QC RShiny",
    fluidPage(
      titlePanel("CPDM Data Quality Control App"),
      sidebarLayout(
        sidebarPanel(
          radioButtons("qc_data_type", "Data Type",
                       choices = c("Growth Data", "End-Point Assay Data"),
                       selected = "Growth Data"
          ),
          conditionalPanel(
            condition = "input.qc_data_type == 'Growth Data'",
            fileInput("qc_growth_file", "Import Growth Data (.xlsx, .csv, .txt)",
                      accept = c(".csv", ".xlsx", ".txt")
            )
          ),
          conditionalPanel(
            condition = "input.qc_data_type == 'End-Point Assay Data'",
            fileInput("ctg_file_qc", "Import End-Point Assay Data (.xlsx, .csv, .txt)",
                      accept = c(".csv", ".xlsx", ".txt")
            )
          ),
          
          downloadButton("sample_dataset", "Download Sample Data"), 
          hr(),
          h4("Exporting Data"),
          conditionalPanel(
            condition = "input.qc_data_type == 'Growth Data'",
            selectInput("outlier_manual_only", "Outliers to Exclude",
                        choices = c(
                          "Outliers Manually Annotated" = TRUE,
                          "Outliers Auto and Manually Annotated" = FALSE
                        ),
                        selected = TRUE
            ),
            checkboxInput("growthcurveme", "GrowthCurveME", value = TRUE),
            checkboxInput("lgrscore", "LGRscore", value = TRUE),
            checkboxInput("prism", "GraphPad PRISM", value = TRUE),
            downloadButton("export_growth_data", "Export Data")
          ),
          conditionalPanel(
            condition = "input.qc_data_type == 'End-Point Assay Data'",
            selectInput("outlier_manual_only_ctg", "Outliers to Exclude",
                        choices = c(
                          "Outliers Manually Annotated" = TRUE,
                          "Outliers Auto and Manually Annotated" = FALSE
                        ),
                        selected = TRUE
            ),
            checkboxInput("prism_ctg", "GraphPad PRISM", value = TRUE),
            downloadButton("qc_export_ctg_data", "Export Data")
          )
        ),
        
        mainPanel(
          tabsetPanel(
            id = "main_tabs",
            tabPanel(
              "Replicates and Concentrations",
              value = "tab_rep_conc",
              numericInput("round_by", "Round Concentrations By", value = 4),
              selectInput("use_nearest_10", "Round to Nearest 10",
                          choices = c("Yes" = TRUE, "No" = FALSE),
                          selected = TRUE
              ),
              actionButton("lock_round", "Lock In Rounded Concentrations"),
              DTOutput("rep_conc_table")
            ),
            #tabPanel("Growth Data QC", value = "tab_growth_qc"),
            tabPanel(
              "Growth Data QC",
              value = "tab_growth_qc",
              ## 1 
              fluidRow(
                column(4,
                       fluidRow(
                         column(12,
                                selectInput("treatment_name_gd", "Treatment Name", choices = NULL),
                                conditionalPanel(
                                  condition = "input.concentration_detected",
                                  selectInput("concentration", "Concentration", choices = c("All Concentrations"))),
                                numericInput("span_value", "Span Value", value = 0.3)
                         ))),
                
                ## 2
                column(4,
                       fluidRow(
                         column(12,
                                checkboxInput("show_outlier", "Show Outliers", value = TRUE),
                                checkboxInput("show_only_outlier_wells", "Show Only Outlier Wells", value = FALSE),
                                checkboxInput("make_interactive", "Make Interactive", value = TRUE)
                         ))),
                
                #3 
                column(4,
                       fluidRow(
                         column(12,
                                numericInput("residual_threshold", "Residual Threshold", value = 3),
                                textInput("growth_metric_name", "Growth Metric Name", value = "growth_metric"),
                                textInput("time_units", "Time Units", value = "hours")
                         )))),
              
              
              #######
              actionButton("run_update_outlier_detection_gd", "Run/Update Outlier Detection"),
              conditionalPanel(condition = "input.make_interactive == false",
                               plotOutput("growth_plot", click = "plot_click", brush = "plot_brush")
              ),
              conditionalPanel(condition = "input.make_interactive == true",plotlyOutput("growth_plotly")),
              
              div(
                style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
                
                actionButton("mark_as_outlier", "Mark as Outlier"),
                
                selectInput(
                  "outlier_reason", "Outlier Reason",
                  choices = c("Use Outlier Auto Flag Reason", "Imaging Error", "Masking Error", "Technical Error",  "Other"),
                  selected = "Use Outlier Auto Flag Reason"
                ),
                
                conditionalPanel(
                  condition = "input.outlier_reason == 'Other'",
                  div(
                    style = "margin: 0;",
                    textInput("outlier_reason_other", "Specify Reason")
                  )
                ),
                
                actionButton("mark_as_not_outlier", "Mark as Not an Outlier")
              ),
              
              DTOutput("selected_points_table")
              
              #####
              
            ),
            
            tabPanel(
              "Controls QC and Normalization",
              value = "tab_ctg_controls",
              checkboxInput("show_outlier", "Show Outliers", value = TRUE),
              checkboxInput("make_interactive", "Make Interactive", value = TRUE),
              uiOutput("positive_control_ui"),
              conditionalPanel(
                condition = "input.make_interactive == false",
                plotOutput("ctg_control_ggplot")
              ),
              conditionalPanel(
                condition = "input.make_interactive == true",
                plotlyOutput("ctg_control_plotly")
              ),
              actionButton("normalize_ctg", "Normalize Data")
            ),
            # End-Point Assay QC tab 
            tabPanel(
              "End-Point Assay QC", 
              value = "tab_ctg_qc",
              fluidRow(
                column(4,
                       fluidRow(
                         column(12,
                                selectInput("treatment_name_qc", "Treatment Name", choices = NULL),
                                checkboxInput("show_outlier_qc", "Show Outliers", value = TRUE),
                                checkboxInput("show_dose_response_curve", "Show Dose-Response Curve", value = TRUE),
                                checkboxInput("make_interactive", "Make Interactive", value = TRUE)
                         )
                       )
                ),
                column(4,
                       fluidRow(
                         column(12,
                                textInput("concentration_unit", "Concentration Unit", value = "µM"),
                                selectInput("method_init", "Curve-Fitting Technique",
                                            choices = c("Logistic" = "logistic", "Mead" = "mead"),
                                            selected = "logistic"),
                                selectInput("method_robust", "Outlier Detection Method",
                                            choices = c("Squared" = "squared", "Absolute" = "absolute", "Huber" = "Huber", "Tukey" = "Tukey"),
                                            selected = "Huber")
                         )
                       )
                ),
                column(4,
                       fluidRow(
                         column(12,
                                sliderInput("lb_if_min_gt", "Curve Fitting Boundaries (Min)", min = -2.5, max = 2.5, value = 0.3, step = 0.1),
                                sliderInput("ub_if_max_lt", "Curve Fitting Boundaries (Max)", min = -2.5, max = 2.5, value = 0.3, step = 0.1),
                                numericInput("z_score_threshold", "Z-Score Threshold (Mean/SD Outlier Detection)", value = 3)
                         )
                       )
                )
              ),
              div(style = "text-align: center;",
                  actionButton("run_update_outlier_detection", "Run/Update Outlier Detection")),
              conditionalPanel(
                condition = "input.make_interactive == false",
                plotOutput("ctg_plot", click = "plot_click", brush = "plot_brush")
              ),
              conditionalPanel(
                condition = "input.make_interactive == true",
                plotlyOutput("ctg_plotly")
              ),
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                
                actionButton("mark_as_outlier", "Mark as Outlier"),
                
                selectInput(
                  "outlier_reason", "Outlier Reason",
                  choices = c("Use Outlier Auto Flag Reason", "Edge-Effect", "Technical Error", "Other"),
                  selected = "Use Outlier Auto Flag Reason"
                ),
                
                conditionalPanel(
                  condition = "input.outlier_reason == 'Other'",
                  div(
                    style = "margin: 0;",  # ensures the textInput aligns better in flex layout
                    textInput("outlier_reason_other", "Specify Reason")
                  )
                ),
                actionButton("mark_as_not_outlier", "Mark as Not an Outlier")
              ),
              
              #DTOutput("updated_data_table")
              tableOutput("updated_data_table")
              
            ),
            ###tabPanel: Updated Dataset
            tabPanel("Updated Dataset", value = "tab_updated",
                     DTOutput("final_updated_data_table"))
          )
        )
      )
    )
  ),
  
  # TabPanel 3 Data analysis 
  tabPanel(
    "Data Analysis",
      fluidPage(
        titlePanel(""),
        sidebarLayout(
          sidebarPanel(
            radioButtons("data_type_data_analysis", "Data Type", choices = c("Growth Data Analysis", "End-Point Assay Data Analysis"), selected = "Growth Data Analysis"),
            
            conditionalPanel(
              condition = "input.data_type_data_analysis == 'Growth Data Analysis'",
              fileInput("growth_data_file", "Import Growth Data Analysis (.xlsx, .csv, .txt)"),
              textInput("growth_metric_name", "Growth Metric Name", value = "Confluency"),
              conditionalPanel(
                condition = "output.has_concentration_column",
                selectInput("concentration_unit_data_analysis", "Concentration Units", choices = c("M", "mM", "µM", "nM", "pM", "fM"), selected = "µM")
              ),
              textInput("time_unit_data_analysis", "Time Unit", value = "hours"),
              actionButton("run_growth", "Run")
            ),
            
            conditionalPanel(
              condition = "input.data_type_data_analysis == 'End-Point Assay Data Analysis'",
              fileInput("endpoint_data_file", "Import End-Point Assay Data Analysis (.xlsx, .csv, .txt)"),
              selectInput("readout", "Assay Readout", choices = c("Activity", "Inhibition"), selected = "Activity"),
              selectInput("method_init_endpoint", "Curve-Fitting Technique", choices = c("Logistic", "Mead"), selected = "Logistic"),
              sliderInput("lb_if_min_gt_endpoint", "Curve Fitting Boundaries Min", min = -2.5, max = 2.5, value = 0.3, step = 0.1),
              sliderInput("ub_if_max_lt_endpoint", "Curve Fitting Boundaries Max", min = -2.5, max = 2.5, value = 0.8, step = 0.1),
              selectInput("concentration_unit_data_analysis", "Concentration Units", choices = c("M", "mM", "µM", "nM", "pM", "fM"), selected = "µM"),
              textInput("y_axis_title", "Normalized Value Name", value = "Normalized Value"),
              sliderInput("activity_threshold", "Activity Threshold", min = 0, max = 100, value = 10, step = 1),
              numericInput("slope_threshold", "Slope Threshold", value = 0.1, min = 0),
              actionButton("run_endpoint", "Run")
            ),
            
            downloadButton("download_data_analysis", "Download Sample Data")
          ),
          
          mainPanel(
            tabsetPanel(
              id = "tabs",
              selected = "Growth Analysis Plots",  # Default selected tab
              
              tabPanel("Growth Analysis Plots",
                       fluidRow(
                         column(4,
                                fluidRow(
                                  column(12,
                                         selectInput("treatment_name_growth", "Treatment Name", choices = NULL),
                                         checkboxInput("show_controls", "Show Controls", value = FALSE),
                                         checkboxInput("make_interactive_data_analysis", "Make Interactive", value = FALSE),
                                         selectInput("display_metric", "Display Metric", 
                                                     choices = c("Wells" = "wells",
                                                                 "Means +/- SE" = "mean_se",
                                                                 "Mean Only" = "mean",
                                                                 "Median Only" = "median"),
                                                     selected = "wells")
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  column(12,
                                         numericInput("min_x_value", "Minimum X Value", value = NA),
                                         numericInput("max_x_value", "Maximum X Value", value = NA),
                                         numericInput("min_y_value", "Minimum Y Value", value = 0),
                                         numericInput("max_y_value", "Maximum Y Value", value = NA)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  column(12,
                                         sliderInput("n_x_axis_breaks_growthdata", "Number of X Axis Breaks", min = 3, max = 16, value = 8),
                                         sliderInput("n_y_axis_breaks_growthdata", "Number of Y Axis Breaks", min = 3, max = 16, value = 6)
                                  )
                                )
                         )
                       ),
                       uiOutput("plot_ui")
              ),
              
              tabPanel("End-Point Assay Results",
                       selectInput("result_figure", "Result Figure for All Treatments", choices = c("Table", "Dose-Response Plot", "Relative IC50 Forrest Plot", "Drug Sensitivity Score Bar Chart"), selected = "Table"),
                       
                       conditionalPanel(
                         condition = "input.result_figure == 'Table'",
                         uiOutput("result_table")
                       ),
                       
                       conditionalPanel(
                         condition = "input.result_figure == 'Dose-Response Plot'",
                         selectInput("x_scale", "X-Axis Scale", choices = c("Logarithmic", "Standard"), selected = "Logarithmic"),
                         selectInput("display_type", "Display Type", choices = c("Replicates", "Means +/- SE"), selected = "Means +/- SE"),
                         numericInput("min_y_value_end", "Minimum Y Value", value = 0),
                         numericInput("max_y_value_end", "Maximum Y Value", value = 1.5),
                         plotOutput("dose_response_plot")
                       ),
                       
                       conditionalPanel(
                         condition = "input.result_figure == 'Relative IC50 Forrest Plot'",
                         selectInput("x_scale_ic50", "X-Axis Scale", choices = c("Logarithmic", "Standard"), selected = "Logarithmic"),
                         plotOutput("ic50_forest_plot")
                       ),
                       
                       conditionalPanel(
                         condition = "input.result_figure == 'Drug Sensitivity Score Bar Chart'",
                         numericInput("score_label_size", "Score Label Value", value = 4),
                         plotOutput("dss_bar_plot")
                       )
              ),
              
              tabPanel("End-Point Assay Plots",
                       fluidRow(
                         column(4,
                                fluidRow(
                                  column(12,
                                         selectInput("treatment_name_end", "Treatment Name", choices = NULL),
                                         selectInput("dose_plot_sub_title", "Sub-Title Type", choices = c("None", "Relative IC50", "DSS3", "Relative IC50 and DSS3"), selected = "Relative IC50 and DSS3"),
                                         checkboxInput("make_interactive_end", "Make Interactive", value = FALSE)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  column(12,
                                         selectInput("x_scale_end", "X-Axis Scale", choices = c("Logarithmic", "Standard"), selected = "Logarithmic"),
                                         selectInput("display_type_end", "Display Type", choices = c("Replicates", "Means +/- SE"), selected = "Means +/- SE"),
                                         sliderInput("n_x_axis_breaks_end", "Number of X Axis Breaks", min = 3, max = 16, value = 6)
                                  )
                                )
                         ),
                         column(4,
                                fluidRow(
                                  column(12,
                                         numericInput("min_y_value_end2", "Minimum Y Value", value = 0),
                                         numericInput("max_y_value_end2", "Maximum Y Value", value = 1.5),
                                         sliderInput("n_y_axis_breaks_end", "Number of Y Axis Breaks", min = 3, max = 16, value = 7)
                                  )
                                )
                         )
                       ),
                       uiOutput("endpoint_plot_ui")
              ),
              
              tabPanel("Export and Report",
                       fluidRow(
                         column(6, 
                                textInput("report_title_dt", "Report Title", value = "Endpoint Assay Results")
                         ),
                         column(6,
                                textInput("report_sub_title_dt", "Report Sub-Title", value = "End-Point Assay Analysis Report")
                         )
                       ),
                       fluidRow(
                         column(6, 
                                numericInput("fig_width_in_dt", "Width:", value = 7.5)
                         ),
                         column(6,
                                numericInput("fig_height_in_dt", "Height:", value = 5.5)
                         )
                       ),
                       br(),
                       div(
                         style = "text-align: center;", downloadButton("export_results", "Export Results and Figures")
                       )
              )
            )
          )
        )
        
      )
    
    
    ),
  
  # placeholder for discription, About and info page
  tabPanel(
    "About and Info",
    fluidPage(
      titlePanel("About This Application")
    )
  )
)
