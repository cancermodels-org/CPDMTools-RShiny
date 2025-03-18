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
        dashboardHeader(title = "CPDM Data Wrangling RShiny"),
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
                    actionButton("import_tecan", "Import File")
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
                    
                    sliderInput("column_number_range", "Column Number Range", min = 1, max = 1, value = c(1, 1), step = 1, ticks = FALSE, animate = TRUE), 
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
              )
            ),
            # 3 Growth data 
            tabItem(
              tabName = "growth_tab",
              fluidRow(
                box(
                  title = "Select Time Point After Treatment", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("time_point", "Please Select First Time Point Recorded After Treatment Was Given", choices = NULL)
                ),
                box(
                  title = "Select Growth Metric Type", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("growth_metric", "Select Growth Metric Type of Growth File", 
                              choices = c("Confluency (%)", "Object Sum Area (µM2)", "Largest Object Area (µM2)", "Relative Fluorescence Intensity (AU)"), 
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
                box(
                  title = "Export Data", status = "primary", solidHeader = TRUE, width = 12,
                  downloadButton("export_ctg_data", "Export Prepared CTG Data")
                )
              ),
              fluidRow(
                box(
                  title = "CTG Data Table", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("ctg_data_table")
                )
              )
              
            ),
            # Join Data
            tabItem(
              tabName = "joined_data_tab",
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
            
          ))))), # tab one Data Wrangling interface end here 
  
  # TabPanel 2 placeholder for QC Rshiny 
  tabPanel(
    "Data QC RShiny",
    fluidPage(
      titlePanel("Data QC RShiny Content")
    )
  ),
  
  # TabPanel 3 placeholder for Graphs and CTG analysis 
  tabPanel(
    "Data Analysis RShiny"
  ),
  
  # placeholder for discription, About and info page
  tabPanel(
    "About and Info",
    fluidPage(
      titlePanel("About This Application")
    )
  )
)


