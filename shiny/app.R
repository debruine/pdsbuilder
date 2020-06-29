# Libraries ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# Functions ----
source("func.R")

# Define UI ----

# . Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Psych-DS", tabName = "main_tab"),
    menuItem("Upload Data", tabName = "upload_tab"),
    menuItem("Codebook", tabName = "cb_tab")
  )
)

# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  p("Upload data in the Data Upload tab and download the codebook JSON file in the Codebook tab."),
  p("More instructions and intro...")
)

# . cb_tab ----
cb_tab <- tabItem(
  tabName = "cb_tab",
  downloadLink("downloadCB", "Download"),
  HTML("<pre id='codebook' class='shiny-text-output'></pre>")
)

# . upload_tab ----
upload_tab <- tabItem(
  tabName = "upload_tab",
  fluidRow(
    box(
      title = "Upload Data",
      width = 12,
      p("This is a working demo and many functions do not work yet."),
      fileInput("inFile", "CSV/XLS(X) Data File", 
                multiple = FALSE, width = NULL,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values,text/plain',
                  '.csv',
                  '.xls',
                  '.xlsx'
                ), 
                buttonLabel = "Browse...", 
                placeholder = "No file selected"
      ),
      checkboxInput("header", "Data file has a header", TRUE),
      DTOutput("rawdata_table")
    )
  )
)

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Psych-DS Codebook"),
  sidebar,
  dashboardBody(
    tabItems(
      main_tab,
      upload_tab,
      cb_tab
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## Load data ----
  v <- reactiveValues(
    rawdata = NULL,
    cb = "",
    file_name = ""
  )
  
  dat <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(NULL)
    
    file_extension <- tools::file_ext(inFile$datapath)
    if (file_extension == "csv") {
      rawdata <- read.csv(inFile$datapath, header = input$header)
    } else if (file_extension %in% c("xls", "xlsx")) {
      rawdata <- as.data.frame(readxl::read_excel(inFile$datapath, 
                                                   col_names = input$header))
    } else if (file_extension %in% c("sav")) {
      rawdata <- haven::read_sav(inFile$datapath)
    } else if (file_extension %in% c("sas")) {
      rawdata <- haven::read_sas(inFile$datapath)
    }
    
    #save file name as global variable for writing
    file_name <- gsub(paste0("." , file_extension), "", inFile$name)
    
    # create initial codebook
    cb <- codebook(rawdata, file_name, return = "json")

    list(data = rawdata,
         cb = cb,
         file_name = file_name)
  })
  
  output$rawdata_table <- renderDataTable({
    datatable(dat()$data, rownames = F)
  })
  
  output$codebook <- renderText({
    as.character(dat()$cb)
  })
  
  output$downloadCB <- downloadHandler(
    filename = function() {
      paste0(dat()$file_name, ".json")
    },
    content = function(file) {
      dat()$cb %>%
        jsonlite::prettify(4) %>%
        writeLines(file)
    }
  )
  
}

# Run the application ----
shinyApp(ui = ui, server = server)

