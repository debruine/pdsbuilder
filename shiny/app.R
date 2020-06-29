# Libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(DT)
library(pdsbuilder)

# Functions and Tabs----
source("func.R")
source("licenses.R")
source("upload.R")
source("authors.R")

# Define UI ----

# . Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Psych-DS", tabName = "main_tab"),
    menuItem("Upload Data", tabName = "upload_tab"),
    menuItem("Authors", tabName = "authors_tab"),
    menuItem("Codebook", tabName = "cb_tab"),
    menuItem("Licenses", tabName = "license_tab")
  )
)

# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  p("Upload data in the Data Upload tab and download the codebook JSON file in the Codebook tab."),
  selectInput("lang", "Language", c("English" = "en",
                                    "Test" = "test"),
              selected = "en"),
  p("More instructions and intro...")
)


# . vars tab ----

vars_tab <- tabItem(
  tabName = "vars_tab",
  p("Describe your variables here")
)

# . cb_tab ----
cb_tab <- tabItem(
  tabName = "cb_tab",
  downloadLink("downloadCB", "Download"),
  HTML("<pre id='codebook' class='shiny-text-output'></pre>")
)

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Psych-DS Codebook",
                  dropdownMenuOutput("notificationsMenu")),
  sidebar,
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      main_tab,
      upload_tab,
      authors_tab,
      cb_tab,
      license_tab
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## setup ----
  observeEvent(input$license, {
    if (input$license == "Other (write in)") {
      shinyjs::show("license_free")
    } else {
      shinyjs::hide("license_free")
    }
  })

  ## localisation ----
  observeEvent(input$lang, {
  })

  ## Load data ----
  rawdata <- reactive({
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

    file_name <- gsub(paste0("." , file_extension), "", inFile$name)
    if (input$name == "") {
      updateTextInput(session, "name", value = file_name)
    }

    rawdata
  })

  # . set up codebook ----
  cb <- reactive({
    # create initial codebook
    props <- list()
    props$data <- rawdata()
    props$return <- "json"

    if (is.null(props$data)) return(NULL)

    dsmeta <- c("name", "description", "schemaVersion", "license", "citation", "funder", "url", "privacyPolicy")

    for (m in dsmeta) {
      val <- input[[m]]
      if (val != "") {
        props[m] <- val
      }
    }

    #props$identifer <- list()
    if (input[["identifier_doi"]] != "") {
      props$identifier$DOI <- input[["identifier_doi"]]
    }
    if (input[["identifier_issn"]] != "") {
      props$identifier$ISSN <- input[["identifier_issn"]]
    }
    if (input[["identifier_pmid"]] != "") {
      props$identifier$PMID <- input[["identifier_pmid"]]
    }
    if (input[["identifier"]] != "") {
      if (length(props$identifier) == 0) {
        props$identifier <-  input[["identifier"]]
      } else {
        props$identifier["other"] <- input[["identifier"]]
      }
    }

    keywords <- strsplit(input$keywords, "\\s*,\\s*")
    if (length(keywords[[1]]) > 0) props$keywords <- keywords[[1]]

    do.call(codebook, props)
  })

  output$rawdata_table <- renderDataTable({
    datatable(rawdata(), rownames = F)
  })

  output$codebook <- renderText({
    as.character(cb())
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

  # . notifications

  notifications <- reactiveVal(list())
  output$notificationsMenu <- renderMenu({
    message(notifications())
    dropdownMenu(type = "notifications", .list = notifications())
  })

  # . authors
  authors <- reactiveVal(list())

  observeEvent(input$add_author, {
    problems <- FALSE
    orcid <- check_orcid(input$orcid)
    if (isFALSE(orcid) & input$orcid != "") {
      problems <- TRUE
      txt <- sprintf("The ORCiD for %s %s is not valid",
                     input$given, input$surname)
      msg <- notificationItem(txt,
                              icon = icon("exclamation-triangle"),
                              status = "warning")
      notifications(c(notifications(), list(msg)))
    }

    a <- list(list(surname = input$surname,
                 given = input$given,
                 orcid = orcid,
                 roles = input$roles))
    authors(c(authors(), a))

    if (!problems) {
      # reset values
      updateTextInput(session, "given", value = "")
      updateTextInput(session, "surname", value = "")
      updateTextInput(session, "orcid", value = "")
    }
  })

  output$author_list <- renderUI({
    i <- 0
    alist <- list()
    for (a in authors()) {
      i <- i + 1
      ab <- actionButton(paste0("author_", i), "Delete")

      txt <- sprintf("%s, %s %s: %s",
                     a$surname, a$given,
                     ifelse(a$orcid, a$orcid, ""),
                     paste(a$roles, collapse = ", "))

      alist[[i]] <- tags$li(txt)
    }

    do.call(tags$ol, alist)
  })

}

# Run the application ----
shinyApp(ui = ui, server = server)

