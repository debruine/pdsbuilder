# Libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(DT)
library(pdsbuilder)
library(shiny.i18n)

translator <- Translator$new(translation_json_path = "i18n/translation.json")

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
    menuItem("Dataset Descriptions", tabName = "dataset_tab"),
    menuItem("Variable Descriptions", tabName = "vars_tab"),
    menuItem("Authors", tabName = "authors_tab"),
    menuItem("Codebook", tabName = "cb_tab"),
    menuItem("Licenses", tabName = "license_tab")
  ),
  uiOutput("lang")
)

# . main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  uiOutput("instructions"),
  tags$a(href="https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit#heading=h.caxnnxqaobj", "Technical Spec"),
  p("The translation is not fully set up yet, so only some text will be translated.")
)


# . vars tab ----

vars_tab <- tabItem(
  tabName = "vars_tab",
  uiOutput("vars_instructions"),
  fluidRow(
    box(
      width = 12,
      fileInput("inFile", "Data File",
            multiple = FALSE, width = NULL,
            accept = c(
              'text/csv',
              'text/comma-separated-values,text/plain',
              '.csv', '.tsv', '.txt', '.sav', '.xls', '.xlsx'
            ),
            buttonLabel = "Browse...",
            placeholder = "No file selected"
      ),
      checkboxInput("header", "Data file has a header", TRUE),
      DTOutput("data_table")
    ),
    box(
      width = 12,
      uiOutput("var_list")
    ),
    box(
      width = 12,
      title = "Variable Descriptions (for debugging)",
      HTML("<pre id='vardesc' class='shiny-text-output'></pre>")
    )
  )
)

# . cb_tab ----
cb_tab <- tabItem(
  tabName = "cb_tab",
  downloadButton("downloadCB", "Download Codebook", width = 4),
  downloadButton("downloadTSV", "Download Data as TSV", width = 4),
  box(width = 12,
      title = "Codebook in JSON format",
      HTML("<pre id='codebook' class='shiny-text-output'></pre>")
  )
)

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Psych-DS Codebook",
                  dropdownMenuOutput("notificationsMenu")),
  sidebar,
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "pds.js")
    ),
    tabItems(
      main_tab,
      dataset_tab,
      vars_tab,
      authors_tab,
      cb_tab,
      license_tab
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  ## . translation ----
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # trigger input label changes on language change
  observeEvent(input$lang, {
    ip <- list(
      updateTextInput = c(name = "Dataset Name",
            schemaVersion = "Schema Version",
            license_free = "Custom License",
            citation = "Citation",
            funder = "Funder",
            url = "URL",
            identifier_doi = "DOI (Document Object Identifier)",
            identifier_issn = "ISSN (International Standard Serial Number)",
            identifier_pmid = "PMID (PubMed ID)",
            identifier ="Other Identifier",
            keywords = "Keywords (separate with commas)"),
      updateSelectInput = c(license = "License",
                            privacyPolicy = "Privacy Policy"),
      updateTextAreaInput = c(description = "Description"),
      updateCheckboxInput = c(header = "Data file has a header Name"),
      updateActionButton = c(author_reorder = "Reorder Authors",
                             add_author = "Add Author",
                             downloadCB = "Download Codebook",
                             downloadTSV = "Download Data as TSV ")
    )

    for (func in names(ip)) {
      for (nm in names(ip[[func]])) {
        args <- list(
          session = session,
          inputId = nm,
          label = i18n()$t(ip[[func]][[nm]])
        )
        do.call(func, args)
      }
    }
  })

  output$lang <- renderUI({
    selectInput('lang',
                 i18n()$t("Change language"),
                 choices = translator$languages,
                 selected = input$lang)
  })
  output$instructions <- renderUI({
    "Upload data in the Data Upload tab and download the codebook JSON file in the Codebook tab." %>% i18n()$t()
  })
  output$dataset_instructions <- renderUI({
    "Fill in any information about the dataset." %>% i18n()$t()
  })
  output$vars_instructions <- renderUI({
    "Upload data and edit the default variable information" %>% i18n()$t()
  })

  ## . setup ----
  observeEvent(input$license, {
    if (input$license == "Other (write in)" %>% i18n()$t()) {
      shinyjs::show("license_free")
    } else {
      shinyjs::hide("license_free")
    }
  })
  shinyjs::hide("author_reorder")
  shinyjs::hide("author_n")

  ## . reactive values ----
  filename <- reactiveVal("file")
  newdata  <- reactiveVal(data.frame())
  vardesc  <- reactiveVal(list())
  notifications <- reactiveVal(list())
  authors <- reactiveVal(list())

  ## . load data ----
  rawdata <- reactive({
    inFile <- input$inFile
    if (is.null(inFile)) return(data.frame())

    file_extension <- tools::file_ext(inFile$datapath)
    txt_ext <- c("csv", "txt", "tsv")
    xls_ext <- c("xls", "xlsx")
    other_ext <- c("sav")
    if (file_extension %in% txt_ext) {
      rawdata <- rio::import(inFile$datapath, header = input$header)
    } else if (file_extension %in% xls_ext) {
      rawdata <- rio::import(inFile$datapath, col_names = input$header)
    } else if (file_extension %in% other_ext) {
      rawdata <- rio::import(inFile$datapath)
    } else {
      return(data.frame())
    }

    paste0("." , file_extension) %>%
      gsub("", inFile$name) %>%
      filename()
    if (input$name == "") {
      updateTextInput(session, "name", value = filename())
    }

    #newdata(rawdata) # not working
    rawdata
  })

  # . set up codebook ----
  cb <- reactive({
    # create initial codebook
    props <- list()
    props$data <- rawdata()
    props$return <- "json"

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

    a <- authors()
    if (length(a) > 0) {
      for (i in 1:length(a)) {
        a[[i]] <- c("@type" = "Person", a[[i]])
        if (isFALSE(a[[i]]$orcid)) a[[i]]$orcid <- NULL
        if (length(a[[1]]$roles) == 0) a[[i]]$roles <- NULL
      }
      props$author <- a
    }

    props$vardesc <- vardesc()

    do.call(codebook, props)
  })

  output$data_table <- renderDataTable({
    datatable(rawdata(), rownames = F)
  })

  output$codebook <- renderText({
    as.character(cb())
  })

  # . download codebook
  output$downloadCB <- downloadHandler(
    filename = function() {
      paste0(filename(), ".json")
    },
    content = function(file) {
      cb() %>%
        jsonlite::prettify(4) %>%
        writeLines(file)
    }
  )

  # . download TSV
  output$downloadTSV <- downloadHandler(
    filename = function() {
      paste0(filename(), ".tsv")
    },
    content = function(file) {
      write.table(rawdata(), file, sep = "\t")
    }
  )

  # . vardesc ----
  observeEvent(input$inFile, {
    cb <- codebook(iris, return = "list")
    vm <- cb$variableMeasured
    vardesc <- list(
      name = sapply(vm, function(x) { x$name }),
      description = sapply(vm, function(x) { x$description }),
      type = sapply(vm, function(x) { x$type })
    )
    names(vardesc$description) <- vardesc$name
    names(vardesc$type) <- vardesc$name

    vardesc(vardesc)

    output$vardesc <- renderText(nested_list(vardesc()))

    output$var_list <- renderUI({
      sapply(vardesc$name, function(x) {
        paste0("<button>", x, "</button>")
      }) %>% paste(collapse = "\n")
    })
  })

  # . notifications ----
  output$notificationsMenu <- renderMenu({
    dropdownMenu(type = "notifications", .list = notifications())
  })

  # . authors ----
  observeEvent(input$add_author, {
    problems <- FALSE

    # check orcid
    orcid <- check_orcid(input$orcid)
    if (isFALSE(orcid) & input$orcid != "") {
      problems <- TRUE
      updateTextInput(session, "orcid",
                      label = "ORCiD is not valid" %>% i18n()$t())
      shinyjs::addClass("orcid", "warning")
    }

    # check names
    for (nm in c("surname", "given")) {
      if (trimws(input[[nm]]) == "") {
        problems <- TRUE
        label <- ifelse(nm == "given",
                        "Given name",
                        "Last name") %>%
                 paste("is missing") %>% i18n()$t()
        updateTextInput(session, nm, label = label)
        shinyjs::addClass(nm, "warning")
      }
    }

    if (!problems) {
      # add author
      a <- list(surname = trimws(input$surname),
                given = trimws(input$given),
                orcid = orcid,
                roles = input$roles)
      aa <- authors()
      aa[[input$author_n]] <- a
      authors(aa)


      # reset values
      updateTextInput(session, "author_n", value = length(aa)+1)
      updateTextInput(session, "given", value = "",
                      label = "Given Name(s) including initials" %>% i18n()$t())
      updateTextInput(session, "surname", value = "",
                      label = "Last Name(s)" %>% i18n()$t())
      updateTextInput(session, "orcid", value = "",
                      label = "ORCiD" %>% i18n()$t())
      updateCheckboxGroupInput(session, "roles", selected = character(0))
      shinyjs::removeClass("given", "warning")
      shinyjs::removeClass("surname", "warning")
      shinyjs::removeClass("orcid", "warning")
    }
  }, ignoreNULL = TRUE)

  # . . author_list ----
  output$author_list <- renderUI({
    a <- authors()
    if (length(a) > 1) {
      shinyjs::show("author_reorder")
    } else {
      shinyjs::hide("author_reorder")
    }
    make_author_list(a)
  })

  # . . author_reorder ----
  observeEvent(input$author_reorder, {
    ord <- strsplit(input$author_order, ",")[[1]] %>% as.integer()
    if (length(unique(ord)) != length(ord)) {
      js <- sprintf('alert("%s");',
                     i18n()$t("Each author must have a unique order"))
      runjs(js)
    } else {
      a <- authors()
      authors(a[ord])
    }
  }, ignoreNULL = TRUE)

  # . . author_edit ----
  observeEvent(input$author_edit, {
    to_edit <- as.integer(input$author_edit)
    message(to_edit)
    a <- authors()[[to_edit]]
    updateTextInput(session, "author_n", value = to_edit)
    updateTextInput(session, "given", value = a$given,
                    label = "Given Name(s) including initials" %>% i18n()$t())
    updateTextInput(session, "surname", value = a$surname,
                    label = "Last Name(s)" %>% i18n()$t())
    updateTextInput(session, "orcid",
                    value = ifelse(isFALSE(a$orcid), "", a$orcid),
                    label = "ORCiD" %>% i18n()$t())
    updateCheckboxGroupInput(session, "roles", selected = a$roles)
    shinyjs::removeClass("given", "warning")
    shinyjs::removeClass("surname", "warning")
    shinyjs::removeClass("orcid", "warning")
  }, ignoreNULL = TRUE)

  # . . author_delete ----
  observeEvent(input$author_delete, {
    to_del <- as.integer(input$author_delete)
    a <- authors()
    a[to_del] <- NULL
    authors(a)
  }, ignoreNULL = TRUE)
}

# Run the application ----
shinyApp(ui = ui, server = server)

