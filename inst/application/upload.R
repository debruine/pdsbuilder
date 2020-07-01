licenses <- c(
  "Public Domain",
  "CC0-1.0",      # "Creative Commons Zero v1.0 Universal",
  "PDDL",         # "Open Data Commons Public Domain Dedication and License"
  "CC-BY-4.0",    # "CC Attribution 4.0 International",
  "CDLA-Permissive-1.0", # Community Data License Agreement – Permissive
  "ODC-BY",       # "Open Data Commons Attribution License",
  "CC-BY-SA-4.0", # "CC-BY Share Alike 4.0 International",
  "CDLA-Sharing-1.0",
  "ODC-ODbL",     # "Open Data Commons Open Database License",
  "CC-BY-NC-4.0", # "CC-BY Non Commercial 4.0 International",
  "CC-BY-ND-4.0", # "CC-BY No Derivatives 4.0 International",
  "CC-BY-NC-SA-4.0",
  "CC-BY-NC-ND-4.0",
  "Other (write in)"
)

# . dataset_tab ----
dataset_tab <- tabItem(
  tabName = "dataset_tab",
  htmlOutput("dataset_instructions"),
  fluidRow(
    box(
      width = 12,
      textInput("name", "Dataset Name"),
      textAreaInput("description", "Description"),
      selectInput("schemaVersion", "Schema Version",
                  c("Psych-DS 0.1.0"), selected = "Psych-DS 0.1.0"),
      selectInput("license", "License", licenses),
      textInput("license_free", "Customm License"),
      textInput("citation", "Citation"),
      textInput("funder", "Funder"),
      textInput("url", "URL"),
      textInput("identifier_doi", "DOI"),
      textInput("identifier_issn", "ISSN"),
      textInput("identifier_pmid", "PMID"),
      textInput("identifier", "Other Identifier"),
      selectInput("privacyPolicy", "Privacy Policy",
                  c("Open" = "open",
                    "Open (deidentified)" = "open_deidentified",
                    "Open (redacted)" = "open_redacted",
                    "Private" = "private")),
      textInput("keywords", "Keywords (separate with commas)")
    )
  )
)

