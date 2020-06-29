credit_roles <- c("Conceptualization",
                  "Data curation",
                  "Formal analysis",
                  "Funding acquisition",
                  "Investigation",
                  "Methodology",
                  "Project administration",
                  "Resources",
                  "Software",
                  "Supervision",
                  "Validation",
                  "Visualization",
                  "Writing - original draft",
                  "Writing - review & editing")

# . authors tab ----
authors_tab <- tabItem(
  tabName = "authors_tab",
  h2("Authors"),
  htmlOutput("author_list"),

  h3("Add an Author"),
  textInput("surname", "Given Name(s) including initials"),
  textInput("given", "Last Name(s)"),
  textInput("orcid", "ORCiD (see https://orcid.org/)"),
  checkboxGroupInput("roles", "Contributor Roles (see https://casrai.org/credit/)", credit_roles),
  actionButton("add_author", "Add Author"),

  h3("Contributor Roles"),
  HTML("<p>See <a href='https://casrai.org/credit/' target='_blank'>Casrai CReDiT Contributor Roles Taxonomy</a></p>"),
  tags$ul(
    tags$li("Conceptualization: Ideas; formulation or evolution of overarching research goals and aims."),
    tags$li("Data curation: Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use."),
    tags$li("Formal analysis: Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data."),
    tags$li("Funding acquisition: Acquisition of the financial support for the project leading to this publication."),
    tags$li("Investigation: Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection."),
    tags$li("Methodology: Development or design of methodology; creation of models."),
    tags$li("Project administration: Management and coordination responsibility for the research activity planning and execution."),
    tags$li("Resources: Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools."),
    tags$li("Software: Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components."),
    tags$li("Supervision: Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team."),
    tags$li("Validation: Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs."),
    tags$li("Visualization: Preparation, creation and/or presentation of the published work, specifically visualization/data presentation."),
    tags$li("Writing - original draft: Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation)."),
    tags$li("Writing - review & editing: Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision -- including pre- or post-publication stages.")
  )
)
