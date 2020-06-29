#' Launch Shiny App
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
pds_app <- function(...) {
  shiny::runApp(appDir = system.file("application", package = "pdsbuilder"), ...)
}
