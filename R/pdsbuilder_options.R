#' Set/get global pdsbuilder options
#'
#' Global afex options are used, for example, to set the default separator for cell names.
#'
#' @param ... One of four: (1) nothing, then returns all options as a list; (2) a name of an option element, then returns its value; (3) a name-value pair which sets the corresponding option to the new value (and returns nothing), (4) a list with option-value pairs which sets all the corresponding arguments.
#'
#' @return a list of options, values of an option, or nothing
#' @export
#'
#' @examples
#'
#' pdsbuilder_options() # see all options
#'
#' pdsbuilder_options("sep") # see value of pdsbuilder.sep
#' \dontrun{
#' # changes cell separator (e.g., A1.B2)
#' pdsbuilder_options(sep = ".")
#' }
pdsbuilder_options <- function (...) {
  # code from afex::afex_options
  dots <- list(...)
  if (length(dots) == 0) {
    op <- options()
    pdsbuilder_op <- op[grepl("^pdsbuilder.", names(op))]
    names(pdsbuilder_op) <- sub("^pdsbuilder.", "", names(pdsbuilder_op))
    return(pdsbuilder_op)
  } else if (is.list(dots[[1]])) {
    newop <- dots[[1]]
    names(newop) <- paste0("pdsbuilder.", names(newop))
    options(newop)
  } else if (!is.null(names(dots))) {
    newop <- dots
    names(newop) <- paste0("pdsbuilder.", names(newop))
    options(newop)
  } else if (is.null(names(dots))) {
    if (length(dots) > 1)
      stop("pdsbuilder_options() can only return the value of a single option.",
           call. = FALSE)
    return(getOption(paste0("pdsbuilder.", unlist(dots))))
  } else {
    warning("Unsupported command to pdsbuilder_options(), nothing done.",
            call. = FALSE)
  }
}
