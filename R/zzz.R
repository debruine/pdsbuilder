## set default options for faux_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.pdsbuilder <- list(
    pdsbuilder.connection = stdin(),
    pdsbuilder.sep = "_",
    pdsbuilder.verbose = TRUE
  )
  toset <- !(names(op.pdsbuilder) %in% names(op))
  if(any(toset)) options(op.pdsbuilder[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  txt <- paste(
    "\n************",
    "Welcome to pdsbuilder For support and examples visit:",
    "http://debruine.github.io/pdsbuilder/",
    "- Get and set global package options with: pdsbuilder_options()",
    "************",
    sep = "\n"
  )

  packageStartupMessage(txt)
}
