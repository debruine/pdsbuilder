#' Create PsychDS Codebook from Data
#'
#' @param data The data frame to generate a codebook for
#' @param name The name of this dataset (if NULL, will be the same as `data`)
#' @param vardesc Optional variable properties in the format of a named list of vectors (can be named or unnamed and in the same order as the data) from the options description, privacy, type, propertyID, minValue, maxValue, levels, ordered, na, naValues, alternateName, unitCode
#' @param ... Further dataset properties (e.g., description, license, author, citation, funder, url, doi/sameAs, keywords, temporalCoverage, spatialCoverage, datePublished, dateCreated)
#' @param schemaVersion defaults to "Psych-DS 0.1.0"
#' @param return Whether the output should be in JSON format (json), a list (list) or the reformatted data with the codebook as an attribute (data)
#'
#' @return a list or json-formatted codebook, or reformatted data withthe codebook as an attribute
#' @export
#'
#' @examples
#'
#' vardesc = list(
#'   description = c("Length of the sepal",
#'                   "Width of the sepal",
#'                   "Length of the petal",
#'                   "Width of the petal",
#'                   "The flower species"),
#'   type = c("float", "float", "float", "float", "string")
#' )
#' codebook(iris, vardesc = vardesc)
#'
codebook <- function(data, name = NULL, vardesc = list(), ...,
                     schemaVersion = "Psych-DS 0.1.0", 
                     return = c("json", "list", "data")
) {
  # use PsychDS format from https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit
  
  if (is.null(name)) {
    name <- utils::capture.output(match.call()$data)
  }
  
  # all possible type abbreviations ----
  types <- c(i = "int", 
             int = "int",
             integer = "int", 
             
             s = "string",
             string = "string",
             factor = "string",
             char = "string", 
             character = "string",
             
             f = "float", 
             float = "float",
             double = "float",
             numeric = "float",
             
             b = "bool",
             bool = "bool",
             boolean = "bool",
             logical = "bool")
  
  schema <- list(
    "@context" = "https://schema.org/",
    "@type" = "Dataset",
    name = name,
    schemaVersion = schemaVersion
  )
  
  # check datadesc
  datadesc <- list(...)
  
  # fix doi and relabel as sameAs ----
  if (!is.null(datadesc$doi)) {
    doi <- tolower(datadesc$doi) %>%
      gsub("\\s", "", .) %>%
      sub("^doi\\:", "", .) %>%
      sub("^https://doi.org/", "", .)
    datadesc$doi <- NULL
    datadesc$sameAs <- paste0("https://doi.org/", doi)
  }
  
  possible_vals <- c("license", "author", "citation", "funder", "url", "sameAs", "keywords", "temporalCoverage", "spatialCoverage", "datePublished", "dateCreated")
  non_standard <- setdiff(names(datadesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following dataset properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # add data properties ----
  schema <- c(schema, datadesc)
  
  # get vardesc from design
  design <- attr(data, "design")
  if (!is.null(design)) {
    win <- ifelse(length(design$within) > 0, 
                  names(design$within), "")
    if (win %in% names(data)) {
      # in long format
      lvls <-  c(design$between, design$within)
      descs <- design$id
    } else {
      lvls <- design$between
      descs <- c(design$id, design$dv)
    }
    
    # overwrite levels and descriptions in design with vardesc
    if (length(lvls) > 0) { 
      # only run this if there are any factor columns
      for (n in names(vardesc$levels)) {
        lvls[[n]] <- as.list(vardesc$levels[[n]])
      }
      vardesc$levels <- lvls
    }
    for (n in names(vardesc$description)) {
      descs[[n]] <- vardesc$description[[n]]
    }
    vardesc$description <- descs
  }
  
  # check vardesc ----
  possible_vals <- c("description", "privacy", "type",
                     "propertyID", "minValue", "maxValue",
                     "levels", "ordered", "na", "naValues",
                     "alternateName", "unitCode")
  
  non_standard <- setdiff(names(vardesc), possible_vals)
  if (length(non_standard) > 0) {
    warning("The following variable properties are not standard: ",
            paste(non_standard, collapse = ", "), "\n")
  }
  
  # TODO: add more validation ----
  
  # make variableMeasured ----
  vm <- list()
  colnames <- names(data)
  
  for (i in 1:ncol(data)) {
    col <- colnames[i]
    
    # @type and name ----
    vm[[i]] <- list(
      `@type` = "PropertyValue",
      name = col,
      description = col # default to be replaced from vardesc
    )
    
    # set variable attributes from vardesc ----
    for (vd in names(vardesc)) {
      vals <- vardesc[[vd]]
      
      if (!is.null(names(vals))) {
        # set from named (if available)
        if (col %in% names(vals)) vm[[i]][vd] <- vals[col]
      } else if (length(vals) == ncol(data)) {
        # set from position
        vm[[i]][vd] <- vals[i]
      } else {
        warning("Couldn't set ", vd, " for ", col)
      }
    }
    
    # set type if not specified ---
    if (!is.null(vm[[i]]$type)) {
      if ((vm[[i]]$type %in% names(types))) {
        vm[[i]]$type <- types[[vm[[i]]$type]]
      } else {
        # not a valid type, so set to null and get from data
        warning(vm[[i]]$name, " does not have a valid type (",
                vm[[i]]$type, ")")
        vm[[i]]$type <- NULL
      }
    }
    
    if (is.null(vm[[i]]$type)) {
      can_be_int <- isTRUE(all(
        data[[i]] == suppressWarnings(as.integer(data[[i]]))
      ))
      
      vm[[i]]$type <- dplyr::case_when(
        is.factor(data[[i]]) ~ "string",
        is.character(data[[i]]) ~ "string",
        is.integer(data[[i]]) ~ "int",
        is.numeric(data[[i]]) & can_be_int ~ "int",
        is.numeric(data[[i]]) ~ "float",
        is.logical(data[[i]]) ~ "bool",
        TRUE ~ typeof(data[[i]])
      )
    }
    
    # get levels for factors if not specified ----
    if (is.factor(data[[i]]) | !is.null(vm[[i]]$levels)) {
      if (is.null(vm[[i]]$levels)) {
        lvls <- levels(data[[i]])
        names(lvls) <- lvls
        vm[[i]]$levels <- lvls
      }
      # make sure it is a list because named vectors don't render right in jsonlite
      vm[[i]]$levels <- as.list(vm[[i]]$levels)
      
      if (is.null(vm[[i]]$ordered)) {
        vm[[i]]$ordered <- is.ordered(data[[i]])
      }
    }
  }
  
  schema$variableMeasured <- vm
  
  # return correct format ----
  return <- match.arg(return)
  
  if (return == "json") {
    schema <- schema %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  } else {
    class(schema) <- c("psychds_codebook", "list")
  }
  
  if (return == "data") {
    # convert data ----
    r_pds <- c(character = "string", 
               logical = "bool",
               integer = "int",
               double = "float")
    for (v in schema$variableMeasured) {
      col <- data[[v$name]]
      ctype <- typeof(col)
      if (r_pds[ctype] != v$type) {
        message("Converting ", v$name, " from ", r_pds[ctype], " to ", v$type)
        # types don't match so convert
        suppressWarnings(
          if (v$type == "string") {
            convcol <- as.character(col)
          } else if (v$type == "bool") {
            convcol <- as.logical(col)
          } else if (v$type == "int") {
            convcol <- as.integer(col)
          } else if (v$type == "float") {
            convcol <- as.double(col)
          } else {
            convcol <- col
          }
        )
        
        # check conversions are equivalent
        if (isTRUE(all(convcol == col))) {
          data[v$name] <- convcol
        } else {
          message("- Error, not converted")
        }
      }
    }
    
    attr(data, "codebook") <- schema
    return(data)
  } else {
    return(schema)
  }
}



