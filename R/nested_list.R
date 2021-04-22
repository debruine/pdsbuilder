#' Output a nested list in RMarkdown list format (or markdown formatted headers)
#'
#' @param x The list
#' @param pre Text to prefix to each line (e.g., if you want all lines indented 4 spaces to start with, use "    -", see examples)
#' @param plus Text to add at each level (e.g., if you want all lines indented 4 extra spaces per level, use "    ")
#' @param nextt Text to add between list name and element (e.g., default ": " to keep a yaml flair)
#'
#' @return A character string
#' @export
#'
#' @examples
#' x <- list(
#'   a = list(a1 = "Named", a2 = "List"),
#'   b = list("Unnamed", "List"),
#'   c = c(c1 = "Named", c2 = "Vector"),
#'   d = c("Unnamed", "Vector"),
#'   e = list(e1 = list("A", "B", "C"),
#'            e2 = list(a = "A", b = "B"),
#'            e3 = c("A", "B", "C"),
#'            e4 = 100),
#'   f = "not a list or vector"
#' )
#' nested_list(x)
#' nested_list(x, pre = "1. ")
#' nested_list(x, pre = "# ", post = "#", nextt = "\n")
nested_list <- function(x, pre = "* ", post = "    ", nextt = ": ") {
  txt <- c()
  if (is.list(x) | length(x) > 1) {
    if (is.null(names(x))) {
      # unnamed list
      for (i in 1:length(x)) {
        y <- x[[i]]
        if (is.list(y) | length(y) > 1) {
          txt[length(txt)+1] <- paste0(pre, i, nextt)
          subtxt<- nested_list(y, paste0(post,pre),post,nextt)
        } else {
          subtxt<- nested_list(y, pre,post,nextt)
        }
        txt <- c(txt, subtxt)
      }
    } else {
      # named list
      for (y in names(x)) {
        if (is.list(x[[y]]) | length(x[[y]]) > 1) {
          # non-terminal named list entry
          txt[length(txt)+1] <- paste0(pre, y, nextt)
          subtxt <- nested_list(x[[y]], paste0(post,pre),post,nextt)
          txt <- c(txt, subtxt)
        } else {
          # terminal named list entry
          entry <- paste(x[[y]], collapse = ", ")
          txt[length(txt)+1] <-
            paste0(pre, y, nextt, entry)
        }
      }
    }
  } else {
    # terminal unnamed list entry
    txt[length(txt)+1] <- paste0(pre,
                                 paste(x, collapse = ", "))
  }

  list_txt <- paste(txt, collapse = "\n")
  class(list_txt) <- c("nested_list", "character")

  list_txt
}



