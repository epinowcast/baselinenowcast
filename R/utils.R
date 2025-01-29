#' Escape brackets returned in a string for passing to glue
#'
#' @param string A string vector containing `{}`
#'
#' @return A string vector where all single brackets are replaced with double
#' brackets
autoescape_brackets <- function(string) {
  return(gsub("\\{|\\}", "", string))
}
