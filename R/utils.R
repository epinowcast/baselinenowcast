#' Escape brackets returned in a string for passing to glue
#'
#' @param string Vector of strings containing `{}`
#' @return Vector of strings where all single brackets are replaced with double
#'   brackets
#' @keywords internal
.autoescape_brackets <- function(string) {
  return(gsub("\\{|\\}", "", string))
}
