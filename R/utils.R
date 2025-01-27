#' Extract Base Function Name
#' Adopted from https://github.com/epinowcast/primarycensored/blob/04f3ff2e5c8741c1f6e003566c68bbaf491cb073/R/utils.R#L11 #nolint
#' This helper function extracts the base name of a function, removing an
#' namespace prefixes.
#'
#' @param func The output of `substitute` on a function.
#'
#' @return A character string representing the base name of the function.
#'
#' @keywords internal
.extract_function_name <- function(func) {
  func_name <- deparse(func)
  base_name <- sub("^.*::", "", func_name)
  return(base_name)
}