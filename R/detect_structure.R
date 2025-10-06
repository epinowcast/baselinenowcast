#' Detect the structure of a reporting triangle
#'
#' @inheritParams estimate_delay
#'
#' @returns  Integer or vector specifying the reporting structure.
#'   If integer, divides columns evenly by that integer (with last possibly
#'   truncated).  If vector, the sum must not be greater than or equal to the
#'   number of columns. Default is 1 (standard triangular structure). If
#'   there are no NAs, will return 0.
#' @export
#'
#' @examples
#' ragged_triangle <- matrix(
#'   c(
#'     1, 3, 5, 7, 9, 7,
#'     4, 5, 9, 4, NA, NA,
#'     1, 6, NA, NA, NA, NA,
#'     3, NA, NA, NA, NA, NA
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#' detected_structure <- detect_structure(ragged_triangle)
#' detected_structure
detect_structure <- function(reporting_triangle) {
  n_row_nas <- sum(is.na(rowSums(reporting_triangle)))
  # Structure is NULL if there are no NAs
  if (n_row_nas == 0) {
    return(0)
  }
  n_prev_nas <- 0
  structure_long <- rep(NA, ncol(reporting_triangle))
  for (i in 1:n_row_nas) {
    n_nas <- sum(!is.na(reporting_triangle[nrow(reporting_triangle) - i + 1, ])) - n_prev_nas # nolint
    structure_long[i] <- n_nas
    n_prev_nas <- n_prev_nas + n_nas
  }
  structure <- structure_long[!is.na(structure_long)]

  # Check to see if this can be reduced to just a single number
  expanded <- .expand_structure_vec(structure[1],
    cols = ncol(reporting_triangle)
  )

  if (identical(expanded, structure)) {
    structure <- structure[1]
  }
  return(structure)
}
