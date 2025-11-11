#' Get reporting structure from a reporting triangle
#'
#' Returns an integer or vector specifying the reporting structure, which
#' indicates how the reporting triangle is organized. This structure tells
#' [construct_triangle()] how to create new reporting triangles with the same
#' reporting pattern.
#'
#' @inheritParams estimate_delay
#'
#' @returns  Integer or vector specifying the reporting structure.
#'   If integer, divides columns evenly by that integer (with last possibly
#'   truncated).  If vector, the sum must not be greater than or equal to the
#'   number of columns. Default is 1 (standard triangular structure). If
#'   there are no NAs, will return 0.
#' @family reporting_triangle
#' @export
#'
#' @examples
#' # Get structure from example triangle
#' structure <- get_reporting_structure(example_reporting_triangle)
#' structure
get_reporting_structure <- function(reporting_triangle) {
  n_row_nas <- sum(is.na(rowSums(reporting_triangle)))
  # Structure is 0 if there are no NAs
  if (n_row_nas == 0) {
    cli_alert_info(
      text =
        "The reporting triangle does not contain any missing values." # nolint
    )
    return(0)
  }
  n_prev_nas <- 0
  structure_long <- rep(NA, ncol(reporting_triangle))
  for (i in 1:n_row_nas) {
    n_nas <- sum(!is.na(reporting_triangle[nrow(reporting_triangle) - i + 1, ])) - n_prev_nas # nolint
    structure_long[i] <- n_nas
    n_prev_nas <- n_prev_nas + n_nas
  }
  struct <- structure_long[!is.na(structure_long)]

  # Check to see if this can be reduced to just a single number
  expanded <- .expand_structure_vec(struct[1],
    cols = ncol(reporting_triangle)
  )

  if (identical(expanded, struct)) {
    struct <- struct[1]
  }
  return(struct)
}
