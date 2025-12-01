#' Truncate reporting triangle by removing bottom rows
#'
#' Generates a list of retrospective reporting triangles by successively
#'   removing rows from the bottom of the original triangle.
#' Each truncated triangle represents what would have been observed at an
#'   earlier reference time.
#' This function truncates by row count, removing the most recent observations.
#' For other truncation approaches, see [truncate_to_quantile()].
#'
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to generate truncated matrices for
#'   each row up until there are insufficient rows to generate nowcasts
#'   from, where the minimum requirement is one more than the  number of
#'   horizon rows (rows containing NAs).
#' @inheritParams estimate_delay
#' @returns `trunc_rep_tri_list` List of `n` truncated reporting triangle
#'   matrices with as many rows as available given the truncation, and the same
#'   number of columns as `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @family generate_retrospective_data
#' @export
#' @examples
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )
#'
#' truncated_rts <- truncate_to_rows(triangle, n = 2)
#' truncated_rts[1:2]
truncate_to_rows <- function(reporting_triangle,
                             n = nrow(reporting_triangle) -
                               sum(is.na(rowSums(reporting_triangle))) - 1) {
  .validate_triangle(reporting_triangle)
  assert_integerish(n, lower = 0)
  trunc_rep_tri_list <- lapply(
    seq_len(n),
    truncate_to_row,
    reporting_triangle
  )

  return(trunc_rep_tri_list)
}

#' Truncate reporting triangle to a specific row
#'
#' Removes the last `t` rows from a reporting triangle to simulate what would
#'   have been observed at an earlier reference time.
#' This function truncates by row count.
#' For other truncation approaches, see [truncate_to_quantile()].
#'
#' @param t Integer indicating the number of timepoints to truncate off the
#'   bottom of the original reporting triangle.
#' @inheritParams estimate_delay
#' @returns `trunc_rep_tri` Matrix with `t` fewer rows than
#'    `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#' @family generate_retrospective_data
#' @export
#' @examples
#' # example code
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )
#'
#' trunc_rep_tri <- truncate_to_row(t = 1, reporting_triangle = triangle)
#' trunc_rep_tri
truncate_to_row <- function(t,
                            reporting_triangle) {
  n_obs <- nrow(reporting_triangle)
  if (t >= n_obs) {
    cli_abort(
      message = c(
        "The as of time point is greater than or equal to the number of ",
        "rows in the original triangle."
      )
    )
  }
  assert_integerish(t, lower = 0)
  rep_tri_trunc <- matrix(
    reporting_triangle[1:(n_obs - t), ],
    nrow = (n_obs - t)
  )
  return(rep_tri_trunc)
}
