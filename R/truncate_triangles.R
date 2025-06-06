#' Generate truncated reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   truncated reporting triangles we want to create, `n`, and iteratively
#'   truncates the reporting triangle, working from the latest reference time
#'   (bottom) to the older reference times (top) for `n`
#'   snapshots.
#'
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to generate truncated matrices
#'   for all triangles that would have a sufficient number of rows to generate
#'   a nowcast from.
#' @inheritParams get_delay_estimate
#' @returns `trunc_rep_tri_list` List of `n` truncated reporting triangle
#'   matrices with as many rows as available given the truncation, and the same
#'   number of columns as `reporting_triangle`.
#' @importFrom checkmate assert_integerish
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
#' truncated_rts <- truncate_triangles(triangle, n = 2)
#' truncated_rts[1:2]
truncate_triangles <- function(reporting_triangle,
                               n = nrow(reporting_triangle) - ncol(reporting_triangle) - 1) { # nolint
  .validate_triangle(reporting_triangle)
  assert_integerish(n, lower = 0)
  if (n > (nrow(reporting_triangle) - ncol(reporting_triangle) - 1)) {
    cli::cli_warn(
      message = c(
        "Not all of the triangles generated will contain sufficient ",
        "observations to generate a nowcast from"
      )
    )
  }

  trunc_rep_tri_list <- lapply(
    seq_len(n),
    truncate_triangle,
    reporting_triangle
  )

  return(trunc_rep_tri_list)
}

#' Get a single truncated triangle
#'
#' This function takes in a integer `t` and a reporting triangle and generates
#'   a truncated reporting triangle, remove the last `t` observations.
#'
#' @param t Integer indicating the number of timepoints to truncate off the
#'   bottom of the original reporting triangle.
#' @inheritParams get_delay_estimate
#' @returns `trunc_rep_tri` Matrix with `t` fewer rows than
#'    `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
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
#' trunc_rep_tri <- truncate_triangle(t = 1, reporting_triangle = triangle)
#' trunc_rep_tri
truncate_triangle <- function(t,
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
