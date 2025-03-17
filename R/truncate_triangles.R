#' Generate truncated reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   truncated reporting triangles we want to create, `n`, and iteratively
#'   truncated the reporting triangle, working from bottom to top for `n`
#'   snapshots.
#'
#' @param triangle Matrix of the reporting triangle/rectangle
#'   to be used to generate retrospective triangles, with rows representing the
#'   time points of reference and columns representing the delays.
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to only generate truncated matrices
#'   that have sufficient number of rows to generate a nowcast from, though
#'   any number can be specified.
#' @returns list_of_trunc_rts List of `n` truncated reporting triangle matrices.
#'   with as many rows as available given the truncation, and the same number
#'   of columns as `triangle`.
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
#' truncated_rts <- truncate_triangles(
#'   triangle = triangle,
#'   n = 2
#' )
#' print(truncated_rts[[1]])
#' print(truncated_rts[[2]])
truncate_triangles <- function(
    triangle,
    n = nrow(triangle) - ncol(triangle) - 1) {
  .validate_triangle(triangle)
  assert_integerish(n, lower = 0)
  triangle <- .replace_lower_right_with_NA(triangle)
  if (n > (nrow(triangle) - ncol(triangle) - 1)) {
    cli::cli_warn(
      message = c(
        "Not all of the triangles generated will contain sufficient ",
        "observations to generate a nowcast from"
      )
    )
  }

  results <- lapply(seq_len(n),
    truncate_triangle,
    matr_observed = triangle
  )

  return(results)
}

#' Get a single truncated triangle
#'
#' This function takes in a integer `t` and a reporting triangle and generates
#'   a truncated reporting triangle, remove the last `t` observations
#'
#' @param t Integer indicating the number of timepoints to truncate off the
#'   bottom of the original reporting triangle
#' @param matr_observed Matrix of the reporting triangle/rectangle
#'   to be used to generate retrospective nowcasts, with rows representing the
#'   time points of reference and columns representing the delays.
#'
#' @returns Matrix with `t` fewer rows than `matr_observed`
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
#' trunc_rep_tri <- truncate_triangle(
#'   t = 1,
#'   matr_observed = triangle
#' )
#' print(trunc_rep_tri)
truncate_triangle <- function(t,
                              matr_observed) {
  n_obs <- nrow(matr_observed)
  if (t >= n_obs) {
    cli_abort(
      message = c(
        "The as of time point is greater than or equal to the number of ",
        "rows in the original triangle."
      )
    )
  }
  assert_integerish(t, lower = 0)
  matr_observed_trunc <- matrix(
    matr_observed[1:(n_obs - t), ],
    nrow = (n_obs - t)
  )
  return(matr_observed_trunc)
}
