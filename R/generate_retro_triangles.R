#' Generate retrospective reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   retrospective reporting triangles we want to create `n`, and iteratively
#'   generates the reporting triangle that would have been available as of the
#'   maximum reference time, working from bottom to top for `n` snapshots
#'
#' @param triangle Matrix of the reporting triangle/rectancgle
#'   to be used to generate retrospective triangles, with rows representing the
#'   time points of reference and columns representing the delays.
#' @param n Integer indicating the number of retrospective
#'   reporting triangles to be generated, always starting from the most
#'   recent reference time. Default is  only generate truncated matrices
#'   that have sufficient number of rows to generate a nowcast from, though
#'   any number can be specified.
#' @returns A list of `n` retrospective reporting triangle matrices.
#'   with as many rows as available given the truncation, and the same number
#'   of columns as `triangle`.
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
#' retro_rts <- generate_retro_triangles(
#'   triangle = triangle,
#'   n = 2
#' )
#' print(retro_rts[[1]])
#' print(retro_rts[[2]])
generate_retro_triangles <- function(
    triangle,
    n = nrow(triangle) - ncol(triangle) - 1) {
  .validate_triangle(triangle)
  if (n > (nrow(triangle) - ncol(triangle) - 1)) {
    cli::cli_warn(
      message = c(
        "Not all of the triangles generated will contain sufficient ",
        "observations to generate a nowcast from"
      )
    )
  }

  results <- lapply(seq_len(n),
    generate_retro_triangle,
    matr_observed = triangle
  )

  return(results)
}


#' Get a single retrospective triangle
#'
#' This function takes in a integer `t` and a reporting triangle and generates
#'  the reporting triangle that would have been observed as of `t` units
#'  earlier (starting from the bottom of the reporting triangle).
#'
#' @param t Integer indicating the number of days prior to generate the
#'  retrospective reporting triangle for.
#' @param matr_observed Matrix of the reporting triangle/rectangle
#'   to be used to generate retrospective nowcasts, with rows representing the
#'   time points of reference and columns representing the delays.
#'
#' @returns Matrix with `t` fewer rows than `matr_observed`, replicating what
#'   would have been observed as of `t` days prior.
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
#' retro_rep_tri <- generate_retro_triangle(
#'   t = 1,
#'   matr_observed = triangle
#' )
#' print(retro_rep_tri)
generate_retro_triangle <- function(t,
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
  if (t < 0) {
    cli_abort(
      message = "t must be a non-negative integer"
    )
  }

  assert_integerish(t)
  matr_observed_trunc <- matrix(
    matr_observed[1:(n_obs - t), ],
    nrow = (n_obs - t)
  )
  matr_observed_temp <- .replace_lower_right_with_NA(matr_observed_trunc)
  return(matr_observed_temp)
}
