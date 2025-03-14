#' Generate retrospective reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   retrospective reporting triangles we want to create, `n`, and iteratively
#'   generates the reporting triangle that would have been available as of the
#'   maximum reference time, working from bottom to top for `n` snapshots.
#'
#' @param truncated_triangles List of truncated reporting triangle matrices.
#' @returns List of retrospective reporting triangle matrices, generated
#'   by removing the bottom right observations from the truncated reporting
#'   triangle matrices.
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
#' trunc_rts <- truncate_triangles(
#'   triangle = triangle,
#'   n = 2
#' )
#' retro_rts <- generate_triangles(
#'   truncated_triangles = trunc_rts
#' )
#' print(retro_rts[[1]])
#' print(retro_rts[[2]])
generate_triangles <- function(
    truncated_triangles) {
  results <- lapply(
    truncated_triangles,
    .replace_lower_right_with_NA
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
#' retro_rep_tri <- generate_triangle(
#'   t = 1,
#'   matr_observed = triangle
#' )
#' print(retro_rep_tri)
generate_triangle <- function(t,
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
  matr_observed_temp <- .replace_lower_right_with_NA(matr_observed_trunc)
  return(matr_observed_temp)
}
