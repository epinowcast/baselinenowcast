#' Generate retrospective reporting triangles
#'
#' This function ingests a reporting triangle and the number of retrospective
#'   reporting triangles we want to create, and iteratively generates the
#'   reporting triangle that would have been available as of the maximum
#'   reference time, and iteratively working backwards for
#'   `n_history_uncertainty` snapshots
#'
#' @param triangle Matrix of the reporting triangle
#'   to be used to generate retrospective triangles, with rows representing the
#'   time points of reference and columns representing the delays.
#' @param n_triangles Integer indicating the number of retrospective
#'   reporting triangles to be generated, always starting from the most
#'   recent reference time. Default is  only generate truncated matrices
#'   that have sufficient number of rows to generate a nowcast from, though
#'   any number can be specified.
#' @returns A list of `n_triangle` retrospective reporting triangle matrices.
#'   with as many rows as available given the truncation, and the same number
#'   of columns as `triangle`
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
#'   n_triangles = 2
#' )
#' print(retro_rts[[1]])
#' print(retro_rts[[2]])
generate_retro_triangles <- function(
    triangle,
    n_triangles = nrow(triangle) - ncol(triangle)) {
  .validate_triangle(triangle)
  if (n_triangles > (nrow(triangle) - ncol(triangle))) {
    cli::cli_warn(
      message = c(
        "Not all of the triangles generated will contain sufficient ",
        "observations to generate a nowcast from"
      )
    )
  }

  # Will be able to remove this step if we require NAs in the bottom right
  # of the triangle
  matr_observed <- .replace_lower_right_with_NA(triangle)

  results <- lapply(seq_len(n_triangles),
    generate_retro_triangle,
    matr_observed = matr_observed
  )

  return(results)
}


#' Get a single retrospective triangle
#'
#' This function takes in a integer `t` and a reporting triangle and generates
#'  the reporting triangle that would have been observed as of `t` units
#'  earlier (starting from the bottom of the reporting triangle)
#'
#' @param t Integer indicating the number of days prior to generate the
#'  retrospective reporting triangle for
#' @param matr_observed Matrix of the incomplete reporting triangle
#'   to be used to generate retrospective nowcasts, with rows representing the
#'   time points of reference and columns representing the delays
#'
#' @returns Matrix with `t` fewer rows than `matr_observed`, replicating what
#'   would have been observed as of `t` days prior.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
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
