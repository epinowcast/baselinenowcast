#' Generate retrospective nowcasts
#'
#' This function ingests a list of incomplete reporting triangles and
#'   generates a list of reporting squares, or "complete"
#'   point estimates of reporting triangles based on the delay estimated in
#'   each triangle. It uses the specified `n` number of
#'   observations to estimate the empirical delay for each retrospective
#'   reporting triangle.
#'
#' @param list_of_rts List of reporting triangle matrices, in order
#'    from most recent (most complete) to least recent. Bottom right of the
#'    matrices should contain NAs.
#' @param max_delay Integer indicating the maximum delay to estimate, in units
#'   of the delay. The default is to use one less than the minimum number of
#'   rows of all of the matrices in the `list_of_rts`
#' @param n Integer indicating the number of observations
#'    (number of rows) to use to estimate the delay distribution for each
#'    reporting triangle. Default is the minimum of the number of rows of
#'    all the matrices in the `list_of_rts`
#'
#' @returns `list_of_nowcasts` List of the same number of elements as the input
#'   `list_of_rts`but with each reporting triangle filled in based on the delay
#'    estimated in that reporting triangle.
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
#'   triangle = triangle
#' )
#' retro_rts <- generate_triangles(
#'   list_of_trunc_rts = trunc_rts
#' )
#' retro_nowcasts <- generate_point_nowcasts(
#'   list_of_rts = retro_rts
#' )
#' print(retro_nowcasts[[1]])
generate_point_nowcasts <- function(list_of_rts,
                                    max_delay = min(
                                      sapply(list_of_rts, ncol)
                                    ) - 1,
                                    n = min(
                                      sapply(list_of_rts, nrow)
                                    )) {
  if (n > min(
    sapply(list_of_rts, nrow)
  )) {
    cli_abort(
      message = c(
        "The number of observations specified for delay estimation is greater ",
        "than the minimum number of rows in all of the retrospective ",
        "reporting triangles. Either remove the reporting triangles that do ",
        "not contain sufficient data, or lower `n_history_delay`"
      )
    )
  }
  if (n < min(sapply(list_of_rts, ncol))) {
    cli_abort(
      message = c(
        "The number of observations specified for delay estimation is less ",
        "than one plus the number of columns in the retrospective reporting ",
        "triangles. The delay distribution can only be estimated from at ",
        "at least the number of columns in the reporting triangle."
      )
    )
  }

  list_of_nowcasts <- lapply(list_of_rts, generate_point_nowcast, n = n)


  return(list_of_nowcasts)
}

#' Generate point nowcast
#'
#' This function ingests a reporting triangle matrix and optionally, a delay
#'    distribution, and returns a completed reporting square which represents
#'    the point nowcast. If a delay distribution is specified, this will be
#'    ised to generate the nowcast, otherwise, a delay distribution will be
#'    estimated from the `triangle_to_nowcast`.
#'
#' @inheritParams get_delay_estimate
#' @returns `comp_rep_square` Matrix of the same number of rows and columns as
#'    the `triangle_to_nowcast` but with the missing values filled in as point
#'   estimates
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' reporting_square <- generate_point_nowcast(
#'   triangle_to_nowcast = triangle
#' )
#' print(reporting_sqaure)
generate_point_nowcast <- function(triangle_to_nowcast,
                                   max_delay = ncol(triangle) - 1,
                                   n = nrow(triangle),
                                   delay_pmf = NULL) {
  .validate_triangle(triangle_to_nowcast)
  if (is.null(delay_pmf)) {
    delay_pmf <- get_delay_estimate(
      triangle_to_nowcast,
      max_delay,
      n
    )
  }

  comp_rep_square <- apply_delay(triangle_to_nowcast, delay_pmf)
  return(comp_rep_square)
}
