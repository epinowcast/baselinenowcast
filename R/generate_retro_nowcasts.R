#' Generate retrospective nowcasts
#'
#' This function ingests a list of retrospectve reporting triangles and
#'   generates a list of retrospective reporting squares, or "complete"
#'   reporting triangles. It uses the specified `n_history_delay`  number of
#'   observations to estimate the empirical delay for each retrospective
#'   reporting triangle.
#'
#' @param list_of_retro_rts List of reporting triangle matrices, in order
#'    from most recent (most complete) to least recent. Bottom right of the
#'    matrices should contain NAs.
#' @param n_history_delay Integer indicating the number of observations
#'    (number of rows) to use to estimate the delay distribution for each
#'    reporting triangle. Default is the minimum of the number of rows of
#'    all the matrices in the `list_of_retro_rts`
#'
#' @returns List of the same number of elements as the input `list_of_retro_rts`
#'    but with each reporting triangle filled in based on the delay estimated
#'    in that reporting triangle.
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
#' retro_nowcasts <- generate_retro_nowcasts(
#'   list_of_retro_rts = retro_rts,
#'   n_history_delay = 5
#' )
generate_retro_nowcasts <- function(list_of_retro_rts,
                                    n_history_delay = min(
                                      sapply(list_of_retro_rts, nrow)
                                    )) {
  list_of_retro_nowcasts <- list()

  for (i in 1:length(list_of_retro_rts)) {
    # Estimate delay
    rt <- list_of_retro_rts[[i]]
    delay_pmf <- get_delay_estimate(rt, n_history_delay = n_history_delay)
    # Apply delay
    rt_complete <- apply_delay(rt, delay_pmf)

    list_of_retro_nowcasts[[i]] <- rt_complete
  }

  return(list_of_retro_nowcasts)
}
