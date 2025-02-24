#' Generate retrospective reporting triangles
#'
#' This function ingests a reporting triangle and the number of retrospective
#'   reporting triangles we want to create, and iteratively generates the
#'   reporting triangle that would have been available as of the maximum
#'   reference time, and iteratively working backwards for
#'   `n_history_uncertainty` snapshots
#'
#' @param triangle_for_retro Matrix of the incomplete reporting triangle
#'   to be used to generate retrospective nowcasts, with rows representing the
#'   time points of reference and columns representing the delays
#' @param n_history_uncertainty Integer indicating the number of retrospective
#'   reporting triangles to be generated, always starting from the most
#'   recent reference time. The default is to generate as many retrospective
#'   reporting triangles that would have a sufficient number of rows to
#'   estimate  a delay distribution and generate a nowcast from.
#' @param n_history_delay Integer indicating the number of reference dates to be
#'   used in the estimate of the reporting delay, always starting from the most
#'   recent reporting delay. The default here is to us the difference between
#'   the number of observation (rows) in `triangle_for_retro` and
#'   `n_history_uncertainty` so all of the data is being used.
#' @returns A list of retrospective reporting triangle matrices with
#'   `n_history_delay` number of rows and the same number of columns as
#'   `triangle_for_retro`
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
#' retro_rts <- generate_retrospective_data(triangle)
#' print(retro_rts[[1]])
#' print(retro_rts[[2]])
generate_retrospective_data <- function(
    triangle_for_retro,
    n_history_uncertainty = nrow(triangle_for_retro) - ncol(triangle_for_retro),
    n_history_delay = nrow(triangle_for_retro) - n_history_uncertainty) {
  # Make sure you will have enough data to estimate the delay and generate
  # the nowcast for each of the retrospective datasets
  triangle <- triangle_for_retro
  .validate_triangle(triangle)

  .validate_retro_inputs(
    triangle,
    n_history_uncertainty,
    n_history_delay
  )
  # Will be able to remove this step if we require NAs in the bottom right
  # of the triangle
  matr_observed <- triangle |> .replace_lower_right_with_NA()

  results <- lapply(seq_len(n_history_uncertainty),
    .get_retro_matrix,
    matr_observed = matr_observed,
    n_history_delay = n_history_delay
  )

  return(results)
}


#' Get retrospective matrix
#'
#' This function takes in a integer t and a reporting triangle and generates
#'  the reporting triangle that would have been observed as of `t` days
#'  earlier
#'
#' @param t Integer indicating the number of days prior to generate the
#'  retrospective reporting triangle for
#' @param matr_observed Matrix of the incomplete reporting triangle
#'   to be used to generate retrospective nowcasts, with rows representing the
#'   time points of reference and columns representing the delays
#' @param n_history_delay Integer indicating the number of observations in
#'   in each retrospective reporting triangle
#'
#' @returns Matrix with `t` fewer rows than `matr_observed`, replicating what
#'   would have been observed as of `t` days prior.
#' @keywords internal
.get_retro_matrix <- function(t,
                              matr_observed,
                              n_history_delay) {
  n_obs <- nrow(matr_observed)
  matr_observed_temp <- matrix(
    matr_observed[(n_obs - n_history_delay - t + 1):
    (n_obs - t), ],
    nrow = n_history_delay
  ) |> .replace_lower_right_with_NA()
  return(matr_observed_temp)
}
