#' Estimate dispersion parameters
#'
#' This function ingests a list of nowcasts (completed reporting rectangles) and
#'    the latest reporting triangle and uses both to estimate dispersion
#'    parameters from the observations and estimates at each delay.
#'
#' @param list_of_nowcasts List of complete reporting rectangles where rows
#'    represent reference time points and columns represent delays.
#' @param latest_rep_tri Matrix of the latest, incomplete, reporting triangle,
#'    with rows representing the time points of reference and columns
#'    representing the delays.
#' @param n Integer indicating the number of reporting rectangles to use to
#'    estimate the dispersion parameters
#'
#' @returns Vector of length one less than the number of columns in the
#'    latest reporting triangle, with each element representing the estimate
#'    of the dispersion parameter for each delay d, starting at delay d=1.
#' @export
#'
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
#' retro_rts <- generate_triangles(
#'   triangle = triangle,
#'   n = 2
#' )
#' retro_nowcasts <- generate_nowcasts(
#'   list_of_rts = retro_rts,
#'   n = 5
#' )
#' disp_params <- estimate_disp_params(
#'   list_of_retro_nowcasts = retro_nowcasts,
#'   latest_rep_tri = triangle
#' )
estimate_disp_params <- function(
    list_of_retro_nowcasts,
    latest_rep_tri,
    n = length(list_of_retro_nowcasts)) {
  .validate_triangle(latest_rep_tri)

  matr_observed <- .replace_lower_right_with_NA(latest_rep_tri)

  for (i in seq_len(n)) {
    # Rretrospective nowcast as of i delays ago
    nowcast_i <- list_of_retro_nowcasts[[i]]
    # Remove the last i observations
    trunc_matr_observed <- matr_observed[1:(nrow(matr_observed) - i), ]
    # What would have been nowcasted? The values that would have been NA
    indices_nowcast <- is.na(.replace_lower_right_with_NA(trunc_matr_observed))
    # What was observed? Any non-NAs from the truncated observed matrix
    indices_observed <- !is.na(trunc_matr_observed)

    # Get a vector of for each delay of what would have been added in this
    # reference time, based on the indices nowcasted and the indices later
    # observed
    exp_to_add <- sapply(seq_len(ncol(matr_observed)),
      .conditional_sum_cols,
      matrix_bool1 = indices_nowcast,
      matrix_bool2 = indices_observed,
      matrix_to_sum = nowcast_i
    )
    # Get a vector for each delay of what was observed using the truncated
    # observed matrix
    to_add <- sapply(seq_len(ncol(matr_observed)),
      .conditional_sum_cols,
      matrix_bool1 = indices_nowcast,
      matrix_bool2 = indices_observed,
      matrix_to_sum = trunc_matr_observed
    )
    # Create a dataframe to what would have been added at each delay for
    # each t delay ago.
    df_i <- data.frame(
      exp_to_add = exp_to_add,
      to_add = to_add,
      t = i,
      d = seq_len(ncol(matr_observed)) - 1
    )
    if (i == 1) {
      df <- df_i
    } else {
      df <- bind_rows(df, df_i)
    }
  }
  disp_params <- vector(length = (ncol(matr_observed) - 1))
  for (i in seq_len(ncol(matr_observed) - 1)) {
    obs_temp <- df$to_add[df$d == i]
    mu_temp <- df$exp_to_add[df$d == i] + 0.1
    disp_params[i] <- .fit_nb(x = obs_temp, mu = mu_temp)
  }

  return(disp_params)
}
