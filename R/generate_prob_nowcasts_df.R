#' Generate probabilistic nowcast dataframe
#'
#' This function ingests a list of matrices that represent expected observed
#'    nowcasts, and generates a long tidy dataframe indexed by
#'
#' @param list_of_nowcasts List of  matrices of expected
#'    observed reporting squares
#' @importFrom cli cli_abort
#' @returns `nowcast_df` Dataframe containing observations and expected
#'    observed nowcasts indexed by reference time and delay.
#' @export
#'
#' @examples
#' point_nowcast <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 17.8,
#'     80, 40, 23.2, 15.9,
#'     70, 35, 20.3, 19.9
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#'
#' list_of_exp_obs_nowcast <- add_obs_errors_to_nowcast(
#'   comp_rep_square = point_nowcast,
#'   disp = c(8, 1.4, 4),
#'   n_draws = 10
#' )
#'
#' nowcast_df <- generate_prob_nowcast_df(
#'   list_of_nowcasts = list_of_exp_obs_nowcast
#' )
generate_prob_nowcast_df <- function(list_of_nowcasts) {
  # Check if all elements are matrices
  if (!all(sapply(list_of_nowcasts, is.matrix))) {
    cli_abort("All elements in the list must be matrices.")
  }

  # Convert each matrix to a data frame with an index column
  df_list <- lapply(seq_along(list_of_nowcasts), function(i) {
    mat <- list_of_nowcasts[[i]]
    df <- as.data.frame(mat)
    df$draw <- i # Add an index column for list element
    df$time <- seq_len(nrow(df)) # Add a row index for matrix rows

    n_rows <- nrow(mat)
    df_long <- df |>
      tidyr::pivot_longer(
        cols = starts_with("V"),
        names_to = "delay",
        names_prefix = "V",
        values_to = "count"
      )


    return(df_long)
  })

  # Combine all data frames into a single data frame
  combined_df <- do.call(rbind, df_list)
}
