#' Get a draw of only the predicted elements of the nowcast vector
#'
#' @param point_nowcast_matrix Matrix of point nowcast predictions and
#'   observations, with rows representing the reference times and columns
#'   representing the delays.
#' @param dispersion Vector of dispersion parameters indexed by horizon from
#'  minus one to the maximum delay.
#' @inheritParams get_delay_estimate
#' @inheritParams estimate_dispersion
#'
#' @returns Vector of predicted draws at each reference time, for all reference
#'    times in the input `point_nowcast_matrix`.
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom stats rnbinom
#' @importFrom zoo rollapply
#' @examples
#' point_nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 16.8,
#'     80, 40, 21.2, 19.5,
#'     70, 34.5, 15.4, 9.1
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' reporting_triangle <- generate_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draw <- get_nowcast_pred_draw(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp
#' )
#' nowcast_pred_draw
#'
#' # Get draws on the rolling sum
#' nowcast_pred_draw_agg <- get_nowcast_pred_draw(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   fun_to_aggregate = sum,
#'   k = 2
#' )
#' nowcast_pred_draw_agg
get_nowcast_pred_draw <- function(point_nowcast_matrix,
                                  reporting_triangle,
                                  dispersion,
                                  fun_to_aggregate = sum,
                                  k = 1) {
  .validate_aggregation_function(fun_to_aggregate)
  if (length(dispersion) > nrow(point_nowcast_matrix)) {
    cli_abort(
      message = c(
        "Vector of dispersion parameters is greater than the number ",
        "of reference times in `point_nowcast_matrix`. Check ",
        "to make sure this is expected behavior and truncate ",
        "dispersion to eliminate the horizons that are not in ",
        "the matrix."
      )
    )
  }
  point_nowcast_pred_matrix <- .extract_predictions(
    point_nowcast_matrix,
    reporting_triangle
  )
  n_horizons <- length(dispersion)
  max_t <- nrow(point_nowcast_pred_matrix)
  mean_pred_long <- rollapply(
    rowSums(point_nowcast_pred_matrix, na.rm = TRUE), # nolint
    k,
    fun_to_aggregate,
    align = "right",
    fill = NA
  ) # nolint
  # Nowcast predictions only (these are reversed, first element is horizon 0)
  mean_pred <- mean_pred_long[(max_t - n_horizons + 1):max_t]
  draw_pred <- rnbinom(n = n_horizons, size = rev(dispersion), mu = mean_pred)
  # Pad with 0s for the fully observed rows, which are before
  # the max_t - n_horizons
  draw_pred_long <- c(rep(0, max_t - n_horizons), draw_pred)
  return(draw_pred_long)
}

#' Combine observed data with a single prediction draw
#'
#' Internally it sums observed counts from the reporting triangle by reference
#' time and adds them to the predicted counts to form a single draw of the
#' nowcast for the final counts by reference time.
#'
#' @param predicted_counts Vector of predicted counts at each reference time
#' @inheritParams get_nowcast_pred_draw
#'
#' @returns A vector of predicted counts at each reference time
#' @export
#' @examples
#' pred_counts <- c(10, 20, 30, 40)
#' reporting_matrix <- matrix(
#'   c(
#'     7, 9, 4, 3,
#'     1, 2, 3, 4,
#'     5, 6, 7, 8,
#'     9, 10, 11, 12
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#' reporting_triangle <- generate_triangle(reporting_matrix)
#' combine_obs_with_pred(pred_counts, reporting_triangle)
#'
#' # Another example with rolling sum
#' combine_obs_with_pred(pred_counts, reporting_triangle, k = 2)
combine_obs_with_pred <- function(predicted_counts,
                                  reporting_triangle,
                                  fun_to_aggregate = sum,
                                  k = 1) {
  .validate_aggregation_function(fun_to_aggregate)
  obs_counts <- rollapply(
    rowSums(reporting_triangle, na.rm = TRUE),
    k,
    fun_to_aggregate,
    fill = NA,
    align = "right"
  )
  return(obs_counts + predicted_counts)
}


#' Get a dataframe of multiple draws of only the predicted elements of the
#'    nowcast vector
#'
#' @param draws Integer indicating the number of draws of the predicted
#'    nowcast vector to generate. Default is `1000`.
#' @inheritParams get_nowcast_pred_draw
#' @returns Dataframe containing the predicted point nowcast vectors indexed by
#'    reference time (`pred_count`), reference time (`time`), and the draw index
#'    (`draw`).
#' @export
#' @examples
#' point_nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 16.8,
#'     80, 40, 21.2, 19.5,
#'     70, 34.5, 15.4, 9.1
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' reporting_triangle <- generate_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draws <- get_nowcast_pred_draws(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   draws = 5
#' )
#' nowcast_pred_draws
#' # Get nowcast pred draws over rolling sum
#' nowcast_pred_draws_rolling_df <- get_nowcast_pred_draws(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   500,
#'   fun_to_aggregate = sum,
#'   k = 2
#' )
#' nowcast_pred_draws_rolling_df
get_nowcast_pred_draws <- function(point_nowcast_matrix,
                                   reporting_triangle,
                                   dispersion,
                                   draws = 1000,
                                   fun_to_aggregate = sum,
                                   k = 1) {
  assert_integerish(draws, lower = 1)
  .validate_aggregation_function(fun_to_aggregate)

  reference_times <- seq_len(nrow(point_nowcast_matrix))

  draws_df_list <- lapply(seq_len(draws), function(i) {
    pred_counts <- get_nowcast_pred_draw(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion
    )

    return(data.frame(
      pred_count = pred_counts,
      time = reference_times,
      draw = i
    ))
  })

  draws_df <- Reduce(rbind, draws_df_list)

  return(draws_df)
}

#' Generate a single draw of a nowcast combining observed and predicted values
#'
#' @inheritParams get_nowcast_pred_draws
#' @param ... Additional arguments passed to `get_nowcast_pred_draws()`
#' @returns Vector of predicted counts at each reference time based on combining
#'    the observed counts and the predicted counts for the unobserved elements.
#' @export
#' @examples
#' point_nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 16.8,
#'     80, 40, 21.2, 19.5,
#'     70, 34.5, 15.4, 9.1
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' reporting_triangle <- generate_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_draw <- get_nowcast_draw(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp
#' )
#' nowcast_draw
get_nowcast_draw <- function(point_nowcast_matrix,
                             reporting_triangle,
                             dispersion,
                             ...) {
  # Generate a single draw of the predictions
  pred_counts <- get_nowcast_pred_draw(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion,
    ...
  )

  # Combine with observations
  draw <- combine_obs_with_pred(
    pred_counts,
    reporting_triangle,
    ...
  )

  return(draw)
}

#' Generate multiple draws of a nowcast combining observed and predicted values
#'
#' @inheritParams get_nowcast_pred_draws
#' @param ... Additional arguments pass to `get_nowcast_pred_draws()`
#' @returns Dataframe containing information for multiple draws with columns
#'  for the reference time (`time`), the predicted counts (`pred_count`), and
#'  the draw number (`draw`).
#' @export
#' @examples
#' point_nowcast_matrix <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 16.8,
#'     80, 40, 21.2, 19.5,
#'     70, 34.5, 15.4, 9.1
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' reporting_triangle <- generate_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_draws <- get_nowcast_draws(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   draws = 5
#' )
#' nowcast_draws
get_nowcast_draws <- function(point_nowcast_matrix,
                              reporting_triangle,
                              dispersion,
                              draws = 1000,
                              ...) {
  reference_times <- seq_len(nrow(point_nowcast_matrix))

  draws_df_list <- lapply(seq_len(draws), function(i) {
    pred_counts <- get_nowcast_draw(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
      ...
    )

    return(data.frame(
      pred_count = pred_counts,
      time = reference_times,
      draw = i
    ))
  })

  draws_df <- Reduce(rbind, draws_df_list)
  return(draws_df)
}
