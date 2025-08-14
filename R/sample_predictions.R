#' Get a draw of only the predicted elements of the nowcast vector
#'
#' @param point_nowcast_matrix Matrix of point nowcast predictions and
#'   observations, with rows representing the reference times and columns
#'   representing the delays.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Note that these will be reversed
#'   internally to match the ordering of the `point_nowcast_matrix` (where
#'   a horizon of 1 is the last entry).
#' @param observation_model Function that ingests a vector or matrix of
#'    predictions and a vector of uncertainty parameters and generates draws
#'    from the observation model. Default is `sample_nb` which expects
#'    arguments `pred` for the vector of predictions and uncertainty parameters
#'    for the corresponding vector of uncertainty parameters, and draws from a
#'    negative binomial for each element of the vector.
#' @inheritParams estimate_delay
#' @inheritParams estimate_uncertainty
#' @returns Vector of predicted draws at each reference time, for all reference
#'    times in the input `point_nowcast_matrix`.
#' @export
#' @importFrom cli cli_abort cli_warn
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
#' reporting_triangle <- construct_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draw <- sample_prediction(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp
#' )
#' nowcast_pred_draw
#'
#' # Get draws on the rolling sum
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   nowcast_pred_draw_agg <- sample_prediction(
#'     point_nowcast_matrix,
#'     reporting_triangle,
#'     disp,
#'     ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
#'   )
#'   nowcast_pred_draw_agg
#' }
sample_prediction <- function(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    observation_model = sample_nb,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)) {
  if (length(uncertainty_params) > nrow(point_nowcast_matrix)) {
    cli_abort(
      message = c(
        "Vector of uncertainty parameters is greater than the number ",
        "of reference times in `point_nowcast_matrix`. Check ",
        "to make sure this is expected behavior."
      )
    )
  }


  aggr_nowcast <- ref_time_aggregator(point_nowcast_matrix)
  aggr_rt <- ref_time_aggregator(reporting_triangle)
  aggr_nowcast_pred_matrix <- .extract_predictions(
    aggr_nowcast,
    aggr_rt
  )
  n_horizons <- sum(is.na(rowSums(aggr_rt)))
  if (length(uncertainty_params) < n_horizons) {
    cli_abort(
      message = c(
        "Vector of uncertainty parameters is less than the number of",
        "horizons in the `reporting_triangle`."
      )
    )
  }

  max_t <- nrow(aggr_nowcast)
  mean_pred_agg <- as.matrix(delay_aggregator(aggr_nowcast_pred_matrix))

  # If there are no partial reference times, return a zero matrix of the
  # appropriate size (nothing to predict).
  if (n_horizons == 0) {
    return(matrix(0, nrow = max_t, ncol = ncol(mean_pred_agg)))
  }
  # Get only the predictions for the partial reference times; ordered by
  # reference time with horizon = 1 being the last entry.

  # Get only the predictions, these are ordered by reference time,
  # so horizon = 0 is last.
  mean_pred <- mean_pred_agg[(max_t - n_horizons + 1):max_t, ]
  uncertainty_params_use <- tail(uncertainty_params, n = n_horizons)
  draw_pred <- as.matrix(observation_model(
    pred = mean_pred,
    uncertainty_params = rev(uncertainty_params_use)
  ))

  # Pad with 0s for the fully observed rows, which are before
  # the max_t - n_horizons
  top_matrix <- matrix(0,
    nrow = max_t - n_horizons,
    ncol = ncol(mean_pred_agg)
  )
  draw_pred_agg <- rbind(
    top_matrix,
    draw_pred
  )
  return(draw_pred_agg)
}

#' Combine observed data with a single prediction draw
#'
#' Internally it sums observed counts from the reporting triangle by reference
#' time and adds them to the predicted counts to form a single draw of the
#' nowcast for the final counts by reference time.
#'
#' @param predicted_counts Vector of predicted counts at each reference time.
#'    Note that if using a reference time or delay aggregator function, this
#'    is assumed to have already been aggregated.
#' @inheritParams sample_prediction
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
#' reporting_triangle <- construct_triangle(reporting_matrix)
#' combine_obs_with_pred(pred_counts, reporting_triangle)
#' # Another example with rolling sum
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   combine_obs_with_pred(pred_counts,
#'     reporting_triangle,
#'     ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
#'   )
#' }
combine_obs_with_pred <- function(
    predicted_counts,
    reporting_triangle,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)) {
  aggr_reporting_triangle <- ref_time_aggregator(reporting_triangle)
  obs_counts_agg <- delay_aggregator(aggr_reporting_triangle)
  return(obs_counts_agg + predicted_counts)
}


#' Get a dataframe of multiple draws of only the predicted elements of the
#'    nowcast vector
#'
#' @param draws Integer indicating the number of draws of the predicted
#'    nowcast vector to generate. Default is `1000`.
#' @param ... Additional arguments passed to `sample_prediction`.
#' @inheritParams sample_prediction
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
#' reporting_triangle <- construct_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draws <- sample_predictions(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   draws = 5
#' )
#' nowcast_pred_draws
#' # Get nowcast pred draws over rolling sum
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   nowcast_pred_draws_rolling_df <- sample_predictions(
#'     point_nowcast_matrix,
#'     reporting_triangle,
#'     disp,
#'     500,
#'     ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
#'   )
#'   nowcast_pred_draws_rolling_df
#' }
sample_predictions <- function(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    draws = 1000,
    ...) {
  assert_integerish(draws, lower = 1)
  reference_times <- seq_len(nrow(point_nowcast_matrix))

  draws_df_list <- lapply(seq_len(draws), function(i) {
    pred_counts <- sample_prediction(
      point_nowcast_matrix,
      reporting_triangle,
      uncertainty_params,
      ...
    )

    # If aggregating, we need to pad with NAs
    pred_counts_padded <- c(
      rep(
        NA,
        length(reference_times) - length(pred_counts)
      ),
      pred_counts
    )
    return(data.frame(
      pred_count = pred_counts_padded,
      time = reference_times,
      draw = i
    ))
  })

  draws_df <- Reduce(rbind, draws_df_list)

  return(draws_df)
}

#' Generate a single draw of a nowcast combining observed and predicted values
#'
#' @inheritParams sample_prediction
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
#' reporting_triangle <- construct_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_draw <- sample_nowcast(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp
#' )
#' nowcast_draw
sample_nowcast <- function(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    observation_model = sample_nb,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)) {
  # Generate a single draw of the predictions
  pred_counts <- sample_prediction(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    observation_model,
    ref_time_aggregator,
    delay_aggregator
  )

  # Combine with observations
  draw <- combine_obs_with_pred(
    pred_counts,
    reporting_triangle,
    ref_time_aggregator,
    delay_aggregator
  )

  return(draw)
}

#' Generate multiple draws of a nowcast combining observed and predicted values
#'
#' @inheritParams sample_predictions
#' @param ... Additional arguments passed to `sample_nowcast`.
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
#' reporting_triangle <- construct_triangle(point_nowcast_matrix)
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_draws <- sample_nowcasts(
#'   point_nowcast_matrix,
#'   reporting_triangle,
#'   disp,
#'   draws = 5
#' )
#' nowcast_draws
sample_nowcasts <- function(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    draws = 1000,
    ...) {
  reference_times <- seq_len(nrow(point_nowcast_matrix))

  draws_df_list <- lapply(seq_len(draws), function(i) {
    pred_counts <- sample_nowcast(
      point_nowcast_matrix,
      reporting_triangle,
      uncertainty_params,
      ...
    )
    # If aggregating, we need to pad with NAs
    pred_counts_padded <- c(
      rep(
        NA,
        length(reference_times) - length(pred_counts)
      ),
      pred_counts
    )

    return(data.frame(
      pred_count = pred_counts_padded,
      time = reference_times,
      draw = i
    ))
  })

  draws_df <- Reduce(rbind, draws_df_list)
  return(draws_df)
}
