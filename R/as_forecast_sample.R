#' Convert a `baselinenowcast_df` object to a `forecast_sample` object
#'
#' This function converts a [baselinenowcast_df] object as returned by
#' [baselinenowcast()] to a `forecast_sample` object which can be used
#' for scoring with the
#' [scoringutils](https://epiforecasts.io/scoringutils/) package.
#'
#' The nowcast samples in `data` are merged with the latest available
#' observations in `latest_obs` on `reference_date` and any other shared
#' columns (e.g. strata columns such as `age_group` or `location`).
#' The merged data is then passed to [scoringutils::as_forecast_sample()],
#' using `pred_count` as the predicted value, `draw` as the sample
#' identifier, and the column named by `observed` as the observed value.
#'
#' @param data A [baselinenowcast_df] object as returned by
#'   [baselinenowcast()] with `output_type = "samples"`.
#' @param latest_obs A data.frame containing the latest observed counts to
#'   score against. Must contain a `reference_date` column and a column with
#'   observed counts named according to `observed` (default `"count"`).
#'   Additional columns shared with `data` (such as strata columns) are used
#'   as merge keys.
#' @param observed Character string giving the name of the column in
#'   `latest_obs` that holds the observed value. Defaults to `"count"` to
#'   match the input format of [baselinenowcast.data.frame()].
#' @param ... Additional arguments passed to
#'   [scoringutils::as_forecast_sample()].
#'
#' @return A `forecast_sample` object as returned by
#'   [scoringutils::as_forecast_sample()].
#' @exportS3Method scoringutils::as_forecast_sample
#' @importFrom checkmate assert_class
#' @importFrom rlang check_installed
#' @family baselinenowcast_df
#' @examplesIf interactive() && requireNamespace("scoringutils", quietly = TRUE)
#' library(scoringutils)
#'
#' nowcast <- baselinenowcast(example_reporting_triangle, draws = 100)
#'
#' # Construct a small set of latest observations to score against
#' latest_obs <- data.frame(
#'   reference_date = get_reference_dates(example_reporting_triangle),
#'   count = rowSums(example_reporting_triangle, na.rm = TRUE)
#' )
#'
#' as_forecast_sample(nowcast, latest_obs)
as_forecast_sample.baselinenowcast_df <- function(data,
                                                  latest_obs,
                                                  observed = "count",
                                                  ...) {
  check_installed(
    "scoringutils",
    reason = "to convert nowcasts to forecast_sample objects."
  )
  assert_class(data, "baselinenowcast_df")
  assert_data_frame(latest_obs)
  assert_character(observed, len = 1)
  assert_names(colnames(latest_obs),
    must.include = c("reference_date", observed)
  )

  if (any(data$output_type != "samples")) {
    cli_abort(
      c(
        "`as_forecast_sample()` requires samples, not point estimates.",
        "i" = "Re-run `baselinenowcast()` with `output_type = \"samples\"`." # nolint
      )
    )
  }

  # output_type is constant across rows; drop so it doesn't become a
  # scoringutils forecast unit column
  data$output_type <- NULL

  merge_cols <- intersect(colnames(data), colnames(latest_obs))
  merge_cols <- setdiff(merge_cols, c("pred_count", "draw", observed))
  if (!"reference_date" %in% merge_cols) {
    cli_abort("`latest_obs` must share a `reference_date` column with `data`.")
  }

  if (anyDuplicated(latest_obs[merge_cols])) {
    cli_abort(
      c(
        "`latest_obs` has duplicated rows for the merge keys ({.val {merge_cols}}).", # nolint
        "i" = "Provide one observed value per merge key combination." # nolint
      )
    )
  }

  merged <- merge(data, latest_obs, by = merge_cols)

  if (nrow(merged) == 0) {
    cli_abort(
      "No rows in `data` share the merge keys ({.val {merge_cols}}) with `latest_obs`." # nolint
    )
  }
  if (nrow(merged) < nrow(data)) {
    n_dropped <- nrow(data) - nrow(merged)
    cli_warn(
      c(
        "Dropped {n_dropped} nowcast row{?s} with no matching observation in `latest_obs`.", # nolint
        "i" = "Check that `latest_obs` covers every merge key combination ({.val {merge_cols}}) in `data`." # nolint
      )
    )
  }

  forecast_data <- scoringutils::as_forecast_sample(
    data = merged,
    observed = observed,
    predicted = "pred_count",
    sample_id = "draw",
    ...
  )
  return(forecast_data)
}
