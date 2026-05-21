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
#' @param latest_obs A data.frame containing the truth to score against, with
#'   one row per (reference_date, strata) combination. Must contain a
#'   `reference_date` column and a column with observed counts named according
#'   to `observed` (default `"count"`). Additional columns shared with `data`
#'   (such as strata columns) are used as merge keys. `latest_obs` should hold
#'   the reported total at each reference date evaluated at the same
#'   `max_delay` horizon used for the nowcast (rolling truth), not the partial
#'   total available when the nowcast was run.
#' @param observed Character string giving the name of the column in
#'   `latest_obs` that holds the observed value. Defaults to `"count"` to
#'   match the input format of [baselinenowcast.data.frame()].
#' @param model Character string used as the value of the `model` column on
#'   the returned forecast object. Lets [scoringutils::summarise_scores()] run
#'   with its default `by = "model"`. Defaults to `"baselinenowcast"`. Pass
#'   `NULL` to omit the column (e.g. when `data` already carries its own
#'   `model` column).
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
#' max_delay <- 25
#' nowcast_date <- as.Date("2026-04-01")
#'
#' # Build the full reporting triangle once from the long-form data
#' full_tri <- as_reporting_triangle(syn_nssp_df) |>
#'   truncate_to_delay(max_delay = max_delay)
#'
#' # As-of view for the nowcast: drop rows past `nowcast_date`, then apply the
#' # diagonal reporting structure so cells reported after it become NA
#' n_drop <- sum(as.Date(rownames(full_tri)) > nowcast_date)
#' rep_tri <- full_tri |>
#'   truncate_to_row(t = n_drop) |>
#'   apply_reporting_structure() |>
#'   tail(n = 40)
#'
#' # Run a probabilistic nowcast
#' nowcast <- baselinenowcast(rep_tri, draws = 100)
#'
#' # Truth: total reports observed within `max_delay` of each reference date,
#' # taken from the same `full_tri` (rolling truth). `as_forecast_sample()`
#' # scores only the right-truncated reference dates that were actually
#' # nowcast (the most recent `max_delay` dates, carried on the nowcast as an
#' # attribute); the inner-join merge then restricts the truth to those dates.
#' # The scoring vignette explains why we use a rolling rather than
#' # latest-vintage truth.
#' truth_df <- as.data.frame(full_tri)
#' latest_obs <- aggregate(count ~ reference_date, data = truth_df, FUN = sum)
#'
#' # Convert and score
#' fs <- as_forecast_sample(nowcast, latest_obs)
#' fs
#' scores <- score(fs)
#' scores
#' summarise_scores(scores)
as_forecast_sample.baselinenowcast_df <- function(data,
                                                  latest_obs,
                                                  observed = "count",
                                                  model = "baselinenowcast",
                                                  ...) {
  merged <- .prepare_forecast_merge(
    data = data,
    latest_obs = latest_obs,
    observed = observed,
    model = model,
    required_output_type = "samples",
    target = "scoringutils::as_forecast_sample"
  )
  forecast_data <- scoringutils::as_forecast_sample(
    data = merged,
    observed = observed,
    predicted = "pred_count",
    sample_id = "draw",
    ...
  )
  return(forecast_data)
}

#' Convert a `baselinenowcast_df` object to a `forecast_point` object
#'
#' This function converts a point [baselinenowcast_df] object as returned by
#' [baselinenowcast()] with `output_type = "point"` to a `forecast_point`
#' object which can be used for scoring with the
#' [scoringutils](https://epiforecasts.io/scoringutils/) package.
#'
#' The nowcast point estimates in `data` are merged with the latest
#' observations in `latest_obs` on `reference_date` and any other shared
#' columns and then passed to [scoringutils::as_forecast_point()], using
#' `pred_count` as the predicted value and the column named by `observed` as
#' the observed value.
#'
#' @inheritParams as_forecast_sample.baselinenowcast_df
#' @param data A [baselinenowcast_df] object as returned by
#'   [baselinenowcast()] with `output_type = "point"`.
#' @param ... Additional arguments passed to
#'   [scoringutils::as_forecast_point()].
#'
#' @return A `forecast_point` object as returned by
#'   [scoringutils::as_forecast_point()].
#' @exportS3Method scoringutils::as_forecast_point
#' @family baselinenowcast_df
#' @examplesIf interactive() && requireNamespace("scoringutils", quietly = TRUE)
#' library(scoringutils)
#'
#' max_delay <- 25
#' nowcast_date <- as.Date("2026-04-01")
#'
#' full_tri <- as_reporting_triangle(syn_nssp_df) |>
#'   truncate_to_delay(max_delay = max_delay)
#'
#' n_drop <- sum(as.Date(rownames(full_tri)) > nowcast_date)
#' rep_tri <- full_tri |>
#'   truncate_to_row(t = n_drop) |>
#'   apply_reporting_structure() |>
#'   tail(n = 40)
#'
#' nowcast <- baselinenowcast(rep_tri, output_type = "point")
#'
#' # Rolling truth from the same full triangle. `as_forecast_point()` scores
#' # only the right-truncated nowcast dates (the most recent `max_delay`); the
#' # inner-join merge restricts the truth to those dates.
#' truth_df <- as.data.frame(full_tri)
#' latest_obs <- aggregate(count ~ reference_date, data = truth_df, FUN = sum)
#'
#' fp <- as_forecast_point(nowcast, latest_obs)
#' fp
#' scores <- score(fp)
#' scores
#' summarise_scores(scores)
as_forecast_point.baselinenowcast_df <- function(data,
                                                 latest_obs,
                                                 observed = "count",
                                                 model = "baselinenowcast",
                                                 ...) {
  merged <- .prepare_forecast_merge(
    data = data,
    latest_obs = latest_obs,
    observed = observed,
    model = model,
    required_output_type = "point",
    target = "scoringutils::as_forecast_point"
  )
  forecast_data <- scoringutils::as_forecast_point(
    data = merged,
    observed = observed,
    predicted = "pred_count",
    ...
  )
  return(forecast_data)
}

#' Shared validation and merge for forecast converters
#'
#' Validates a [baselinenowcast_df] object, joins it with observations on
#' `reference_date` plus any shared strata columns, and warns or aborts on
#' duplicate keys or missing coverage. Used by
#' [as_forecast_sample.baselinenowcast_df()] and
#' [as_forecast_point.baselinenowcast_df()].
#'
#' @inheritParams as_forecast_sample.baselinenowcast_df
#' @param required_output_type Either `"samples"` or `"point"`.
#' @param target Character label for the calling function used in error
#'   messages.
#' @returns A merged data.frame ready for the relevant `scoringutils`
#'   converter.
#' @keywords internal
.prepare_forecast_merge <- function(data,
                                    latest_obs,
                                    observed,
                                    model,
                                    required_output_type,
                                    target) {
  check_installed(
    "scoringutils",
    reason = "to convert nowcasts to scoringutils forecast objects."
  )
  assert_class(data, "baselinenowcast_df")
  assert_baselinenowcast_df(data)
  assert_data_frame(latest_obs)
  assert_character(observed, len = 1)
  assert_character(model, len = 1, null.ok = TRUE)
  assert_names(colnames(latest_obs),
    must.include = c("reference_date", observed)
  )

  if (any(data$output_type != required_output_type)) {
    other <- if (required_output_type == "samples") "point" else "samples"
    cli_abort(
      c(
        "`{target}()` requires {required_output_type}, not {other} estimates.", # nolint
        "i" = "Re-run `baselinenowcast()` with `output_type = \"{required_output_type}\"`." # nolint
      )
    )
  }

  # output_type is constant across rows; drop so it doesn't become a
  # scoringutils forecast unit column
  data$output_type <- NULL

  # Score only the right-truncated reference dates that were actually nowcast
  # (the most recent `max_delay` dates). Earlier dates are fully observed, so
  # scoring them just rewards the model for copying data. `max_delay` is
  # carried as an attribute by `baselinenowcast()`; absent it (e.g. a
  # hand-built object) we score every date.
  max_delay <- attr(data, "max_delay")
  if (!is.null(max_delay)) {
    nowcast_dates <- tail(sort(unique(data$reference_date)), max_delay)
    data <- data[data$reference_date %in% nowcast_dates, ]
  }

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

  if (!is.null(model) && !"model" %in% colnames(merged)) {
    merged$model <- model
  }

  return(merged)
}
