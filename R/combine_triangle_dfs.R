#' Combine triangle data.frames
#'
#' @description This function ingests a dataframe with case counts indexed by
#' reference dates and report dates for one or more strata and sums all the case
#' counts across the shared set of reference and report dates, after grid
#' expansions. Note that if there are different maximum delays across strata,
#' the grid will be expanded to use the minimum value of the maximum delay
#' across strata.
#'
#' @param data Data.frame containing the incident count of cases by reference
#'   date and report date for one or more strata.
#' @inheritParams baselinenowcast.data.frame
#' @inheritParams as_reporting_triangle
#' @importFrom stats aggregate
#' @keywords internal
#' @returns `result` Data.frame with the same column names for reference date,
#'   report date, and case count as in `data` but summed across all strata in
#'   the original data.
.combine_triangle_dfs <- function(data,
                                  delays_unit = "days",
                                  strata_cols = NULL) {
  if (!is.null(strata_cols) && length(strata_cols) > 0) {
    n_strata <- nrow(unique(data[, strata_cols, drop = FALSE]))
  } else {
    n_strata <- 1
  }

  reference_dates <- sort(unique(data$reference_date))
  select_data <- data[, c(
    "reference_date", "count",
    "delay", strata_cols
  )]
  # Find max delay within each stratum combination, then take the minimum
  max_delay <- min(tapply(select_data$delay,
    select_data[, strata_cols, drop = FALSE],
    max,
    na.rm = TRUE
  ))

  strata_values <- lapply(strata_cols, function(col) unique(data[[col]]))
  names(strata_values) <- strata_cols

  expand_list <- c(
    list(reference_date = reference_dates, delay = 0:max_delay),
    strata_values
  )

  all_combos_grid <- do.call(expand.grid, expand_list)

  all_combos <- merge(all_combos_grid, select_data,
    by = c("reference_date", "delay", strata_cols),
    all.x = TRUE
  )
  all_combos$report_date <- all_combos$reference_date +
    as.difftime(all_combos$delay, units = delays_unit)

  # Unique combinations of reference and report dates
  all_combos$pair_id <- paste(data$reference_date,
    all_combos$report_date,
    sep = "|"
  )
  # Count of how many times we see that reference and report date combination
  pair_counts <- table(all_combos$pair_id)
  valid_pairs <- names(pair_counts[pair_counts >= n_strata])
  n_dropped <- sum(pair_counts < n_strata)
  if (length(valid_pairs) == 0) {
    cli_abort(
      message = "There is no overlapping set of reference and report dates across all strata. `strata_sharing` is not possible." # nolint
    )
  }
  if (n_dropped > 0) {
    cli_warn(
      message = c("Not all reference dates and report dates combinations are available for all strata.", # nolint
        "i" = "Only the subset of reference and report dates that are available for all strata will be used to aggregate cases." # nolint
      )
    )
  }
  filtered_data <- all_combos[all_combos$pair_id %in% valid_pairs, ]
  filtered_data$pair_id <- NULL

  result <- aggregate(
    count ~ reference_date + report_date,
    data = filtered_data,
    FUN = sum,
    na.rm = TRUE
  )
  return(result)
}
