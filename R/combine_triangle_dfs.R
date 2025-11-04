#' Combine triangle data.frames
#'
#' @description This function ingests a dataframe with case counts indexed by
#' reference dates and report dates for one or more strata and sums all the case
#' counts across the shared set of reference and report dates. Note that this
#' may be a subset of the unique set of reference and report date combinations
#' in the original data.
#'
#' @param data Data.frame containing the incident count of cases by reference
#'   date and report date for one or more strata.
#' @inheritParams baselinenowcast.data.frame
#' @importFrom stats aggregate as.formula
#' @returns `result` Data.frame with the same column names for reference date,
#'   report date, and case count as in `data` but summed across all strata in
#'   the original data.
.combine_triangle_dfs <- function(data,
                                  strata_cols = NULL) {
  group_cols <- c("reference_date", "report_date")
  value_col <- "count"
  if (!is.null(strata_cols) && length(strata_cols) > 0) {
    n_strata <- nrow(unique(data[, strata_cols, drop = FALSE]))
  } else {
    n_strata <- 1
  }
  # Unique combinations of reference and report dates
  data$pair_id <- paste(data$reference_date, data$report_date, sep = "|")
  # Count of how many times we see that reference and report date combination
  pair_counts <- table(data$pair_id)
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
  filtered_data <- data[data$pair_id %in% valid_pairs, ]
  filtered_data$pair_id <- NULL

  formula_str <- paste(value_col, "~", paste(group_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)

  result <- aggregate(
    formula_obj,
    data = filtered_data,
    FUN = sum,
    na.rm = TRUE
  )
  return(result)
}
