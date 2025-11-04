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
#' @importFrom stats aggregate as.formula
#' @returns `result` Data.frame with the same column names for reference date,
#'   report date, and case count as in `data` but summed across all strata in
#'   the original data.
.combine_triangle_dfs <- function(data) {
  group_cols <- c("reference_date", "report_date")
  value_col <- "count"

  # Unique combinations of reference and report dates
  data$pair_id <- paste(data$reference_date, data$report_date, sep = "|")
  # Count of how many times we see that reference and report date combination
  pair_counts <- table(data$pair_id)
  # If there are a different set of reference and report dates per strata, subset to
  # the overlapping set.
  if (length(unique(pair_counts)) != 1) {
    cli_warn(
      message = c("The data being aggregated contains a different number of unique reference and report date combinations.", # nolint
        "i" = "Only the fully overlapping subset of reference and report dates will be used to aggregate cases." # nolint
      )
    )
    valid_pairs <- names(pair_counts[pair_counts >= max(pair_counts)])
    n_dropped <- sum(pair_counts < max(pair_counts))
    filtered_data <- data[data$pair_id %in% valid_pairs, ]
    filtered_data$pair_id <- NULL
    data <- filtered_data
  }

  formula_str <- paste(value_col, "~", paste(group_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)

  result <- aggregate(
    formula_obj,
    data = data,
    FUN = sum,
    na.rm = TRUE
  )
  return(result)
}
