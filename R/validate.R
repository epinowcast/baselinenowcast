#' Validate triangle
#' Various checks to make sure that the reporting triangle passed in to
#'   [estimate_delay()] is formatted properly.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @inheritParams .validate_delay_and_triangle
#' @inheritParams estimate_delay
#' @returns NULL, invisibly
#' @keywords internal
.validate_triangle <- function(
    triangle,
    n = nrow(triangle)) {
  # Make sure the input triangle is of the correct class and n is an integer
  if (is.null(triangle)) {
    triangle_name <- deparse(substitute(triangle)) # nolint
    cli_abort(message = "`{triangle_name}` argument is missing.") # nolint
  }
  assert_class(triangle, "matrix")
  assert_integerish(n)
  assert_matrix(triangle, all.missing = FALSE)

  # Check if the triangle has a valid structure
  # Ensure each column has at least one non-NA value
  if (any(colSums(!is.na(triangle)) == 0)) {
    cli_abort(
      message = c(
        "Invalid reporting triangle structure. Each column must have",
        "at least one non-NA value."
      )
    )
  }

  if (nrow(triangle) < n) {
    cli_abort(
      message = c(
        "Number of observations in input reporting triangle is insufficient",
        "for the user specified number of historical observations to use",
        "for delay estimaton."
      )
    )
  }

  if (n < 1) {
    cli_abort(
      message = "Insufficient `n`, must be greater than or equal to 1."
    )
  }

  n_rows <- nrow(triangle)
  has_complete_row <- any(
    rowSums(is.na(triangle[(n_rows - n + 1):n_rows, ])) == 0
  )
  if (isFALSE(has_complete_row)) {
    cli_abort(
      message = c(
        "The rows used for delay estimation in the reporting triangle must ",
        "contain at least one row with no missing observations. Consider ",
        "increasing `n` to ensure a complete row of the reporting triangle is ",
        "being used for delay estimation."
      )
    )
  }

  if (isFALSE(.check_lhs_not_only_zeros(triangle[(n_rows - n + 1):n_rows, ]))) { # nolint
    cli_abort(
      message = c(
        "The values for the recent reference times and delays only contain 0s,",
        "which means the method to iteratively complete the reporting triangle",
        "and estimate a delay PMF will be invalid. Consider increasing `n` to ",
        "capture reference times that contain observations for early delays",
        "or truncating to an earlier reference time to ensure a nowcast, ",
        "not a forecast, is being produced. "
      )
    )
  }
  first_na <- which(is.na(triangle[nrow(triangle), ]))[1]
  if (!is.na(first_na) && first_na == 1) {
    cli_abort(
      message = c(
        "The last row of the reporting triangle contains an NA in the first",
        "column. There must be at least an entry for the first delay at the",
        "latest reference time."
      )
    )
  }

  return(NULL)
}

#' Check if matrix has valid NA pattern
#'
#' @param mat Matrix
#'
#' @returns Boolean indicating whether the matrix only contains NAs in the
#'    bottom right (TRUE if only in bottom right, FALSE if elsewhere).
#' @keywords internal
.check_na_bottom_right <- function(mat) {
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)

  for (i in 1:n_rows) {
    row_data <- mat[i, ]
    na_indices <- which(is.na(row_data))

    # If there are NAs in this row
    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      # Check that all entries from the first NA onwards are also NA
      if (!all(is.na(row_data[min_na_idx:n_cols]))) {
        return(FALSE)
      }
    }
  }

  # Check column consistency (if a cell is NA, all cells below it must be NA)
  for (j in 1:n_cols) {
    col_data <- mat[, j]
    na_indices <- which(is.na(col_data))

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(is.na(col_data[min_na_idx:n_rows]))) {
        return(FALSE)
      }
      # Check that all entries above the first NA are not NA
      if (min_na_idx > 1 && anyNA(col_data[1:(min_na_idx - 1)])) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

#' Check if there are non-zero-values on the LHS of NAs
#'
#' @param mat Matrix to check
#'
#' @returns Boolean indicating whether or not there are non-zero values on the
#'    LHS of the first NA (TRUE = has non-zeros, FALSE = only zeros)
#' @keywords internal
.check_lhs_not_only_zeros <- function(mat) {
  # Find first NA
  first_na <- which(is.na(mat[nrow(mat), ]))[1]
  if (is.na(first_na)) {
    has_non_zeros <- TRUE
  } else if (first_na == 1) {
    has_non_zeros <- TRUE # No columns to check
  } else {
    mat_LHS <- mat[, 1:(first_na) - 1]
    has_non_zeros <- !all(mat_LHS == 0)
  }
  return(has_non_zeros)
}

#' Validate triangle to nowcast and delay PMF together
#' Various checks to make sure that the reporting triangle  and the delay PMF
#'   passed in to [apply_delay()] are formatted properly and compatible.
#' @param triangle Matrix of values with rows indicating the time points and
#'   columns indicating the delays.
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @returns NULL, invisibly
#' @keywords internal
.validate_delay_and_triangle <- function(triangle, delay_pmf) {
  # Check that the inputs are the correct type
  assert_class(triangle, "matrix")
  assert_class(delay_pmf, "numeric")
  assert_matrix(triangle, all.missing = FALSE)

  # Make sure the triangle has the same number of columns as the delay
  if ((ncol(triangle) != length(delay_pmf))) {
    cli_abort(
      message = c(
        "Length of the delay PMF is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay PMF using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }

  # Make sure that nowcasts can be generated iteratively, ensuring delay_pmf[1]
  # !=0 while the triangle is a reporting triangle (NAs all in bottom right)
  if (is.na(triangle[nrow(triangle), 2]) && delay_pmf[1] == 0) {
    cli_abort(
      message = c(
        "Value of delay PMF at delay = 0 is 0, and the latest reference time ",
        "in the reporting matrix only contains a value at delay = 0. There is",
        "insufficient information to generate a point nowcast for the latest ",
        "reference time. Consider truncating to an earlier reference time to ",
        "ensure a nowcast, not a forecast, is being produced."
      )
    )
  }

  # Check that first PMF entry is not negative
  if (delay_pmf[1] < 0) {
    cli_abort(
      message = c(
        "x" = "First entry of delay PMF (delay = 0) is negative ({delay_pmf[1]}).", # nolint
        "i" = "Negative PMF entries are only valid for later delays where they represent systematic downward corrections.", # nolint
        "i" = "The first delay must have a non-negative probability as it represents the baseline reporting pattern." # nolint
      )
    )
  }

  return(NULL)
}

#' Validate the inputs to `estimate_and_apply_uncertainty()` to ensure that
#'    the reporting triangle, point nowcast matrix, and specified maximum delay
#'    are correct.
#'
#' @inheritParams estimate_and_apply_uncertainty
#'
#' @returns NULL, invisibly
#' @keywords internal
.validate_multiple_inputs <- function(
    point_nowcast_matrix,
    reporting_triangle) {
  # Check that both inputs have the same max_delay (same number of columns)
  max_delay_point <- ncol(point_nowcast_matrix) - 1
  max_delay_rt <- get_max_delay(reporting_triangle)

  if (max_delay_point != max_delay_rt) {
    cli_abort(c(
      "x" = "`point_nowcast_matrix` and `reporting_triangle` must have the same max_delay.", # nolint
      "i" = "Got max_delay of {max_delay_point} and {max_delay_rt} respectively." # nolint
    ))
  }

  return(NULL)
}

#' Helper function to validate allocation parameters
#'
#'
#' @inheritParams allocate_reference_times
#' @importFrom checkmate assert_scalar assert_numeric assert_integerish
#'
#' @returns NULL invisibly
#' @keywords internal
.validate_inputs_allocation <- function(scale_factor,
                                        prop_delay,
                                        n_min_retro_nowcasts) {
  assert_integerish(n_min_retro_nowcasts, lower = 0)
  assert_scalar(prop_delay)
  assert_numeric(prop_delay, lower = 0, upper = 1, finite = TRUE)
  assert_scalar(scale_factor)
  assert_numeric(scale_factor, lower = 0, finite = TRUE)
  return(NULL)
}


#' Validate the specified number of reference times meets the minimum
#'    requirements
#'
#' @param n_ref_times Integer indicating the number of reference times
#'    available.
#' @param n_min_delay Integer indicating minimum number of
#'    reference times needed for delay estimation.
#' @inheritParams estimate_and_apply_uncertainty
#' @inheritParams allocate_reference_times
#' @returns NULL, invisibly
#' @keywords internal
.validate_inputs_uncertainty <- function(n_ref_times,
                                         n_min_delay,
                                         n_history_delay,
                                         n_retrospective_nowcasts,
                                         n_min_retro_nowcasts = 2) {
  assert_integerish(n_ref_times, lower = 0)
  assert_integerish(n_min_delay, lower = 0)
  assert_integerish(n_history_delay, lower = 0)
  assert_integerish(n_retrospective_nowcasts, lower = 0)
  assert_integerish(n_min_retro_nowcasts, lower = 0)

  if (n_ref_times < n_history_delay + n_retrospective_nowcasts) {
    cli_abort(message = c(
      "Insufficient reference times in reporting triangle for specified `n_history_delay` and `n_retrospective_nowcasts`.", # nolint
      "i" = "{n_history_delay + n_retrospective_nowcasts} reference times are specified for delay and uncertainty estimation.", # nolint
      "x" = "Only {n_ref_times} reference times are available in the reporting triangle." # nolint
    ))
  }

  if (n_history_delay < n_min_delay) {
    cli_abort(message = c(
      "Insufficient `n_history_delay`.", # nolint
      "i" = "{n_min_delay} reference times needed for delay estimation.", # nolint
      "x" = "{n_history_delay} reference times were specified." # nolint
    ))
  }

  if (n_retrospective_nowcasts < n_min_retro_nowcasts) {
    cli_abort(message = c(
      "Insufficient `n_retrospective_nowcasts`.", # nolint
      "i" = "{n_min_retro_nowcasts} reference times needed for uncertainty estimation.", # nolint
      "x" = "{n_retrospective_nowcasts} reference times were specified." # nolint
    ))
  }

  return(NULL)
}

#' Check observations and predictions are compatible
#'
#' @param obs Matrix or vector of observations.
#' @param pred Matrix or vector of predictions.
#'
#' @returns NULL, invisibly
#' @keywords internal
.check_obs_and_pred <- function(obs, pred) {
  if (is.null(obs) || is.null(pred)) {
    cli_abort("Missing `obs` and/or `pred`") # nolint
  }
  # Coerce vectors/data.frames to matrices and validate numeric
  obs <- as.matrix(obs)
  pred <- as.matrix(pred)
  if (!is.numeric(obs) || !is.numeric(pred)) {
    cli_abort("`obs` and `pred` must be numeric (after coercion to matrix).")
  }

  # Ensure obs and pred have the same dimensions
  if (!identical(dim(obs), dim(pred))) {
    cli_abort("`obs` and `pred` must have the same dimensions") # nolint
  }
  return(NULL)
}

#' Validate the reporting triangle data.frame
#' @description Checks for duplicate reference date report dates, missing
#'    columns, report dates beyond the final reference date, and missing
#'    combinations of delays and reports.
#'
#' @param data Data.frame in long tidy form with reference dates, report dates,
#'   and case counts, used to create a `reporting_triangle` object.
#' @inheritParams as_reporting_triangle.data.frame
#'
#' @importFrom checkmate assert_data_frame
#' @returns NULL, invisibly
#' @keywords internal
.validate_rep_tri_df <- function(data,
                                 delays_unit) {
  assert_data_frame(data)
  # Validate inputs
  required_cols <- c(
    "reference_date",
    "report_date",
    "count"
  )
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli_abort(
      message = c(
        "Required columns missing from data",
        "x" = "Missing: {.val {missing_cols}}" # nolint
      )
    )
  }

  # Check for distinct pairs of reference dates and report dates
  dup_pairs <- duplicated(data[, c("reference_date", "report_date")])

  if (any(dup_pairs)) {
    cli_abort(
      message = c(
        "Data contains duplicate `reference_date` and `report_date` combinations", # nolint
        "x" = "Found {sum(dup_pairs)} duplicate pair{?s}", # nolint
        "i" = "Each reference_date and report_date combination should appear only once" # nolint
      )
    )
  }

  if (max(data$report_date) > max(data$reference_date)) {
    cli_alert_info(
      text = "The dataframe contains report dates beyond the final reference date." # nolint
    )
  }

  # Check that all reference dates from min to max are available
  all_dates_length <- length(seq(
    from = min(data$reference_date),
    to = max(data$reference_date),
    by = {{ delays_unit }}
  ))
  if (all_dates_length != length(unique(data$reference_date))) {
    cli_alert_info(
      text =
        "Data does not contain case counts for all possible reference dates."
    )
  }
  return(NULL)
}

#' Validate the uncertainty parameters if they are passed in
#'
#' @inheritParams .validate_delay_and_triangle
#' @inheritParams sample_prediction
#'
#' @returns NULL invisibly
#' @keywords internal
.validate_uncertainty <- function(triangle,
                                  uncertainty_params) {
  assert_numeric(uncertainty_params)
  n_possible_horizons <- sum(is.na(rowSums(triangle)))
  if (n_possible_horizons != length(uncertainty_params)) {
    cli_abort(
      message = c("`uncertainty_params` are not the same length as the number of horizons in the reporting triangle.") # nolint
    )
  }
  return(NULL)
}


#' Validate the delay PMF if it is passed in
#'
#' @inheritParams .validate_delay_and_triangle
#' @inheritParams sample_prediction
#'
#' @returns NULL invisibly
#' @importFrom checkmate check_numeric
#' @keywords internal
.validate_delay <- function(triangle,
                            delay_pmf) {
  pmf_sum <- sum(delay_pmf)
  test <- check_numeric(pmf_sum, lower = 0.99, upper = 1.01, len = 1)
  if (!isTRUE(test)) {
    cli_alert_info(
      "Delay PMF does not sum to approximately 1 (sum = {round(pmf_sum, 4)}). This may be expected when working with corrections or incomplete data, but could also indicate an error in the delay estimation." # nolint: line_length_linter
    )
  }

  n_delays <- ncol(triangle)
  if (n_delays != length(delay_pmf)) {
    cli_abort(
      message = c("`delay_pmf` is not the same length as the number of delays in the reporting triangle.") # nolint: line_length_linter
    )
  }
  return(NULL)
}

#' Validate each item in the reporting triangle
#'
#' @inheritParams new_reporting_triangle
#' @inheritParams as_reporting_triangle.matrix
#' @inheritParams as_reporting_triangle
#'
#' @returns NULL
#' @keywords internal
#' Assert delays_unit is valid
#'
#' @param delays_unit Character string specifying the temporal granularity
#' @returns NULL, invisibly. Stops execution with error if validation fails.
#' @keywords internal
assert_delays_unit <- function(delays_unit) {
  assert_character(delays_unit, len = 1)
  assert_choice(delays_unit,
    choices = c("days", "weeks", "months", "years")
  )
  return(invisible(NULL))
}

.validate_rep_tri_args <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   delays_unit) {
  assert_matrix(reporting_triangle_matrix)
  assert_date(reference_dates,
    unique = TRUE,
    null.ok = FALSE,
    min.len = 1,
    len = nrow(reporting_triangle_matrix)
  )

  assert_delays_unit(delays_unit)
  return(NULL)
}

#' Validate each of the strata columns passed to baselinenowcast
#'
#' @inheritParams baselinenowcast.data.frame
#' @keywords internal
#' @returns NULL
.validate_strata_cols <- function(strata_cols,
                                  data) {
  # Ensure nowcast_unit is not reference_date, report_date, count
  conflicting_cols <- intersect(strata_cols, c(
    "reference_date",
    "report_date",
    "count"
  ))
  if (length(conflicting_cols) > 0) {
    cli_abort(
      message = c(
        "`strata_cols` cannot contain any of the required columns of: reference_date, report_date, count .", # nolint
        "i" = "Found: {conflicting_cols} in `strata_cols`" # nolint
      )
    )
  }

  if (!all(strata_cols %in% colnames(data))) {
    cli_abort(
      message =
        c("`strata_cols`, if specified, must be a column in `data`.",
          "i" = "{strata_cols[!strata_cols %in% colnames(data)]} is not a column in `data`." # nolint
        )
    )
  }
  return(NULL)
}
