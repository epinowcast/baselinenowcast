#' Internal checks, asserts, and validators
#'
#' This file centralises all checking, validating, and asserting code used
#' across the package. Function names indicate intent:
#'
#' - `assert_*()`: exported assertion of an input or object class.
#' - `validate_*()`: exported validation of a constructed object.
#' - `.validate_*()`: internal validation of multiple inputs.
#' - `.check_*()`: internal predicate or low-level inspection.
#'
#' @name check
#' @keywords internal
NULL

# ---------------------------------------------------------------------------
# Class assertions and validators
# ---------------------------------------------------------------------------

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @param validate Logical. If TRUE (default), validates the object. Set to
#'   FALSE only when called from functions that already validated.
#' @return Returns `NULL` invisibly. Throws an error if validation fails.
#' @family reporting_triangle
#' @examples
#' # Validate an example reporting triangle
#' assert_reporting_triangle(example_reporting_triangle)
#'
#' @export
assert_reporting_triangle <- function(data, validate = TRUE) {
  if (isTRUE(validate)) {
    validate_reporting_triangle(data)
  }
  return(NULL)
}

#' Validate a reporting_triangle object
#'
#' @param data A [reporting_triangle] object to validate
#' @return The validated object (invisibly) or throws error
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
validate_reporting_triangle <- function(data) {
  if (!inherits(data, "matrix")) {
    cli_abort(message = "data must be a matrix")
  }
  if (!inherits(data, "reporting_triangle")) {
    cli_abort(message = "data must have class 'reporting_triangle'")
  }

  # Check matrix is not all NA
  if (all(is.na(data))) {
    cli_abort(message = "Matrix cannot be all NA")
  }

  # Check NA pattern (IF NAs exist, THEN must be in bottom-right pattern)
  if (anyNA(data)) {
    na_check <- .check_na_pattern(data)
    if (!na_check$valid) {
      n_oop <- na_check$n_out_of_pattern
      cli_abort(
        message = c(
          `!` = "Invalid reporting triangle structure",
          x = paste0(
            "Found ", n_oop, " NA value",
            if (n_oop != 1) "s" else "",
            " in unexpected position",
            if (n_oop != 1) "s" else ""
          ),
          i = paste0(
            "NA values should only appear in the bottom right ",
            "portion of the triangle"
          )
        )
      )
    }
  }

  reference_dates <- as.Date(rownames(data))
  delays_unit <- get_delays_unit(data)

  .validate_rep_tri_args(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    delays_unit = delays_unit
  )

  return(invisible(data))
}

#' Assert object has reporting_triangle class
#'
#' Lightweight class check without full validation. Use when validation
#' will occur through other operations (e.g., subsetting via `[`).
#'
#' @param data Object to check for reporting_triangle class.
#' @param arg_name Character name to use in the error message (defaults to
#'   "data").
#' @return NULL
#' @keywords internal
#' @noRd
assert_rep_tri_class <- function(data, arg_name = "data") {
  if (!is_reporting_triangle(data)) {
    cli_abort(
      message = "{arg_name} must have class 'reporting_triangle'"
    )
  }
  return(NULL)
}

#' Validate reporting_triangle constructor arguments
#'
#' Internal helper to validate the arguments passed to new_reporting_triangle.
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle data.
#' @param reference_dates Date vector of reference dates.
#' @inheritParams as_reporting_triangle
#' @return NULL
#' @keywords internal
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

#' Assert validity of `baselinenowcast_df` objects
#'
#' @param data A [baselinenowcast_df] object to check for validity.
#' @return Returns `NULL` invisibly. Throws an error if validation fails.
#' @family baselinenowcast_df
#' @examples
#' # Create a valid baselinenowcast_df object
#' valid_df <- data.frame(
#'   reference_date = as.Date("2024-01-01") + 0:4,
#'   pred_count = c(10, 15, 12, 18, 20),
#'   draw = 1,
#'   output_type = "point"
#' )
#' class(valid_df) <- c("baselinenowcast_df", "data.frame")
#'
#' # Validate the object
#' assert_baselinenowcast_df(valid_df)
#' @export
assert_baselinenowcast_df <- function(data) {
  assert_data_frame(data)

  required_cols <- c("reference_date", "pred_count")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli_abort(
      message = c(
        "Required columns missing from data",
        "x" = "Missing: {.val {missing_cols}}" # nolint
      )
    )
  }

  assert_date(data$reference_date)
  # Check for duplicated reference dates
  cols_to_check <- names(data)[names(data) %in% c("reference_date", "draw")]

  dups <- duplicated(data[, c(cols_to_check)])
  if (any(dups)) {
    cli_abort(
      message = c(
        "Data contains multiple `reference_date`s", # nolint
        "x" = "Found {sum(dups)} duplicate `reference_date`{?s}", # nolint
        "i" = "`baselinenowcast_df` objects should only contain a single estimate for each reference date." # nolint
      )
    )
  }

  return(NULL)
}

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

# ---------------------------------------------------------------------------
# NA pattern checks for reporting_triangle
# ---------------------------------------------------------------------------

#' Check NA pattern validity in reporting triangle
#'
#' Internal function that validates NA positions. Identifies NA values that
#' don't follow the expected triangular reporting delay pattern. Out-of-pattern
#' NAs occur when a value is NA but values below it (later reference dates) or
#' to its right (longer delays) are non-NA, suggesting data quality issues
#' rather than reporting delay.
#'
#' @param x A matrix or [reporting_triangle] object.
#' @return A list with components:
#'   - `valid`: Logical indicating if all NAs are in valid bottom-right pattern
#'   - `n_out_of_pattern`: Count of out-of-pattern NA values
#'
#' @details
#' An NA is considered "out-of-pattern" if:
#' - There exists a non-NA value in the same column but a later row
#'   (indicating data should have been available), OR
#' - There exists a non-NA value in the same row but a later column
#'   (indicating earlier delays were reported)
#'
#' Uses a vectorised cummax-based approach for efficient validation.
#'
#' @keywords internal
#' @noRd
.check_na_pattern <- function(x) {
  # Convert to matrix if needed
  mat <- if (inherits(x, "reporting_triangle")) {
    as.matrix(x)
  } else {
    x
  }

  # Early return if no NAs
  if (!anyNA(mat)) {
    return(list(
      valid = TRUE,
      n_out_of_pattern = 0L
    ))
  }

  # Track which NA positions are out of pattern
  out_of_pattern <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  na_positions <- is.na(mat)

  # Check rows and columns
  out_of_pattern <- .check_na_rows(na_positions, out_of_pattern)
  out_of_pattern <- .check_na_cols(na_positions, out_of_pattern)

  n_out_of_pattern <- sum(out_of_pattern)

  return(list(
    valid = n_out_of_pattern == 0L,
    n_out_of_pattern = n_out_of_pattern
  ))
}

#' Check rows for out-of-pattern NAs
#' @noRd
.check_na_rows <- function(na_positions, out_of_pattern) {
  nr <- nrow(na_positions)
  nc <- ncol(na_positions)

  for (i in seq_len(nr)) {
    row_na <- na_positions[i, ]
    na_indices <- which(row_na)

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(row_na[min_na_idx:nc])) {
        for (j in min_na_idx:nc) {
          if (j < nc && na_positions[i, j] && !all(row_na[(j + 1):nc])) {
            out_of_pattern[i, j] <- TRUE
          }
        }
      }
    }
  }
  return(out_of_pattern)
}

#' Check columns for out-of-pattern NAs
#' @noRd
.check_na_cols <- function(na_positions, out_of_pattern) {
  nr <- nrow(na_positions)
  nc <- ncol(na_positions)

  for (j in seq_len(nc)) {
    col_na <- na_positions[, j]
    na_indices <- which(col_na)

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(col_na[min_na_idx:nr])) {
        for (i in min_na_idx:nr) {
          if (i < nr && na_positions[i, j] && !all(col_na[(i + 1):nr])) {
            out_of_pattern[i, j] <- TRUE
          }
        }
      }
    }
  }
  return(out_of_pattern)
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

# ---------------------------------------------------------------------------
# Input validators
# ---------------------------------------------------------------------------

#' Validate reporting_triangle for delay estimation
#' Domain-specific checks to ensure the reporting triangle is suitable for
#'   delay estimation in [estimate_delay()].
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#' @inheritParams .validate_delay_and_triangle
#' @inheritParams estimate_delay
#' @returns NULL, invisibly
#' @keywords internal
.validate_for_delay_estimation <- function(
  triangle,
  n = nrow(triangle)
) {
  # Make sure the input triangle is of the correct class and n is an integer
  if (is.null(triangle)) {
    triangle_name <- deparse(substitute(triangle)) # nolint
    cli_abort(message = "`{triangle_name}` argument is missing.") # nolint
  }

  assert_integerish(n)

  # Convert to matrix once to avoid repeated validation in subsetting
  triangle_mat <- as.matrix(triangle)

  # Check if the triangle has a valid structure
  # Ensure each column has at least one non-NA value
  if (any(colSums(!is.na(triangle_mat)) == 0)) {
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

  n_rows <- nrow(triangle_mat)
  has_complete_row <- any(
    rowSums(is.na(triangle_mat[(n_rows - n + 1):n_rows, ])) == 0
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

  if (isFALSE(.check_lhs_not_only_zeros(triangle_mat[(n_rows - n + 1):n_rows, ]))) { # nolint
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
  first_na <- which(is.na(triangle_mat[nrow(triangle_mat), ]))[1]
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
  reporting_triangle
) {
  # Check that both inputs have the same max_delay (same number of columns)
  max_delay_point <- get_max_delay(point_nowcast_matrix)
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

# ---------------------------------------------------------------------------
# Allocation and length checks
# ---------------------------------------------------------------------------

#' Check target size against number of reference times available and the number
#'   required
#'
#' @param n_ref_times Integer indicating the number of reference times
#'    available
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns `n_used` Integer indicating how many reference times will be
#'    used
#' @keywords internal
.check_against_requirements <- function(n_ref_times,
                                        n_required,
                                        n_target,
                                        n_min_delay,
                                        n_min_retro_nowcasts,
                                        scale_factor,
                                        max_delay) {
  # Early return for simple case
  if (n_target <= n_ref_times && n_target >= n_required) {
    return(n_target)
  }

  # Handle target exceeds available reference times
  if (n_target > n_ref_times) {
    return(.handle_target_exceeds_avail(
      n_ref_times, n_required, n_target,
      n_min_delay, n_min_retro_nowcasts
    ))
  }

  # Handle target less than required
  return(.handle_target_insufficient(
    n_target, n_required, n_min_delay,
    n_min_retro_nowcasts, scale_factor, max_delay
  ))
}

#' Helper for when target exceeds available reference times
#'
#' @inheritParams .check_against_requirements
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns number of reference times to use or NULL, invisibly
#' @keywords internal
.handle_target_exceeds_avail <- function(n_ref_times,
                                         n_required,
                                         n_target,
                                         n_min_delay,
                                         n_min_retro_nowcasts) {
  if (n_ref_times >= n_required) {
    cli_warn(message = c(
      "{n_ref_times} reference times available and {n_target} are specified.", # nolint
      "i" = "All {n_ref_times} reference times will be used." # nolint
    ))
    return(n_ref_times)
  }

  cli_abort(message = c(
    "{n_ref_times} reference times available and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint
    "x" = "Probabilistic nowcasts cannot be generated." # nolint
  ))
  return(NULL)
}

#' Helper for when the target is less than the required minimum
#'
#' @inheritParams .check_against_requirements
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns NULL invisibly
#' @keywords internal
.handle_target_insufficient <- function(n_target,
                                        n_required,
                                        n_min_delay,
                                        n_min_retro_nowcasts,
                                        scale_factor,
                                        max_delay) {
  cli_abort(message = c(
    "{scale_factor*max_delay} reference times specified and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint,
    "x" = "Probabilistic nowcasts cannot be generated." # nolint
  ))
  return(NULL)
}

#' Check that a list has the required length
#'
#' @param list_obj List object to check.
#' @param name Character string giving the argument name (used in messages).
#' @param required_length Integer minimum length.
#' @param custom_msg Optional additional message to append on insufficient
#'   elements.
#' @param empty_check Logical. If TRUE (default), also error on empty lists.
#'
#' @returns NULL, invisibly
#' @keywords internal
.check_list_length <- function(list_obj, name, required_length,
                               custom_msg = NULL, empty_check = TRUE) {
  # Validate input is a list
  if (!is.list(list_obj)) {
    cli_abort(paste0("`", name, "` must be a list"))
  }

  if (length(list_obj) < required_length) {
    cli_abort(message = c(
      "Insufficient elements in `", name, "` for the `n` desired ",
      custom_msg
    ))
  }
  if (empty_check && length(list_obj) < 1) {
    cli_abort(paste0("`", name, "` is an empty list"))
  }
  return(invisible(NULL))
}
