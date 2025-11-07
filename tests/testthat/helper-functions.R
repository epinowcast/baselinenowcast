# Helper Functions for baselinenowcast Tests
#
# This file contains helper functions used across test files including:
# - Partial functions with good defaults (e.g., baselinenowcast_test)
# - Data manipulation helpers (e.g., summarise_final_day_mean)
# - Comparison helpers (e.g., expect_estimates_differ)
# - Test data creation functions (e.g., make_test_triangle, make_test_data)
# - Validation helpers (e.g., expect_triangle_output)
#
# By centralizing these patterns, we reduce boilerplate and make test
# intent clearer.

# Partial Functions --------------------------------------------------------

#' baselinenowcast with test defaults
#'
#' Wrapper around baselinenowcast with sensible defaults for testing.
#' Reduces boilerplate in tests while allowing easy override of parameters.
#'
#' @param data Input data
#' @param max_delay Maximum delay (default: 40)
#' @param draws Number of draws (default: 100)
#' @param ... Additional arguments passed to baselinenowcast
#' @return baselinenowcast output
#' @keywords internal
baselinenowcast_test <- function(data, max_delay = 40, draws = 100, ...) {
  # nolint start: object_usage_linter
  return(baselinenowcast(
    data = data,
    max_delay = max_delay,
    draws = draws,
    ...
  ))
  # nolint end
}

# Data Manipulation Helpers ------------------------------------------------

#' Summarise mean for final day
#'
#' Common pattern for extracting mean predictions for the most recent
#' reference date, grouped by specified variables.
#'
#' @param df Data frame with reference_date and pred_count columns
#' @param group_vars Variables to group by (default: reference_date, age_group)
#' @return Data frame with mean_est column
#' @keywords internal
summarise_final_day_mean <- function(df, group_vars = c(
                                       "reference_date",
                                       "age_group"
                                     )) {
  .data <- NULL
  # nolint start: nested_pipe_linter
  return(df |>
    dplyr::filter(.data$reference_date == max(.data$reference_date)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(mean_est = mean(.data$pred_count), .groups = "drop"))
  # nolint end
}

# Test Data Creation Functions ---------------------------------------------

#' Create a simple test triangle matrix
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param type Type of triangle ("simple" [fixed 5x4], "with_nas", "no_nas")
#' @return A matrix suitable for testing
#' @keywords internal
make_test_triangle <- function(nrow = 5, ncol = 4, type = "simple") {
  return(switch(type,
    simple = {
      if (nrow != 5 || ncol != 4) {
        warning(
          "Simple triangle has fixed dimensions 5x4; nrow and ncol ignored",
          call. = FALSE
        )
      }
      return(matrix(
        c(
          10, 7, 1, NA,
          15, 12, 2, NA,
          14, 16, 3, NA,
          18, 15, NA, NA,
          20, NA, NA, NA
        ),
        nrow = 5,
        ncol = 4,
        byrow = TRUE
      ))
    },
    with_nas = {
      mat <- matrix(
        seq_len(nrow * ncol),
        nrow = nrow,
        ncol = ncol
      )
      # Add NAs in bottom-right triangle (reporting triangle pattern)
      # Row 1: no NAs, Row 2: 1 trailing NA, Row 3: 2 trailing NAs, etc.
      for (i in seq_len(nrow)) {
        na_count <- i - 1
        if (na_count > 0 && na_count <= ncol) {
          na_start <- ncol - na_count + 1
          mat[i, na_start:ncol] <- NA
        }
      }
      return(mat)
    },
    no_nas = {
      return(matrix(
        seq_len(nrow * ncol),
        nrow = nrow,
        ncol = ncol
      ))
    },
    stop(
      "Invalid type: must be 'simple', 'with_nas', or 'no_nas'",
      call. = FALSE
    )
  ))
}

#' Create test delay PMF
#'
#' @param type Type of distribution ("uniform", "geometric", "simple")
#' @param length Length of PMF
#' @return A numeric vector representing a delay PMF
#' @keywords internal
make_delay_pmf <- function(type = "uniform", length = 4) {
  return(switch(type,
    uniform = {
      return(rep(1 / length, length))
    },
    geometric = {
      pmf <- dgeom(0:(length - 1), prob = 0.3)
      return(pmf / sum(pmf))
    },
    simple = {
      if (length != 4) {
        warning(
          "Simple PMF has fixed length 4, ignoring length parameter",
          call. = FALSE
        )
      }
      return(c(0.4, 0.3, 0.2, 0.1))
    },
    stop(
      "Invalid type: must be 'uniform', 'geometric', or 'simple'",
      call. = FALSE
    )
  ))
}

#' Create simple test data frame with reference/report dates
#'
#' @param n_dates Number of reference dates
#' @param max_delay Maximum delay
#' @param add_strata Whether to add strata columns
#' @param seed Optional seed for reproducibility (uses rpois and sample)
#' @return A data frame with reference_date, report_date, count columns
#' @keywords internal
make_test_data <- function(n_dates = 10, max_delay = 5, add_strata = FALSE,
                           seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  reference_dates <- seq.Date(
    from = as.Date("2024-01-01"),
    by = "day",
    length.out = n_dates
  )

  data_list <- lapply(reference_dates, function(ref_date) {
    delays <- 0:max_delay
    report_dates <- ref_date + delays
    counts <- rpois(length(delays), lambda = 10)

    return(data.frame(
      reference_date = ref_date,
      report_date = report_dates,
      count = counts
    ))
  })

  test_df <- do.call(rbind, data_list)

  if (add_strata) {
    test_df$location <- "Location1"
    test_df$age_group <- sample(c("00-04", "05-17", "18+"), nrow(test_df),
      replace = TRUE
    )
  }

  return(test_df)
}

#' Create COVID test data
#'
#' Creates a standardized COVID-19 hospitalization test dataset from the
#' germany_covid19_hosp package data. This is used across multiple test
#' files to ensure consistency.
#'
#' @param age_groups Character vector of age groups to include
#' @param add_weekday Whether to add weekday_ref_date column
#' @return Data frame with filtered COVID data
#' @keywords internal
create_covid_test_data <- function(
    age_groups = c("00+", "00-04", "60-79", "80+"),
    add_weekday = TRUE) {
  # nolint start: object_usage_linter
  covid_data <- germany_covid19_hosp[
    germany_covid19_hosp$report_date <=
      max(germany_covid19_hosp$reference_date) &
      germany_covid19_hosp$age_group %in% age_groups,
  ]
  # nolint end

  if (add_weekday) {
    covid_data$weekday_ref_date <- lubridate::wday(
      covid_data$reference_date,
      label = TRUE
    )
  }

  return(covid_data)
}
