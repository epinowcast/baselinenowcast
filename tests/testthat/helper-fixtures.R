# Test Data Fixtures for baselinenowcast Tests
#
# This file contains functions for creating common test data patterns used
# across multiple test files. By centralizing test data creation, we ensure
# consistency and reduce duplication.

# Matrix Fixtures ----------------------------------------------------------

#' Create a simple test triangle matrix
#'
#' @param nrow Number of rows
#' @param ncol Number of columns
#' @param type Type of triangle ("simple", "with_nas", "no_nas")
#' @return A matrix suitable for testing
#' @keywords internal
make_test_triangle <- function(nrow = 5, ncol = 4, type = "simple") {
  if (type == "simple") {
    matrix(
      c(
        10, 7, 1, NA,
        15, 12, 2, NA,
        14, 16, 3, NA,
        18, 15, NA, NA,
        20, NA, NA, NA
      ),
      nrow = nrow,
      ncol = ncol,
      byrow = TRUE
    )
  } else if (type == "with_nas") {
    mat <- matrix(
      seq_len(nrow * ncol),
      nrow = nrow,
      ncol = ncol
    )
    # Add NAs in bottom-right triangle
    for (i in seq_len(nrow)) {
      na_start <- ncol - (nrow - i)
      if (na_start <= ncol && na_start > 0) {
        mat[i, na_start:ncol] <- NA
      }
    }
    mat
  } else if (type == "no_nas") {
    matrix(
      seq_len(nrow * ncol),
      nrow = nrow,
      ncol = ncol
    )
  }
}

#' Create test delay PMF
#'
#' @param type Type of distribution ("uniform", "geometric", "simple")
#' @param length Length of PMF
#' @return A numeric vector representing a delay PMF
#' @keywords internal
make_delay_pmf <- function(type = "uniform", length = 4) {
  if (type == "uniform") {
    pmf <- rep(1 / length, length)
  } else if (type == "geometric") {
    pmf <- dgeom(0:(length - 1), prob = 0.3)
    pmf <- pmf / sum(pmf)
  } else if (type == "simple") {
    pmf <- c(0.4, 0.3, 0.2, 0.1)
    if (length != 4) {
      warning(
        "Simple PMF has fixed length 4, ignoring length parameter",
        call. = FALSE
      )
    }
  }
  pmf
}

# Data Frame Fixtures ------------------------------------------------------

#' Create simple test data frame with reference/report dates
#'
#' @param n_dates Number of reference dates
#' @param max_delay Maximum delay
#' @param add_strata Whether to add strata columns
#' @return A data frame with reference_date, report_date, count columns
#' @keywords internal
make_test_data <- function(n_dates = 10, max_delay = 5, add_strata = FALSE) {
  reference_dates <- seq.Date(
    from = as.Date("2024-01-01"),
    by = "day",
    length.out = n_dates
  )

  data_list <- lapply(reference_dates, function(ref_date) {
    delays <- 0:max_delay
    report_dates <- ref_date + delays
    counts <- rpois(length(delays), lambda = 10)

    data.frame(
      reference_date = ref_date,
      report_date = report_dates,
      count = counts
    )
  })

  df <- do.call(rbind, data_list)

  if (add_strata) {
    df$location <- "Location1"
    df$age_group <- sample(c("00-04", "05-17", "18+"), nrow(df),
      replace = TRUE
    )
  }

  df
}
