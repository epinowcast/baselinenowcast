# Create a triangle with known delay PMF
sim_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)

# Generate counts for each reference date
counts <- c(100, 150, 200, 250, 300)

# Create a complete triangle based on the known delay PMF
complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

# Create a reporting triangle with NAs in the lower right
reporting_triangle <- construct_triangle(complete_triangle)

test_that("estimate_delay returns a valid probability mass function", {
  # Get delay estimate
  result <- estimate_delay(reporting_triangle)

  # Test that the result is a numeric vector
  expect_is(result, "numeric")

  # Test that the length matches the number of columns in the triangle
  expect_identical(
    as.integer(length(result)), as.integer(ncol(reporting_triangle))
  )

  # Test that all values are valid probabilities
  expect_true(all(result >= 0 & result <= 1))

  # Test that the PMF sums to 1
  expect_equal(sum(result), 1, tolerance = 1e-6)

  # Test that the estimated PMF matches the true PMF
  expect_equal(result, sim_delay_pmf, tolerance = 1e-6)
})

test_that("estimate_delay handles custom max_delay parameter", {
  # Test with max_delay = 3 (full delay)
  result_full <- estimate_delay(reporting_triangle, max_delay = 3)
  expect_identical(as.integer(length(result_full)), 4L)
  expect_equal(result_full, sim_delay_pmf, tolerance = 1e-6)

  # Test with max_delay = 2 (truncated delay)
  result_truncated <- estimate_delay(reporting_triangle, max_delay = 2)
  expect_identical(as.integer(length(result_truncated)), 3L)

  expected_truncated <- sim_delay_pmf[1:3] / sum(sim_delay_pmf[1:3])
  expect_equal(result_truncated, expected_truncated, tolerance = 1e-6)
})

test_that("estimate_delay handles custom n_history parameter", {
  # Test with different n values
  result_full <- estimate_delay(reporting_triangle, n = 5)
  result_partial <- estimate_delay(reporting_triangle, n = 4)

  # Both should return the correct PMF
  expect_equal(result_full, sim_delay_pmf, tolerance = 1e-6)
  expect_equal(result_partial, sim_delay_pmf, tolerance = 1e-6)
})

test_that("estimate_delay validates input parameters correctly", {
  # Test invalid max_delay
  expect_error(estimate_delay(reporting_triangle, max_delay = 0))

  # Test invalid n
  expect_error(estimate_delay(reporting_triangle, n = 0))

  # Test n > nrow(reporting_triangle)
  expect_error(estimate_delay(reporting_triangle, n = 10))

  # Test max_delay >= ncol(reporting_triangle) -- should error
  expect_error(estimate_delay(reporting_triangle, max_delay = 5),
    regexp = "The maximum delay must be less than the number of columns"
  ) # nolint
})

test_that(
  "estimate_delay errors when NAs are in upper part of reporting triangle",
  {
    # Add NA in the upper part where it shouldn't be
    triangle_with_na <- reporting_triangle
    triangle_with_na[1, 2] <- NA

    expect_error(estimate_delay(triangle_with_na))
  }
)

test_that("estimate_delay errors if not passed a matrix", {
  # Create a vector instead of a matrix
  triangle_single_day <- reporting_triangle[1, ]
  expect_error(estimate_delay(triangle_single_day))
})

test_that("estimate_delay calculates correct PMF with complete matrix", {
  # Create a triangle with known delay PMF
  complete_pmf <- c(0.45, 0.25, 0.2, 0.1)

  # Generate counts for each reference date
  complete_counts <- c(80, 100, 90, 80, 70)

  # Create a complete triangle based on the known delay PMF
  full_triangle <- lapply(complete_counts, function(x) round(x * complete_pmf))
  full_triangle <- do.call(rbind, full_triangle)

  delay_pmf <- estimate_delay(
    reporting_triangle = full_triangle,
    max_delay = 3,
    n = 5
  )

  expect_equal(delay_pmf, complete_pmf, tolerance = 0.001)
})

test_that(
  "estimate_delay works with every other day reporting of daily data",
  {
    sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

    counts <- c(30, 40, 50, 60, 70)

    complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
    complete_triangle <- do.call(rbind, complete_triangle)

    reporting_triangle <- construct_triangle(
      complete_triangle,
      structure = 2
    )

    # Get delay estimate
    delay_pmf <- estimate_delay(
      reporting_triangle = reporting_triangle
    )
    # Test that the function returns the expected PMF
    expect_identical(delay_pmf, sim_delay_pmf)
  }
)

test_that("estimate_delay handles diagonal reporting triangles", {
  # Create a triangle with known delay PMF
  partial_pmf <- c(0.4, 0.2, 0.2, 0.2)

  # Generate counts for each reference date
  partial_counts <- c(80, 100, 90, 80, 70)

  # Create a complete triangle based on the known delay PMF
  partial_complete <- lapply(partial_counts, function(x) x * partial_pmf)
  partial_complete <- do.call(rbind, partial_complete)

  # Create a reporting triangle with NAs in the lower right
  partial_triangle <- construct_triangle(
    partial_complete,
    structure = 1
  )

  delay_pmf <- estimate_delay(
    reporting_triangle = partial_triangle,
    max_delay = 3,
    n = 4
  )

  expect_is(delay_pmf, "numeric")
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
  expect_equal(delay_pmf, partial_pmf, tolerance = 1e-6)
})

test_that(
  "estimate_delay with preprocess = preprocess_negative_values handles negatives", # nolint
  {
    # Use example data with negative values
    triangle_neg <- matrix(
      c(
        100, 60, -20, 10,
        120, 70, -25, 15,
        110, 65, -22, 12,
        130, 75, -28, 18,
        115, 68, -24, 14,
        125, 72, -26, NA,
        105, 62, NA, NA,
        95, NA, NA, NA
      ),
      nrow = 8,
      byrow = TRUE
    )

    # Default behaviour should handle negatives
    delay_pmf <- estimate_delay(
      reporting_triangle = triangle_neg,
      max_delay = 3,
      n = 5
    )

    # Should return valid PMF without negative entries
    expect_true(all(delay_pmf >= 0))
    expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
  }
)

test_that("estimate_delay with preprocess = NULL preserves negative values", {
  # Use example data with negative values
  triangle_neg <- matrix(
    c(
      100, 60, -20, 10,
      120, 70, -25, 15,
      110, 65, -22, 12,
      130, 75, -28, 18,
      115, 68, -24, 14,
      125, 72, -26, NA,
      105, 62, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 8,
    byrow = TRUE
  )

  # With preprocess = NULL, negatives should be preserved
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_neg,
    max_delay = 3,
    n = 5,
    preprocess = NULL
  )

  # Should return a PMF that sums to 1
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)

  # PMF can have negative entries
  expect_true(any(delay_pmf < 0))
})

test_that("estimate_delay can handle more extreme negative backfill", {
  triangle <- matrix(
    c(
      10, 6, -30, 10,
      12, 7, -35, 15,
      11, 6, -32, 12,
      13, 7, -38, 18,
      11, 6, -34, 14,
      12, 7, -36, NA,
      10, 6, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 8,
    byrow = TRUE
  )
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle,
    max_delay = 3,
    n = 5,
    preprocess = NULL
  )

  # This is all backwards bc sum(triangle) is negative, will fail
  expect_true(all(delay_pmf[1:2] > 0))
})

test_that("estimate_delay with negative PMF produces non-increasing CDF", {
  # Use example data with negative values
  triangle_neg <- matrix(
    c(
      100, 60, -20, 10,
      120, 70, -25, 15,
      110, 65, -22, 12,
      130, 75, -28, 18,
      115, 68, -24, 14,
      125, 72, -26, NA,
      105, 62, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 8,
    byrow = TRUE
  )

  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_neg,
    max_delay = 3,
    n = 5,
    preprocess = NULL
  )

  # Compute CDF
  delay_cdf <- cumsum(delay_pmf)

  # CDF differences should include at least one negative value
  cdf_diffs <- diff(delay_cdf)
  expect_true(any(cdf_diffs < 0))
})

test_that("estimate_delay custom preprocessing function works", {
  # Custom preprocessing that doubles all values
  custom_preprocess <- function(triangle) {
    return(triangle * 2)
  }

  triangle <- matrix(
    c(
      10, 5, 3, 2,
      8, 4, 2, 1,
      6, 3, NA, NA,
      4, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Apply custom preprocessing
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle,
    max_delay = 3,
    n = 3,
    preprocess = custom_preprocess
  )

  # Should still return valid PMF
  expect_true(all(delay_pmf >= 0))
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
})
