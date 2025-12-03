# Create a triangle with known delay PMF
sim_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)

# Generate counts for each reference date
counts <- c(100, 150, 200, 250, 300)

# Create a complete triangle based on the known delay PMF
complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

# Create a reporting triangle with NAs in the lower right
reporting_triangle <- make_test_triangle(data = complete_triangle) |>
  apply_reporting_structure()

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
  expect_equal(as.numeric(result), as.numeric(sim_delay_pmf), tolerance = 1e-6)
})

test_that("estimate_delay works with truncated triangles", {
  # Test with full triangle (max_delay = 3)
  result_full <- estimate_delay(reporting_triangle)
  expect_identical(as.integer(length(result_full)), 4L)
  expect_equal(
    as.numeric(result_full), as.numeric(sim_delay_pmf), tolerance = 1e-6
  )

  # Test with truncated triangle (max_delay = 2)
  truncated_triangle <- truncate_to_delay(reporting_triangle, max_delay = 2)
  result_truncated <- estimate_delay(truncated_triangle)
  expect_identical(as.integer(length(result_truncated)), 3L)

  expected_truncated <- sim_delay_pmf[1:3] / sum(sim_delay_pmf[1:3])
  expect_equal(
    as.numeric(result_truncated), as.numeric(expected_truncated),
    tolerance = 1e-6
  )
})

test_that("estimate_delay handles custom n_history parameter", {
  # Test with different n values
  result_full <- estimate_delay(reporting_triangle, n = 5)
  result_partial <- estimate_delay(reporting_triangle, n = 4)

  # Both should return the correct PMF
  expect_equal(
    as.numeric(result_full), as.numeric(sim_delay_pmf), tolerance = 1e-6
  )
  expect_equal(
    as.numeric(result_partial), as.numeric(sim_delay_pmf), tolerance = 1e-6
  )
})

test_that("estimate_delay validates input parameters correctly", {
  # Test invalid n
  expect_error(estimate_delay(reporting_triangle, n = 0))

  # Test n > nrow(reporting_triangle)
  expect_error(estimate_delay(reporting_triangle, n = 10))
})

test_that(
  "estimate_delay errors when NAs are in upper part of reporting triangle",
  {
    # Create a matrix with NA in the upper part where it shouldn't be
    # Do this directly as a matrix to bypass reporting_triangle validation
    mat_data <- unclass(reporting_triangle)
    mat_data[1, 2] <- NA

    # Create reporting_triangle with invalid structure by temporarily
    # using internal structure
    triangle_with_na <- structure(
      mat_data,
      class = c("reporting_triangle", "matrix"),
      reference_dates = get_reference_dates(reporting_triangle),
      max_delay = get_max_delay(reporting_triangle),
      delays_unit = attr(reporting_triangle, "delays_unit"),
      structure = get_reporting_structure(reporting_triangle),
      mean_delay = get_mean_delay(reporting_triangle)
    )

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
  full_triangle <- lapply(
    complete_counts, function(x) round(x * complete_pmf)
  )
  full_triangle <- make_test_triangle(data = do.call(rbind, full_triangle))

  delay_pmf <- estimate_delay(
    reporting_triangle = full_triangle,
    n = 5
  )

  expect_equal(
    as.numeric(delay_pmf), as.numeric(complete_pmf), tolerance = 0.001
  )
})

test_that(
  "estimate_delay works with every other day reporting of daily data",
  {
    sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

    counts <- c(30, 40, 50, 60, 70)

    complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
    complete_triangle <- do.call(rbind, complete_triangle)

    reporting_triangle <- make_test_triangle(
      data = complete_triangle, construct = TRUE, structure = 2
    )

    # Get delay estimate
    delay_pmf <- estimate_delay(
      reporting_triangle = reporting_triangle
    )
    # Test that the function returns the expected PMF
    expect_equal(
      as.numeric(delay_pmf), as.numeric(sim_delay_pmf), tolerance = 1e-6
    )
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
  partial_triangle <- make_test_triangle(
    data = partial_complete, construct = TRUE, structure = 1
  )

  delay_pmf <- estimate_delay(
    reporting_triangle = partial_triangle,
    n = 4
  )

  expect_is(delay_pmf, "numeric")
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
  expect_equal(
    as.numeric(delay_pmf), as.numeric(partial_pmf), tolerance = 1e-6
  )
})

test_that(
  "estimate_delay with preprocess = preprocess_negative_values handles negatives", # nolint
  {
    # Use example data with negative values
    triangle_neg <- make_test_triangle(data = matrix(
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
    ))

    # Default behaviour should handle negatives
    delay_pmf <- estimate_delay(
      reporting_triangle = triangle_neg,
      n = 5
    )

    # Should return valid PMF without negative entries
    expect_true(all(delay_pmf >= 0))
    expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
  }
)

test_that("estimate_delay with preprocess = NULL preserves negative values", {
  # Use example data with negative values
  triangle_neg <- make_test_triangle(data = matrix(
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
  ))

  # With preprocess = NULL, negatives should be preserved
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_neg,
    n = 5,
    preprocess = NULL
  )

  # Should return a PMF that sums to 1
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)

  # PMF can have negative entries
  expect_true(any(delay_pmf < 0))
})

test_that("estimate_delay with negative PMF produces non-increasing CDF", {
  # Use example data with negative values
  triangle_neg <- make_test_triangle(data = matrix(
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
  ))

  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_neg,
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
  custom_preprocess <- function(triangle, validate = TRUE) {
    return(triangle * 2)
  }

  triangle <- make_test_triangle(data = matrix(
    c(
      10, 5, 3, 2,
      8, 4, 2, 1,
      6, 3, NA, NA,
      4, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  ))

  # Apply custom preprocessing
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle,
    n = 3,
    preprocess = custom_preprocess
  )

  # Should still return valid PMF
  expect_true(all(delay_pmf >= 0))
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
})
