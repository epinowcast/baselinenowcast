# Create a triangle with known delay PMF
sim_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)

# Generate counts for each reference date
counts <- c(100, 150, 200, 250, 300)

# Create a complete triangle based on the known delay PMF
complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

# Create a reporting triangle with NAs in the lower right
reporting_triangle <- replace_lower_right_with_NA(complete_triangle)

test_that("get_delay_estimate returns a valid probability mass function", {
  # Get delay estimate
  result <- get_delay_estimate(reporting_triangle)

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

test_that("get_delay_estimate handles custom max_delay parameter", {
  # Test with max_delay = 3 (full delay)
  result_full <- get_delay_estimate(reporting_triangle, max_delay = 3)
  expect_identical(as.integer(length(result_full)), 4L)
  expect_equal(result_full, sim_delay_pmf, tolerance = 1e-6)

  # Test with max_delay = 2 (truncated delay)
  result_truncated <- get_delay_estimate(reporting_triangle, max_delay = 2)
  expect_identical(as.integer(length(result_truncated)), 3L)

  expected_truncated <- sim_delay_pmf[1:3] / sum(sim_delay_pmf[1:3])
  expect_equal(result_truncated, expected_truncated, tolerance = 1e-6)
})

test_that("get_delay_estimate handles custom n_history parameter", {
  # Test with different n values
  result_full <- get_delay_estimate(reporting_triangle, n = 5)
  result_partial <- get_delay_estimate(reporting_triangle, n = 4)

  # Both should return the correct PMF
  expect_equal(result_full, sim_delay_pmf, tolerance = 1e-6)
  expect_equal(result_partial, sim_delay_pmf, tolerance = 1e-6)
})

test_that("get_delay_estimate validates input parameters correctly", {
  # Test invalid max_delay
  expect_error(get_delay_estimate(reporting_triangle, max_delay = 0))

  # Test invalid n
  expect_error(get_delay_estimate(reporting_triangle, n = 0))

  # Test n > nrow(reporting_triangle)
  expect_error(get_delay_estimate(reporting_triangle, n = 10))

  # Test max_delay >= ncol(reporting_triangle)
  expect_error(get_delay_estimate(reporting_triangle, max_delay = 5))
})

test_that(
  "get_delay_estimate errors when NAs are in upper part of reporting triangle",
 {
  # Add NA in the upper part where it shouldn't be
  triangle_with_na <- reporting_triangle
  triangle_with_na[1, 2] <- NA

  expect_error(get_delay_estimate(triangle_with_na))
})

test_that("get_delay_estimate errors if not passed a matrix", {
  # Create a vector instead of a matrix
  triangle_single_day <- reporting_triangle[1, ]
  expect_error(get_delay_estimate(triangle_single_day))
})

test_that("get_delay_estimate handles partially complete reporting triangles", {
  # Create a triangle with known delay PMF
  partial_pmf <- c(0.45, 0.25, 0.2, 0.1)

  # Generate counts for each reference date
  partial_counts <- c(80, 100, 90, 80, 70)

  # Create a complete triangle based on the known delay PMF
  partial_complete <- lapply(partial_counts, function(x) x * partial_pmf)
  partial_complete <- do.call(rbind, partial_complete)

  # Create a reporting triangle with NAs in the lower right
  partial_triangle <- replace_lower_right_with_NA(partial_complete)

  delay_pmf <- get_delay_estimate(
    reporting_triangle = partial_triangle,
    max_delay = 3,
    n = 4
  )

  expect_is(delay_pmf, "numeric")
  expect_equal(sum(delay_pmf), 1, tolerance = 1e-6)
  expect_equal(delay_pmf, partial_pmf, tolerance = 1e-6)
})

test_that("get_delay_estimate calculates correct PMF with complete triangle", {
  # Create a triangle with known delay PMF
  complete_pmf <- c(0.45, 0.25, 0.2, 0.1)

  # Generate counts for each reference date
  complete_counts <- c(80, 100, 90, 80, 70)

  # Create a complete triangle based on the known delay PMF
  full_triangle <- lapply(complete_counts, function(x) round(x * complete_pmf))
  full_triangle <- do.call(rbind, full_triangle)

  delay_pmf <- get_delay_estimate(
    reporting_triangle = full_triangle,
    max_delay = 3,
    n = 5
  )

  expect_equal(delay_pmf, complete_pmf, tolerance = 0.001)
})

test_that("get_delay_estimate works with every other day reporting of daily data", {
  sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

  counts <- c(30, 40, 50, 60, 70)

  complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
  complete_triangle <- do.call(rbind, complete_triangle)

  reporting_triangle <- replace_lower_right_with_NA(
    complete_triangle, structure = 2
  )

  # Get delay estimate
  delay_pmf <- get_delay_estimate(
    reporting_triangle = reporting_triangle
  )
  # Test that the function returns the expected PMF
  expect_identical(delay_pmf, sim_delay_pmf)
})
