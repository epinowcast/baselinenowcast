test_that("apply_delay fills triangle matching known delay distribution", {
  triangle <- matrix(
    c(
      10, 5, 5, 5,
      20, 10, 10, NA,
      40, 20, NA, NA,
      60, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  delay_pmf <- c(0.4, 0.2, 0.2, 0.2)

  triangle_obj <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj,
    delay_pmf = delay_pmf
  )

  expect_equal(result[2, 4], 10, tol = 0.1)
  expect_equal(unname(result[3, 3:4]), c(20, 20), tol = 0.1)

  # now try with 0s
  triangle <- matrix(
    c(
      10, 5, 5, 5,
      20, 10, 10, NA,
      40, 20, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  delay_pmf <- c(0.4, 0.2, 0.2, 0.2)

  triangle_obj <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj,
    delay_pmf = delay_pmf
  )

  expect_equal(result[4, 4], 0.2, tol = 0.1)

  # Now try with a delay pmf with 0s
  triangle <- matrix(
    c(
      10, 5, 5, 5,
      20, 10, 10, NA,
      40, 20, NA, NA,
      1, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  delay_pmf <- c(0, 0.4, 0.4, 0.2)

  triangle_obj <- make_test_triangle(data = triangle)
  expect_error(apply_delay(
    reporting_triangle = triangle_obj,
    delay_pmf = delay_pmf
  ))


  # Try with 0 in the middle of delay distrib
  triangle <- matrix(
    c(
      10, 5, 5, 5,
      20, 10, 10, NA,
      40, 20, NA, NA,
      1, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  delay_pmf <- c(0.2, 0.4, 0, 0.4)

  triangle_obj <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj,
    delay_pmf = delay_pmf
  )
  expect_false(anyNA(result))
  expect_false(any(is.infinite(result)))
})


test_that("apply_delay preserves reporting_triangle structure and attributes", {
  set.seed(123)
  # Make a simple triangle of ones
  triangle <- make_test_triangle(
    data = matrix(nrow = 5, ncol = 4, data = 1),
    construct = TRUE
  )
  delay_pmf <- make_simple_delay_pmf()
  result <- apply_delay(
    reporting_triangle = triangle,
    delay_pmf = delay_pmf
  )

  expect_triangle_output(result, triangle)

  mat <- matrix(nrow = 5, ncol = 4, data = 1)
  expect_error(
    apply_delay(
      reporting_triangle = mat,
      delay_pmf = delay_pmf
    ),
    regexp = "data must have class 'reporting_triangle'"
  )

  # Test that the known values remain unchanged
  expect_identical(result[1:3, 1:2], triangle[1:3, 1:2])
})

test_that("apply_delay function works on a triangle with 0s", {
  set.seed(123)
  # Triangle with 0s in first column to test edge case
  triangle <- matrix(
    c(
      8, 5, 2, 1,
      1, 5, 3, 2,
      0, 4, 2, NA,
      3, 4, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  delay_pmf <- make_simple_delay_pmf()
  triangle_obj <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj,
    delay_pmf = delay_pmf
  )

  expect_triangle_output(result, triangle_obj)

  # Test that the known values remain unchanged
  expect_identical(result[1:3, 1:2], triangle_obj[1:3, 1:2])

  # Test that there are no 0s being propagated to the rest of the row
  expect_false(any(result[, 2:4] == 0))
})

test_that("apply_delay handles larger triangles with multiple missing rows", {
  # Create a sample triangle to nowcast
  triangle_to_nowcast <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, NA,
      80, 40, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Create a sample delay PMF
  delay_pmf <- c(0.5, 0.3, 0.15, 0.05)

  # Run the function
  triangle_obj <- make_test_triangle(data = triangle_to_nowcast)
  result <- apply_delay(triangle_obj, delay_pmf)

  expect_triangle_output(result, triangle_obj)

  # Test that the known values remain unchanged
  expect_identical(result[1:3, 1:2], triangle_obj[1:3, 1:2])

  # Test that NA values are replaced
  expect_false(anyNA(result))

  # Test specific calculations
  # For the last row, second column (delay = 1)
  exp_N <- (70 + 1 - delay_pmf[1]) / delay_pmf[1]
  expect_equal(result[4, 2], exp_N * delay_pmf[2], tolerance = 1e-6)

  # For the third row, third column (delay = 2)
  expected_total <- (sum(triangle_to_nowcast[3, 1:2]) + 1 - sum(delay_pmf[1:2])) / sum(delay_pmf[1:2]) # nolint
  expect_equal(result[3, 3], expected_total * delay_pmf[3], tolerance = 1e-6)

  # Test error handling, specific error message expected
  triangle_obj2 <- make_test_triangle(data = triangle_to_nowcast)
  expect_error(
    apply_delay(
      triangle_obj2,
      c(0.5, 0.5)
    ),
    regexp = "Length of the delay PMF is not the same"
  )
})

test_that("apply_delay function works the same as the more verbose for loop", {
  triangle <- make_test_triangle(
    data = matrix(nrow = 5, ncol = 4, data = 1),
    construct = TRUE
  )
  delay_pmf <- make_simple_delay_pmf()
  result <- apply_delay(
    reporting_triangle = triangle,
    delay_pmf = delay_pmf
  )

  n_delays <- length(delay_pmf)
  n_dates <- nrow(triangle)
  expectation2 <- triangle
  for (co in 2:n_delays) {
    block_bottom_left <- expectation2[(n_dates - co + 2):n_dates, 1:(co - 1),
      drop = FALSE
    ]
    # Uses the observed data to find the expected total on that reference date
    exp_total <- (rowSums(block_bottom_left) + 1 - sum(delay_pmf[1:(co - 1)])) / sum(delay_pmf[1:(co - 1)]) # nolint
    expectation2[(n_dates - co + 2):n_dates, co] <- exp_total * delay_pmf[co]
  }

  expect_identical(result, expectation2)
})

test_that("apply_delay works with ragged reporting triangles", {
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  triangle_data <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle_data <- do.call(rbind, triangle_data)
  triangle <- make_test_triangle(
    data = triangle_data,
    construct = TRUE,
    structure = c(1, 2, 1)
  )

  result <- apply_delay(
    reporting_triangle = triangle,
    delay_pmf = delay_pmf
  )
  cols <- colSums(result[3:5, ])
  pmf <- unname(cols / sum(cols))
  expect_equal(pmf, delay_pmf, tolerance = 0.01)
})

test_that("apply_delay works with structure=2 ragged reporting triangles", {
  delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  triangle_data <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle_data <- do.call(rbind, triangle_data)
  ragged_triangle <- make_test_triangle(
    data = triangle_data,
    construct = TRUE,
    structure = 2
  )

  result <- apply_delay(
    reporting_triangle = ragged_triangle,
    delay_pmf = delay_pmf
  )
  cols <- colSums(result[3:5, ])
  pmf <- unname(cols / sum(cols))
  expect_equal(pmf, delay_pmf, tolerance = 0.01)
})

test_that("apply_delay works with PMF containing negative entries", {
  # Create triangle with negative values
  triangle <- matrix(
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

  # Get PMF with negative entries
  triangle_obj <- make_test_triangle(data = triangle)
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_obj,
    n = 5,
    preprocess = NULL
  )

  # Verify PMF has negative entries
  expect_true(any(delay_pmf < 0))

  # apply_delay should work
  triangle_obj2 <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj2,
    delay_pmf = delay_pmf
  )

  # Result should be a reporting_triangle with same dimensions
  expect_triangle_output(result, triangle_obj2)

  # No NAs should remain
  expect_false(anyNA(result))
})

test_that("apply_delay CDF can be not strictly increasing", {
  # Create triangle with negative values
  triangle <- matrix(
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

  # Get PMF with negative entries
  triangle_obj <- make_test_triangle(data = triangle)
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_obj,
    n = 5,
    preprocess = NULL
  )

  # Compute CDF
  delay_cdf <- cumsum(delay_pmf)

  # Verify CDF is not strictly increasing
  cdf_diffs <- diff(delay_cdf)
  expect_true(any(cdf_diffs < 0))

  # apply_delay should still work
  triangle_obj2 <- make_test_triangle(data = triangle)
  result <- apply_delay(
    reporting_triangle = triangle_obj2,
    delay_pmf = delay_pmf
  )

  expect_false(anyNA(result))
})

test_that("apply_delay completes full workflow with negative PMF", {
  # Create triangle with negative values
  triangle <- matrix(
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

  # Full workflow: estimate delay with preprocess = NULL
  triangle_obj <- make_test_triangle(data = triangle)
  delay_pmf <- estimate_delay(
    reporting_triangle = triangle_obj,
    n = 5,
    preprocess = NULL
  )

  # Apply delay
  triangle_obj2 <- make_test_triangle(data = triangle)
  nowcast <- apply_delay(
    reporting_triangle = triangle_obj2,
    delay_pmf = delay_pmf
  )

  # Verify result properties
  expect_triangle_output(nowcast, triangle_obj2)
  expect_false(anyNA(nowcast))

  # Verify observed values are preserved
  expect_identical(nowcast[1:5, 1:4], triangle_obj[1:5, 1:4])
})

test_that("apply_delay errors with complete matrices without NAs", {
  # Create a complete triangle (no NAs)
  triangle <- matrix(
    c(
      100, 50, 30, 20,
      120, 60, 35, 25,
      110, 55, 32, 22,
      130, 65, 38, 28
    ),
    nrow = 4,
    byrow = TRUE
  )

  delay_pmf <- c(0.5, 0.25, 0.15, 0.10)

  triangle_obj <- make_test_triangle(data = triangle)

  # Should error when there are no NAs to nowcast
  expect_error(
    apply_delay(
      reporting_triangle = triangle_obj,
      delay_pmf = delay_pmf
    ),
    "doesn't contain any missing values"
  )
})
