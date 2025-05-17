test_that("get_nowcast_pred_draw: function returns vector of correct length", {
  # Setup test data
  point_nowcast_matrix <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, 16.8,
      80, 40, 21.2, 19.5,
      70, 34.5, 15.4, 9.1
    ),
    nrow = 4,
    byrow = TRUE
  )

  dispersion <- c(0.8, 12.4, 9.1)
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Run function
  result <- get_nowcast_pred_draw(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  # Tests
  expect_no_error(assert_integerish(result))
  expect_length(result, nrow(point_nowcast_matrix))
  expect_false(anyNA(result))
  expect_true(all(result >= 0)) # Counts should be non-negative
  # The first (max_t - n_horizons + 1) elements should be 0
  expected_zeros <- 4 - 3
  expect_identical(sum(result[1:expected_zeros] == 0), as.integer(expected_zeros)) # nolint
  expect_true(any(result[(expected_zeros + 1):length(result)] != 0))
})

test_that("get_nowcast_pred_draw: statistical properties are reasonable", {
  # Setup test data with known properties
  point_nowcast_matrix <- matrix(
    c(
      1, 3, 5, 10,
      5, 5, 20, 15,
      1, 30, 25, 20
    ),
    nrow = 3,
    byrow = TRUE
  )
  dispersion <- c(10, 20) # High dispersion for predictable variance
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Generate many samples to test distribution properties
  set.seed(456)
  n_samples <- 1000
  samples <- replicate(
    n_samples,
    get_nowcast_pred_draw(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion
    )
  )

  # Test mean is close to expected mean for the non-zero elements
  # For a 3x4 matrix with disp length 2, we expect the last 2 elements to be
  # non-zero and the means should be the rowSums of the last 2 rows
  expected_means <- rowSums(
    .extract_predictions(point_nowcast_matrix, reporting_triangle),
    na.rm = TRUE
  )[2:3]
  sample_means <- rowMeans(samples[2:3, ])

  # Means should be close (within 10% for large samples)
  for (i in seq_along(expected_means)) {
    relative_error <- abs(sample_means[i] - expected_means[i]) / expected_means[i] # nolint
    expect_lt(relative_error, 0.1)
  }

  # The first element should always be 0
  expect_identical(mean(samples[1, ]), 0)
})

test_that("get_nowcast_pred_draw: function handles edge cases correctly", {
  # All NAs in prediction matrix
  all_na_matrix <- matrix(NA, nrow = 3, ncol = 4)
  dispersion <- c(1, 2)
  reporting_triangle <- matrix(0, nrow = 3, ncol = 4)
  reporting_triangle[lower.tri(reporting_triangle, diag = TRUE)] <- NA

  result <- expect_error(
    get_nowcast_pred_draw(all_na_matrix, reporting_triangle, dispersion)
  )

  # Single row matrix
  single_row <- matrix(c(1, 1, 5, 10), nrow = 1)
  single_triangle <- generate_triangle(single_row, structure = 2)
  dispersion_single <- 1


  result <- get_nowcast_pred_draw(
    single_row,
    single_triangle,
    dispersion_single
  )
  expect_length(result, 1)
})

test_that(
  "get_nowcast_pred_draw: function correctly uses reversed dispersion parameters", { # nolint
  point_nowcast_matrix <- matrix(
    c(
      1, 3, 5, 10,
      5, 5, 20, 15,
      1, 30, 25, 20
    ),
    nrow = 3,
    byrow = TRUE
  )
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Create very different dispersion parameters
  dispersion <- c(0.01, 1000) # high dispersion at horizon 1,
  # low dispersion at horizon 2

  set.seed(456)
  n_samples <- 1000
  samples <- replicate(
    n_samples,
    get_nowcast_pred_draw(
      point_nowcast_matrix, reporting_triangle, dispersion
    )
  )

  sample_std <- apply(samples[2:3, ], 1, sd)
  # The standard deviation in horizon 2 should be much lower than the standard
  # deviation at horizon 1, so the raito  of horizon 1 vs 2 should be very low
  expect_lt(sample_std[1] / sample_std[2], 0.1) # horizon 2 / horizon 1
})

test_that("function handles incorrect dimensions appropriately", {
  point_nowcast_matrix <- matrix(
    c(
      1, 2, 10, 10,
      3, 20, 20, 20
    ),
    nrow = 2,
    byrow = TRUE
  )
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Dispersion vector longer than matrix rows
  dispersion_too_long <- c(1, 2, 3)

  # Should throw an informative error
  expect_error(
    get_nowcast_pred_draw(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion_too_long
    )
  )

  # Test case: zero-row matrix
  zero_row_matrix <- matrix(nrow = 0, ncol = 4)
  zero_reporting_triangle <- matrix(nrow = 0, ncol = 4)
  expect_error(
    get_nowcast_pred_draw(
      zero_row_matrix,
      zero_reporting_triangle,
      1
    )
  )
})

test_that("get_nowcast_pred_draw: function correctly indexes into mean_pred", {
  # Create a simple test case
  point_nowcast_matrix <- matrix(
    c(
      1, 3, 4,
      4, 6, 7,
      4, 10, 11
    ),
    nrow = 3,
    byrow = TRUE
  )
  dispersion <- c(1000, 1000)
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Calculate expected means
  expected_means_from_rowsums <- c(0, 7, 21)

  # Run function
  set.seed(123)
  result <- get_nowcast_pred_draw(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  # Check that the means are very close
  expect_equal(result, expected_means_from_rowsums, tol = 5)
})

test_that(
  "get_nowcast_pred_draw: return vector has zeros in the right places", {
  # Create test matrix
  point_nowcast_matrix <- matrix(
    c(
      10, 100, 100, 100,
      100, 50, 30, 20,
      90, 45, 25, 16.8,
      80, 40, 21.2, 19.5,
      70, 34.5, 15.4, 9.1
    ),
    nrow = 5,
    byrow = TRUE
  )
  dispersion <- c(0.8, 12.4, 9.1)
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  # Run function
  result <- get_nowcast_pred_draw(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  # Expected pattern: first 2 elements should be 0, last 3 should be 1, 2, 3
  expected <- c(0, 0)

  # Test
  expect_identical(result[1:2], expected)
  expect_false(result[3] == 0)
})
