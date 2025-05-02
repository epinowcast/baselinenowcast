context("Nowcast prediction draw generation")

test_that("function returns vector of correct length", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 16.8,
      NA, NA, 21.2, 19.5,
      NA, 34.5, 15.4, 9.1
    ),
    nrow = 4,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4, 9.1)

  # Run function
  result <- get_nowcast_pred_draw(point_nowcast_pred_matrix, disp)

  # Tests
  expect_no_error(assert_integerish(result))
  expect_length(result, nrow(point_nowcast_pred_matrix))
  expect_true(!anyNA(result))
  expect_true(all(result >= 0)) # Counts should be non-negative
  # The first (max_t - n_horizons + 1) elements should be 0
  expected_zeros <- 4 - 3
  expect_equal(sum(result[1:expected_zeros] == 0), expected_zeros)
  expect_true(any(result[(expected_zeros + 1):length(result)] != 0))
})



test_that("statistical properties are reasonable", {
  # Setup test data with known properties
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, 10,
      NA, NA, 20, 15,
      NA, 30, 25, 20
    ),
    nrow = 3,
    byrow = TRUE
  )
  disp <- c(10, 20) # High dispersion for predictable variance

  # Generate many samples to test distribution properties
  set.seed(456)
  n_samples <- 1000
  samples <- replicate(
    n_samples,
    get_nowcast_pred_draw(point_nowcast_pred_matrix, disp)
  )

  # Test mean is close to expected mean for the non-zero elements
  # For a 3x4 matrix with disp length 2, we expect the last 2 elements to be
  # non-zero and the means should be the rowSums of the last 2 rows
  expected_means <- rowSums(point_nowcast_pred_matrix, na.rm = TRUE)[2:3]
  sample_means <- rowMeans(samples[2:3, ])

  # Means should be close (within 10% for large samples)
  for (i in 1:length(expected_means)) {
    relative_error <- abs(sample_means[i] - expected_means[i]) / expected_means[i] # nolint
    expect_true(relative_error < 0.1)
  }

  # The first element should always be 0
  expect_equal(mean(samples[1, ]), 0)
})

test_that("function handles edge cases correctly", {
  # All NAs in prediction matrix
  all_na_matrix <- matrix(NA, nrow = 3, ncol = 4)
  disp <- c(1, 2)

  result <- get_nowcast_pred_draw(all_na_matrix, disp)
  expect_length(result, 3)
  expect_true(all(result == 0)) # all elements 0

  # Single row matrix
  single_row <- matrix(c(NA, NA, 5, 10), nrow = 1)
  disp_single <- c(1)

  result <- get_nowcast_pred_draw(single_row, disp_single)
  expect_length(result, 1)

  # disp length equals matrix rows
  full_disp <- c(1, 2, 3)
  full_matrix <- matrix(c(
    NA, 20,
    30, 40,
    40, 60
  ), nrow = 3, byrow = TRUE)

  result <- get_nowcast_pred_draw(full_matrix, full_disp)
  expect_length(result, 3)
})

test_that("function correctly uses reversed dispersion parameters", {
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, 100, 100,
      NA, 100, 100, 100,
      100, 100, 100, 100
    ),
    nrow = 3,
    byrow = TRUE
  )

  # Create very different dispersion parameters
  disp <- c(0.01, 1000) # high dispersion at horizon 1,
  # low dispersion at horizon 2

  set.seed(456)
  n_samples <- 1000
  samples <- replicate(
    n_samples,
    get_nowcast_pred_draw(point_nowcast_pred_matrix, disp)
  )

  sample_std <- apply(samples[2:3, ], 1, sd)
  # The standard deviation in horizon 2 should be much lower than the standard
  # deviation at horizon 1, so the raito  of horizon 1 vs 2 should be very low
  expect_true(sample_std[1] / sample_std[2] < 0.1) # horizon 2 / horizon 1
})

test_that("function handles incorrect dimensions appropriately", {
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, 10, 10,
      NA, 20, 20, 20
    ),
    nrow = 2,
    byrow = TRUE
  )

  # Dispersion vector longer than matrix rows
  disp_too_long <- c(1, 2, 3)

  # Should throw an informative error
  expect_error(get_nowcast_pred_draw(point_nowcast_pred_matrix, disp_too_long))

  # Test case: zero-row matrix
  zero_row_matrix <- matrix(nrow = 0, ncol = 4)
  expect_error(get_nowcast_pred_draw(zero_row_matrix, c(1)))
})

test_that("function correctly indexes into mean_pred", {
  # Create a simple test case
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA,
      NA, NA, 7,
      NA, 10, 11
    ),
    nrow = 3,
    byrow = TRUE
  )
  disp <- c(1000, 1000)

  # Calculate expected means
  expected_means_from_rowsums <- c(0, 7, 21)

  # Run function
  set.seed(123)
  result <- get_nowcast_pred_draw(point_nowcast_pred_matrix, disp)

  # Check that the means are very close
  expect_equal(result, expected_means_from_rowsums, tol = 5)
})

test_that("function throws a warning if no NAs in the matrix", {
  point_nowcast_pred_matrix <- matrix(
    c(
      3, 4, 5,
      5, 6, 7,
      8, 9, 0
    ),
    nrow = 3,
    byrow = TRUE
  )
  disp <- c(1, 3)

  expect_warning(get_nowcast_pred_draw(point_nowcast_pred_matrix, disp))
})

test_that("return vector has zeros in the right places", {
  # Create test matrix
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, NA,
      NA, NA, NA, 16.8,
      NA, NA, 21.2, 19.5,
      NA, 34.5, 15.4, 9.1
    ),
    nrow = 5,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4, 9.1)


  # Run function
  result <- get_nowcast_pred_draw(point_nowcast_pred_matrix, disp)

  # Expected pattern: first 2 elements should be 0, last 3 should be 1, 2, 3
  expected <- c(0, 0)

  # Test
  expect_equal(result[1:2], expected)
  expect_false(result[3] == 0)
})
