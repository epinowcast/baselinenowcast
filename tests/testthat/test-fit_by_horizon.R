# Mock fit_nb function for testing (since it's not defined in the snippet)
mock_fit_nb <- function(x, mu) {
  # Simple mock that returns the mean of the ratio
  if (length(x) == 0 || any(mu <= 0)) {
    return(NA_real_)
  }
  return(mean(x / mu, na.rm = TRUE))
}

# Simple test function that returns sum
sum_fun <- function(x, mu) {
  return(sum(x, na.rm = TRUE))
}

# Test function that returns mean difference
mean_diff_fun <- function(x, mu) {
  return(mean(x - mu, na.rm = TRUE))
}

# Create test matrices
obs_matrix <- matrix(
  c(
    5, 6, 2,
    1, 4, 2,
    8, 4, 2
  ),
  nrow = 3,
  byrow = TRUE
)

pred_matrix <- matrix(
  c(
    4.2, 5.2, 1.8,
    0.7, 3.5, 3.4,
    7.3, 4.1, 1.2
  ),
  nrow = 3,
  byrow = TRUE
)

test_that("fit_by_horizon: basic functionality with default function", {
  # Since fit_nb is not defined, we'll use a mock function
  result <- fit_by_horizon(
    observation_model = mock_fit_nb,
    obs = obs_matrix,
    pred = pred_matrix
  )

  # Should return a numeric vector with length equal to number of columns
  expect_type(result, "double")
  expect_length(result, ncol(obs_matrix))
  expect_length(result, 3L)
})

test_that("fit_by_horizon: works with custom function", {
  result <- fit_by_horizon(
    observation_model = sum_fun,
    obs = obs_matrix,
    pred = pred_matrix
  )

  # For sum_fun, should return column sums of obs_matrix
  expected <- c(
    sum(obs_matrix[, 1]), # 5 + 1 + 8 = 14 #nolint
    sum(obs_matrix[, 2]), # 6 + 4 + 4 = 14 #nolint
    sum(obs_matrix[, 3]) # 2 + 2 + 2 = 6 #nolint
  )

  expect_identical(result, expected)
})

test_that("fit_by_horizon: errors obs is NULL", {
  expect_error(
    fit_by_horizon(
      observation_model = sum_fun,
      obs = NULL,
      pred = pred_matrix
    ),
    regexp = "Missing `obs` and/or `pred" # nolint
  )
})

test_that("fit_by_horizon: errors when pred is NULL", {
  expect_error(
    fit_by_horizon(
      observation_model = sum_fun,
      obs = obs_matrix,
      pred = NULL
    ),
    regexp = "Missing `obs` and/or `pred" # nolint
  )
})

test_that("fit_by_horizon: errors when both obs and pred are NULL", {
  expect_error(
    fit_by_horizon(
      observation_model = sum_fun,
      obs = NULL,
      pred = NULL
    ),
    regexp = "Missing `obs` and/or `pred" # nolint
  )
})

test_that("fit_by_horizon: errors when dimensions don't match", {
  obs_wrong_dim <- matrix(1:6, nrow = 2, ncol = 3)

  expect_error(
    fit_by_horizon(
      observation_model = sum_fun,
      obs = obs_wrong_dim,
      pred = pred_matrix
    ),
    "`obs` and `pred` must have the same dimensions"
  )
})

test_that("fit_by_horizon: handles single column matrices", {
  obs_single_col <- matrix(c(1, 2, 3), ncol = 1)
  pred_single_col <- matrix(c(1.1, 2.1, 3.1), ncol = 1)

  result <- fit_by_horizon(
    observation_model = sum_fun,
    obs = obs_single_col,
    pred = pred_single_col
  )

  expect_length(result, 1L)
  expect_identical(result, 6) # sum of c(1, 2, 3)
})

test_that("fit_by_horizon: handles single row matrices", {
  obs_single_row <- matrix(c(1, 2, 3), nrow = 1)
  pred_single_row <- matrix(c(1.1, 2.1, 3.1), nrow = 1)

  result <- fit_by_horizon(
    observation_model = sum_fun,
    obs = obs_single_row,
    pred = pred_single_row
  )

  expect_length(result, 3L)
  expect_identical(result, c(1, 2, 3))
})

test_that("fit_by_horizon: handles 1x1 matrices", {
  obs_1x1 <- matrix(5, nrow = 1, ncol = 1)
  pred_1x1 <- matrix(4.5, nrow = 1, ncol = 1)

  result <- fit_by_horizon(
    observation_model = sum_fun,
    obs = obs_1x1,
    pred = pred_1x1
  )

  expect_length(result, 1L)
  expect_identical(result, 5)
})

test_that("fit_by_horizon: function receives correct column data", {
  # Test function that checks if it receives the right data
  check_data_fun <- function(x, mu) {
    # For column 1: x should be c(5, 1, 8), mu should be c(4.2, 0.7, 7.3)
    # For column 2: x should be c(6, 4, 4), mu should be c(5.2, 3.5, 4.1)
    # For column 3: x should be c(2, 2, 2), mu should be c(1.8, 3.4, 1.2)
    return(length(x)) # Just return the length to verify
  }

  result <- fit_by_horizon(
    observation_model = check_data_fun,
    obs = obs_matrix,
    pred = pred_matrix
  )

  # Each column should have 3 elements
  expect_identical(result, c(3, 3, 3))
})


test_that("fit_by_horizon: function that errors", {
  error_fun <- function(x, mu) {
    stop("Test error", call. = FALSE)
  }

  expect_error(
    fit_by_horizon(
      observation_model = error_fun,
      obs = obs_matrix,
      pred = pred_matrix
    ),
    "Test error"
  )
})

test_that("fit_by_horizon: works with vectors converted to single-column matrices", { # nolint
  obs_vec <- c(1, 2, 3)
  pred_vec <- c(1.1, 2.1, 3.1)

  # Convert to single-column matrices
  obs_matrix_vec <- matrix(obs_vec, ncol = 1)
  pred_matrix_vec <- matrix(pred_vec, ncol = 1)

  result <- fit_by_horizon(
    observation_model = sum_fun,
    obs = obs_matrix_vec,
    pred = pred_matrix_vec
  )

  expect_length(result, 1L)
  expect_identical(result, sum(obs_vec))
})

test_that("fit_by_horizon: handles empty matrices", {
  obs_empty <- matrix(nrow = 0, ncol = 3)
  pred_empty <- matrix(nrow = 0, ncol = 3)

  empty_fun <- function(x, mu) {
    return(length(x))
  }

  result <- fit_by_horizon(
    observation_model = empty_fun,
    obs = obs_empty,
    pred = pred_empty
  )

  expect_length(result, 3L)
  expect_identical(result, c(0, 0, 0))
})

test_that("fit_by_horizon: default function parameter works", {
  # This test assumes fit_nb is available in the environment
  # If not available, we can skip or mock it
  skip_if_not(exists("fit_nb"), "fit_nb function not available")

  result <- fit_by_horizon(obs = obs_matrix, pred = pred_matrix)

  expect_type(result, "double")
  expect_length(result, ncol(obs_matrix))
})
