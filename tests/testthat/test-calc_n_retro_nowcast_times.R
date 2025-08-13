# Helper function: simple identity aggregator (no aggregation)
identity_aggregator <- function(mat) {
  return(mat)
}

# Helper function: aggregator that removes first row
remove_first_row_aggregator <- function(mat) {
  if (nrow(mat) <= 1) {
    return(matrix(nrow = 0, ncol = ncol(mat)))
  }
  return(mat[-1, , drop = FALSE])
}

# Helper function: aggregator that removes first two rows
remove_two_rows_aggregator <- function(mat) {
  if (nrow(mat) <= 2) {
    return(matrix(nrow = 0, ncol = ncol(mat)))
  }
  return(mat[-(1:2), , drop = FALSE])
}

# Create test matrices of different sizes
mat_5x4 <- matrix(1:20, nrow = 5, ncol = 4)
mat_4x4 <- matrix(1:16, nrow = 4, ncol = 4)
mat_3x4 <- matrix(1:12, nrow = 3, ncol = 4)
mat_2x4 <- matrix(1:8, nrow = 2, ncol = 4)
mat_1x4 <- matrix(1:4, nrow = 1, ncol = 4)

test_that(".calc_n_retro_nowcast_times: basic functionality with identity aggregator", { # nolint
  list_of_obs <- list(mat_5x4, mat_4x4, mat_3x4)
  n_possible_horizons <- 3

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    identity_aggregator
  )

  # With identity aggregator, rows_to_lose = 0
  # n_rows_required = 3 + 0 = 3
  # All matrices have >= 3 rows, so all should be included
  expect_identical(result, 3L)
})

test_that(".calc_n_retro_nowcast_times: returns only the 2 larger matrices", {
  list_of_obs <- list(mat_5x4, mat_4x4, mat_3x4, mat_2x4, mat_1x4)
  n_possible_horizons <- 4

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    identity_aggregator
  )

  # n_rows_required = 4 + 0 = 4
  # Only mat_5x4 (5 rows) and mat_4x4 (4 rows) have >= 4 rows
  expect_identical(result, 2L)
})

test_that(".calc_n_retro_nowcast_times: works with aggregator that removes rows", { # nolint
  list_of_obs <- list(mat_5x4, mat_4x4, mat_3x4)
  n_possible_horizons <- 2

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    remove_first_row_aggregator
  )

  # remove_first_row_aggregator: 5x4 -> 4x4, so rows_to_lose = 1
  # n_rows_required = 2 + 1 = 3
  # All matrices have >= 3 rows, so all should be included
  expect_identical(result, 3L)

  # If add in 2x4, the result is the same
  list_of_obs <- list(mat_5x4, mat_4x4, mat_3x4, mat_2x4)
  n_possible_horizons <- 2

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    remove_first_row_aggregator
  )
  expect_identical(result, 3L)
})
test_that(".calc_n_retro_nowcast_times: handles edge case with no valid matrices", { # nolint
  list_of_obs <- list(mat_2x4, mat_1x4)
  n_possible_horizons <- 5

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    identity_aggregator
  )

  # n_rows_required = 5 + 0 = 5
  # Neither matrix has >= 5 rows
  expect_identical(result, 0L)
})
test_that(".calc_n_retro_nowcast_times: handles single matrix input", { # nolint
  list_of_obs <- list(mat_5x4)
  n_possible_horizons <- 3

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    identity_aggregator
  )

  # Single matrix with 5 rows, needs 3 rows
  expect_identical(result, 1L)
})

test_that(".calc_n_retro_nowcast_times: handles single matrix that's too small", { # nolint
  list_of_obs <- list(mat_2x4)
  n_possible_horizons <- 5

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    identity_aggregator
  )

  # Single matrix with 2 rows, needs 5 rows
  expect_identical(result, 0L)
})
test_that(".calc_n_retro_nowcast_times: mixed matrix sizes with moderate filtering", { # nolint
  # Create a more realistic scenario
  list_of_obs <- list(
    matrix(1:28, nrow = 7, ncol = 4), # 7 rows
    matrix(1:24, nrow = 6, ncol = 4), # 6 rows
    matrix(1:20, nrow = 5, ncol = 4), # 5 rows
    matrix(1:16, nrow = 4, ncol = 4), # 4 rows
    matrix(1:12, nrow = 3, ncol = 4) # 3 rows
  )
  n_possible_horizons <- 3

  result <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    remove_first_row_aggregator
  )

  # remove_first_row_aggregator: 7x4 -> 6x4, so rows_to_lose = 1
  # n_rows_required = 3 + 1 = 4
  # Matrices with >= 4 rows: 7x4, 6x4, 5x4, 4x4 (4 matrices)
  expect_identical(result, 4L)
})
