# Setup shared test data
valid_point_nowcast_matrix <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, 16.8,
    80, 40, 21.2, 19.5,
    70, 34.5, 15.4, 9.1
  ),
  nrow = 5,
  byrow = TRUE
)

valid_reporting_triangle <- as_reporting_triangle(
  data = matrix(
    c(
      80, 50, 25, 10,
      100, 50, 30, NA,
      90, 45, NA, NA,
      80, NA, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  ),
  reference_dates = seq(as.Date("2025-01-01"), by = "day", length.out = 5)
)

valid_max_delay <- 3

test_that(".validate_multiple_inputs accepts valid inputs", {
  expect_no_error(.validate_multiple_inputs(
    valid_point_nowcast_matrix,
    valid_reporting_triangle
  ))
})

test_that(".validate_multiple_inputs accepts matrices with same dimensions", {
  # Test with smaller matrices
  small_point_nowcast <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  small_reporting_triangle <- as_reporting_triangle(
    data = matrix(c(1, 2, 3, NA), nrow = 2, ncol = 2),
    reference_dates = seq(as.Date("2025-01-01"), by = "day", length.out = 2)
  )
  small_max_delay <- 1

  expect_no_error(.validate_multiple_inputs(
    small_point_nowcast,
    small_reporting_triangle
  ))
})

test_that(".validate_multiple_inputs errors if point_nowcast_matrix is not a matrix", { # nolint
  # Test with data frame - this passes ncol check but data frames work
  # No error expected for data.frame as it behaves like matrix for ncol
  df_input <- as.data.frame(valid_point_nowcast_matrix)
  expect_no_error(
    .validate_multiple_inputs(
      df_input,
      valid_reporting_triangle
    )
  )

  # Test with vector - should error on ncol
  vector_input <- as.vector(valid_point_nowcast_matrix)
  expect_error(
    .validate_multiple_inputs(
      vector_input,
      valid_reporting_triangle
    ),
    "argument is of length zero"
  )

  # Test with list - should error on ncol
  list_input <- list(valid_point_nowcast_matrix)
  expect_error(
    .validate_multiple_inputs(
      list_input,
      valid_reporting_triangle
    ),
    "argument is of length zero"
  )
})


test_that(".validate_multiple_inputs errors when matrices have different number of columns", { # nolint
  # Create matrices with different column counts
  point_nowcast_3_cols <- valid_point_nowcast_matrix[, 1:3]

  expect_error(
    .validate_multiple_inputs(
      point_nowcast_3_cols,
      valid_reporting_triangle
    ),
    regexp = "`point_nowcast_matrix` and `reporting_triangle` must have the "
  )

  # Test the other direction
  reporting_triangle_2_cols <- as_reporting_triangle(
    data = unclass(valid_reporting_triangle)[, 1:2],
    reference_dates = get_reference_dates(valid_reporting_triangle)
  )

  expect_error(
    .validate_multiple_inputs(
      valid_point_nowcast_matrix,
      reporting_triangle_2_cols
    ),
    regexp = "`point_nowcast_matrix` and `reporting_triangle` must have the"
  )
})


test_that(".validate_multiple_inputs accepts matrices with different row numbers", { # nolint
  # Test where matrices have different numbers of rows but same columns
  different_rows_point <- valid_point_nowcast_matrix[1:3, ]

  expect_no_error(.validate_multiple_inputs(
    different_rows_point,
    valid_reporting_triangle
  ))
})
test_that(".validate_multiple_inputs returns NULL invisibly on success", {
  result <- .validate_multiple_inputs(
    valid_point_nowcast_matrix,
    valid_reporting_triangle
  )

  expect_null(result)
})
