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

valid_reporting_triangle <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, NA,
    90, 45, NA, NA,
    80, NA, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)

test_that(".validate_multiple_inputs accepts valid inputs", {
  expect_no_error(.validate_multiple_inputs(
    valid_point_nowcast_matrix,
    valid_reporting_triangle
  ))
})

test_that(".validate_multiple_inputs accepts matrices with same dimensions", {
  # Test with smaller matrices
  small_point_nowcast <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  small_reporting_triangle <- matrix(c(1, 2, 3, NA), nrow = 2, ncol = 2)

  expect_no_error(.validate_multiple_inputs(
    small_point_nowcast,
    small_reporting_triangle
  ))
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
  reporting_triangle_2_cols <- valid_reporting_triangle[, 1:2]

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
