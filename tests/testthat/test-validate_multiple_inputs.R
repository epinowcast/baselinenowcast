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

valid_max_delay <- 3

test_that(".validate_multiple_inputs accepts valid inputs", {
  expect_no_error(.validate_multiple_inputs(
    valid_point_nowcast_matrix,
    valid_reporting_triangle,
    valid_max_delay
  ))
})

test_that(".validate_multiple_inputs accepts matrices with same dimensions", {
  # Test with smaller matrices
  small_point_nowcast <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  small_reporting_triangle <- matrix(c(1, 2, 3, NA), nrow = 2, ncol = 2)
  small_max_delay <- 1

  expect_no_error(.validate_multiple_inputs(
    small_point_nowcast,
    small_reporting_triangle,
    small_max_delay
  ))
})

test_that(".validate_multiple_inputs errors if point_nowcast_matrix is not a matrix", { # nolint
  # Test with data frame
  df_input <- as.data.frame(valid_point_nowcast_matrix)
  expect_error(
    .validate_multiple_inputs(
      df_input,
      valid_reporting_triangle,
      valid_max_delay
    ),
    "`point_nowcast_matrix` must be a matrix."
  )

  # Test with vector
  vector_input <- as.vector(valid_point_nowcast_matrix)
  expect_error(
    .validate_multiple_inputs(
      vector_input,
      valid_reporting_triangle,
      valid_max_delay
    ),
    "`point_nowcast_matrix` must be a matrix."
  )

  # Test with list
  list_input <- list(valid_point_nowcast_matrix)
  expect_error(
    .validate_multiple_inputs(
      list_input,
      valid_reporting_triangle,
      valid_max_delay
    ),
    "`point_nowcast_matrix` must be a matrix."
  )
})

test_that(".validate_multiple_inputs errors when reporting_triangle columns don't match max_delay + 1", { # nolint
  # Test with max_delay too small
  expect_error(
    .validate_multiple_inputs(
      valid_point_nowcast_matrix,
      valid_reporting_triangle,
      max_delay = 2
    ),
    regexp = "Inconsistent `max_delay`.*ncol\\(reporting_triangle\\).*= 4.*max_delay \\+ 1.*= 3" # nolint
  )

  # Test with max_delay too large
  expect_error(
    .validate_multiple_inputs(
      valid_point_nowcast_matrix,
      valid_reporting_triangle,
      max_delay = 5
    ),
    regexp = "Inconsistent `max_delay`.*ncol\\(reporting_triangle\\).*= 4.*max_delay \\+ 1.*= 6" # nolint
  )
})

test_that(".validate_multiple_inputs errors when point_nowcast_matrix columns don't match max_delay + 1", { # nolint
  # Create a reporting triangle with different number of columns
  different_reporting_triangle <- valid_reporting_triangle[, 1:3]

  expect_error(
    .validate_multiple_inputs(
      valid_point_nowcast_matrix,
      different_reporting_triangle,
      max_delay = 2
    )
  )
})

test_that(".validate_multiple_inputs errors when matrices have different number of columns", { # nolint
  # Create matrices with different column counts
  point_nowcast_3_cols <- valid_point_nowcast_matrix[, 1:3]

  expect_error(
    .validate_multiple_inputs(
      point_nowcast_3_cols,
      valid_reporting_triangle,
      valid_max_delay
    ),
    regexp = "`point_nowcast_matrix` and `reporting_triangle` must have the "
  )

  # Test the other direction
  reporting_triangle_2_cols <- valid_reporting_triangle[, 1:2]

  expect_error(
    .validate_multiple_inputs(
      valid_point_nowcast_matrix,
      reporting_triangle_2_cols,
      max_delay = 1
    ),
    regexp = "`point_nowcast_matrix` and `reporting_triangle` must have the"
  )
})


test_that(".validate_multiple_inputs accepts matrices with different row numbers", { # nolint
  # Test where matrices have different numbers of rows but same columns
  different_rows_point <- valid_point_nowcast_matrix[1:3, ]

  expect_no_error(.validate_multiple_inputs(
    different_rows_point,
    valid_reporting_triangle,
    valid_max_delay
  ))
})
test_that(".validate_multiple_inputs returns NULL invisibly on success", {
  result <- .validate_multiple_inputs(
    valid_point_nowcast_matrix,
    valid_reporting_triangle,
    valid_max_delay
  )

  expect_null(result)
})
