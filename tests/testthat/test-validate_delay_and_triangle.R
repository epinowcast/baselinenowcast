# Shared inputs for tests
valid_triangle <- matrix(1:12, nrow = 3, ncol = 4)
valid_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)

test_that(".validate_delay_and_triangle valid inputs pass validation", {
  expect_no_error(.validate_delay_and_triangle(valid_triangle, valid_delay_pmf))
})

test_that(".validate_delay_and_triangle accepts non-integer values", {
  non_integer_triangle <- matrix(c(1, 2.5, 3, 4), nrow = 2, ncol = 2)
  expect_no_error(
    .validate_delay_and_triangle(non_integer_triangle, c(0.6, 0.4))
  )
})

test_that(".validate_delay_and_triangle non-matrix triangle causes error", {
  not_matrix_triangle <- c(1, 2, 3, 4)
  expect_error(
    .validate_delay_and_triangle(not_matrix_triangle, c(0.6, 0.4)),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  )
})

test_that(".validate_delay_and_triangle non-numeric delay PMF causes error", {
  non_numeric_delay <- c("0.5", "0.5")
  expect_error(
    .validate_delay_and_triangle(valid_triangle, non_numeric_delay),
    "Assertion on 'delay_pmf' failed: Must inherit from class 'numeric'"
  )
})

test_that(
  ".validate_delay_and_triangle mismatched inputs cause error",
  {
    mismatched_delay <- c(0.3, 0.3, 0.4)
    expect_error(
      .validate_delay_and_triangle(valid_triangle, mismatched_delay),
      "Length of the delay PMF is not the same as the number of delays"
    )
  }
)

test_that(".validate_delay_and_triangle empty triangle causes error", {
  empty_triangle <- matrix(integer(0), nrow = 0, ncol = 0)
  expect_error(
    .validate_delay_and_triangle(empty_triangle, valid_delay_pmf),
    "Assertion on 'triangle' failed: Contains only missing values."
  )
})

test_that(".validate_delay_and_triangle empty delay PMF causes error", {
  empty_delay <- numeric(0)
  expect_error(
    .validate_delay_and_triangle(valid_triangle, empty_delay),
    "Length of the delay PMF is not the same as the number of delays"
  )
})

test_that(
  ".validate_delay_and_triangle delay_pmf[1] = 0 with triangle causes error",
  {
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
    delay_pmf <- c(0, 0.2, 0.4, 0.2)
    expect_error(.validate_delay_and_triangle(triangle, delay_pmf))
  }
)
