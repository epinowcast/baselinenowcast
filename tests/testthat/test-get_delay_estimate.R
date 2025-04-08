test_that("get_delay_estimate function works correctly", {
  set.seed(123)
  triangle <- matrix(rpois(30 * 5, lambda = 25), nrow = 30, ncol = 5)

  # Test 1: Basic functionality
  result <- get_delay_estimate(triangle)
  expect_is(result, "numeric")
  expect_identical(as.integer(length(result)), as.integer(ncol(triangle)))
  expect_true(all(result >= 0 & result <= 1))
  expect_equal(sum(result), 1, tolerance = 1e-6)

  # Test 2: Custom max_delay
  result_max_delay <- get_delay_estimate(triangle,
    max_delay = 3
  )
  expect_identical(as.integer(length(result_max_delay)), 4L)

  # Test 3: Custom n_history
  result_n_history <- get_delay_estimate(triangle,
    n = 20
  )
  expect_is(result_n_history, "numeric")

  # Test 4: Input validation *These should be more useful error messages*
  expect_error(get_delay_estimate(triangle, max_delay = 0))
  expect_error(get_delay_estimate(triangle, n = 0))
  expect_error(get_delay_estimate(triangle,
    max_delay = 10,
    n = 40
  ))

  # Test 5: Errors when NAs are in upper part of reportign triangle
  # (These should be 0s)
  triangle_with_na <- triangle
  triangle_with_na[1, 2] <- NA
  expect_error(get_delay_estimate(triangle_with_na))

  # Test 6: Consistency of results
  result1 <- get_delay_estimate(triangle)
  result2 <- get_delay_estimate(triangle)
  expect_identical(result1, result2)

  # Test 7: Check that the function errors if its not passed a triangle
  triangle_single_day <- triangle[1, ]
  expect_error(get_delay_estimate(triangle_single_day))
})

test_that("get_delay_estimate handles partially complete reporting triangles", {
  # Test 8: Test that a partial triangle works correctly
  partial_triangle <- matrix(
    c(
      80, 50, 25, 10,
      100, 50, 30, 20,
      90, 45, 25, 20,
      80, 40, 15, NA,
      70, 30, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  delay_pmf <- get_delay_estimate(
    triangle = partial_triangle,
    max_delay = 3,
    n = 4
  )

  # Test 9: Test that you get the correct delay with a complete triangle
  complete_triangle <- matrix(
    c(
      80, 50, 25, 10,
      100, 50, 30, 20,
      90, 45, 25, 20,
      80, 40, 15, 5,
      70, 30, 10, 10
    ),
    nrow = 5,
    byrow = TRUE
  )
  delay_pmf <- get_delay_estimate(
    triangle = complete_triangle,
    max_delay = 3,
    n = 5
  )
  pmf <- colSums(complete_triangle) / sum(complete_triangle)
  expect_identical(delay_pmf, pmf)

  # Test 10: Test that you get a warning with zeros in the bottom right
  zero_triangle <- matrix(
    c(
      80, 50, 25, 10,
      100, 50, 30, 20,
      90, 45, 25, 0,
      80, 40, 0, 0,
      70, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

  expect_warning(get_delay_estimate(
    triangle = zero_triangle,
    max_delay = 3,
    n = 5
  ))

  # Test 11: zeros not just on bottom right
  zero_triangle2 <- matrix(
    c(
      80, 50, 25, 10,
      100, 0, 30, 20,
      90, 45, 25, 0,
      80, 40, 0, 0,
      70, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

  expect_warning(get_delay_estimate(
    triangle = zero_triangle2,
    max_delay = 3,
    n = 5
  ))

  # Test 12: All 0s in first column produces warning
  triangle <- matrix(
    c(
      0, 1, 1, 1,
      0, 5, 5, 5,
      0, 10, 10, NA,
      0, 20, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_warning(get_delay_estimate(triangle))
})
