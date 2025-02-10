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
  result_max_delay <- get_delay_estimate(triangle, max_delay = 3)
  expect_identical(as.integer(length(result_max_delay)), 4L)

  # Test 3: Custom n_history
  result_n_history <- get_delay_estimate(triangle, n_history = 20)
  expect_is(result_n_history, "numeric")

  # Test 4: Input validation *These should be more useful error messages*
  expect_error(get_delay_estimate(triangle, max_delay = 0))
  expect_error(get_delay_estimate(triangle, n_history = 0))
  expect_error(get_delay_estimate(triangle, max_delay = 10, n_history = 40))

  # Test 5: Handling of missing values
  triangle_with_na <- triangle
  triangle_with_na[1, 2] <- NA
  result_with_na <- get_delay_estimate(triangle_with_na)
  expect_is(result_with_na, "numeric")

  # Test 6: Consistency of results
  result1 <- get_delay_estimate(triangle)
  result2 <- get_delay_estimate(triangle)
  expect_identical(result1, result2)

  # Test 7: Check that the function errors if its not passed a triangle
  triangle_single_day <- triangle[1, ]
  expect_error(get_delay_estimate(triangle_single_day))
})
