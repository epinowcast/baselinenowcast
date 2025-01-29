test_that("estimate_delay function works correctly", {
  set.seed(123)
  triangle <- data.table::data.table(
    reference_date = as.Date("2023-01-01") + 0:29,
    `0` = rpois(30, 100),
    `1` = rpois(30, 50),
    `2` = rpois(30, 25),
    `3` = rpois(30, 10),
    `4` = rpois(30, 5)
  )

  # Test 1: Basic functionality
  result <- estimate_delay(triangle)
  expect_is(result, "data.frame")
  expect_identical(as.integer(nrow(result)), as.integer(ncol(triangle) - 1))
  expect_identical(colnames(result), c("delay", "pmf"))
  expect_true(all(result$delay == 0:(ncol(triangle) - 2)))
  expect_true(all(result$pmf >= 0 & result$pmf <= 1))
  expect_equal(sum(result$pmf), 1, tolerance = 1e-6)

  # Test 2: Custom max_delay
  result_max_delay <- estimate_delay(triangle, max_delay = 3)
  expect_identical(as.integer(nrow(result_max_delay)), 4L)

  # Test 3: Custom n_history
  result_n_history <- estimate_delay(triangle, n_history = 20)
  expect_is(result_n_history, "data.frame")

  # Test 4: Input validation *These should be more useful error messages*
  expect_error(estimate_delay(triangle, max_delay = 0))
  expect_error(estimate_delay(triangle, n_history = 0))
  expect_error(estimate_delay(triangle, max_delay = 10, n_history = 40))

  # Test 5: Handling of missing values
  triangle_with_na <- triangle
  triangle_with_na[1, 2] <- NA
  result_with_na <- estimate_delay(triangle_with_na)
  expect_is(result_with_na, "data.frame")

  # Test 6: Consistency of results
  result1 <- estimate_delay(triangle)
  result2 <- estimate_delay(triangle)
  expect_identical(result1, result2)

  # Test 7: Handling different input types
  result_df <- estimate_delay(as.data.frame(triangle))
  result_dt <- estimate_delay(data.table::as.data.table(triangle))
  expect_identical(result_dt, result_df)

  # Test 8: Check that the function errors if its not passed a triangle
  triangle_single_day <- triangle[1, ]
  expect_error(estimate_delay(triangle_single_day))

  # Test 9: Check that function throws error if column names are incorrect
  triangle_renamed <- as.data.frame(triangle)
  triangle_renamed <- stats::setNames(
    triangle_renamed,
    c("date", paste0("delay_", 0:4))
  )
  expect_error(estimate_delay(triangle_renamed),
    regexp = "Names must include the elements"
  )
})
