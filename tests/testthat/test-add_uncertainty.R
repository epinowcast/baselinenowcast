test_matrix <- matrix(
  c(
    10, 20, 30, 40,
    15, 25, 35, 45.6,
    20, 30.4, 40.9, 50.4,
    25, 35.1, 45.3, 55.5
  ),
  nrow = 4,
  byrow = TRUE
)

valid_disp <- c(5, 3, 2)
n_draws <- 10

### Test 1: Basic Functionality ------------------------------------------------
test_that("Basic functionality with valid inputs", {
  set.seed(123)
  result <- add_uncertainty(test_matrix, valid_disp, n_draws)

  # Verify output structure
  expect_type(result, "list")
  expect_length(result, n_draws)
  expect_true(all(sapply(result, is.matrix)))
  expect_true(all(sapply(result, \(x) identical(dim(x), dim(test_matrix)))))
})

### Test 2: Input Validation ---------------------------------------------------
test_that("Input validation works correctly", {
  # Invalid comp_rep_square, must be a matrix
  expect_error(add_uncertainty(
    as.data.frame(test_matrix),
    valid_disp
  ))

  # Invalid dispersion parameters
  expect_error(add_uncertainty(test_matrix, c(-1, 2, 3))) # Negative #nolint
  expect_error(add_uncertainty(test_matrix, c(1, 2))) # mismatch

  # Invalid n_draws
  expect_error(add_uncertainty(test_matrix,
    valid_disp,
    n_draws = -5
  ))
  expect_error(add_uncertainty(test_matrix,
    valid_disp,
    n_draws = 2.5
  ))
  # case where draws is 0
  expect_error(add_uncertainty(test_matrix, valid_disp, 0))

  # dispersion insufficient
  expect_error(add_uncertainty(test_matrix, c(1, 2), 1))
})

### Test 3: Stochastic Behavior ------------------------------------------------
test_that("Random generation is reproducible with seed", {
  set.seed(456)
  result1 <- add_uncertainty(test_matrix, valid_disp, 5)
  set.seed(456)
  result2 <- add_uncertainty(test_matrix, valid_disp, 5)
  expect_identical(result1, result2)
})

### Test 4: Default Parameter Handling -----------------------------------------
test_that("Default n_draws works correctly", {
  default_result <- add_uncertainty(test_matrix, valid_disp)
  expect_length(default_result, 1000)
})

### Test 6: Observation Preservation -------------------------------------------
test_that("Upper triangle remains unchanged", {
  set.seed(789)
  result <- add_uncertainty(test_matrix, valid_disp, 5)

  # Check upper triangle matches input for all draws
  lapply(result, function(x) {
    return(expect_identical(x[1:2, 1:2], test_matrix[1:2, 1:2]))
  })
})

### Test 7: Error Distribution -------------------------------------------------
test_that("Lower triangle shows expected variation", {
  set.seed(111)
  results <- add_uncertainty(test_matrix, c(100, 100, 100), 100)
  lower_right_vals <- sapply(results, function(x) x[3:4, 3:4])

  # Check coefficient of variation is within expected range
  cv <- sd(lower_right_vals) / mean(lower_right_vals)
  expect_gt(cv, 0.05)
  expect_lt(cv, 0.5) # Adjust based on dispersion parameters
})
