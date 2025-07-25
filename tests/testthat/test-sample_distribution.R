# Sample data setup
test_pred <- matrix(
  c(
    11, 21, 8,
    10, 15.3, 3.5
  ),
  nrow = 2,
  byrow = TRUE
)
uncertainty_params <- c(5, 6)

test_that("sample_distribution: works with all three options for error functions", {
  result <- sample_distribution(
    pred = test_pred,
    uncertainty_params = uncertainty_params,
    observation_model_name = "dnbinom"
  )
  expect_type(result, "double")
  expect_length(result, length(test_pred))
  expect_true(all(is.finite(result)))


  result <- sample_distribution(
    pred = test_pred,
    uncertainty_params = uncertainty_params,
    observation_model_name = "dnorm"
  )
  expect_type(result, "double")
  expect_length(result, length(test_pred))
  expect_true(all(is.finite(result)))

  result <- sample_distribution(
    pred = test_pred,
    uncertainty_params = uncertainty_params,
    observation_model_name = "dgamma"
  )
  expect_type(result, "double")
  expect_length(result, length(test_pred))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))
})
