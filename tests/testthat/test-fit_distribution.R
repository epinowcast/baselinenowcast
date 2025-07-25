# Sample data setup
test_obs <- matrix(
  c(
    4, 15, 10,
    8, 10, 6
  ),
  nrow = 2,
  byrow = TRUE
)

test_pred <- matrix(
  c(
    11, 21, 8,
    10, 15.3, 3.5
  ),
  nrow = 2,
  byrow = TRUE
)

test_that("fit_distribution: works with all three options for error functions", {
  result <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
    observation_model_name_name = "dnbinom"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))

  result <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
    observation_model_name = "dnorm"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))

  result <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
    observation_model_name = "dgamma"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))
})
