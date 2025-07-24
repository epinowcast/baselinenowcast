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

test_that(".fit_distrib: works with all three options for error functions", {
  result <- .fit_distrib(
    obs = test_obs,
    pred = test_pred,
    observation_model = "dnbinom"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))

  result <- .fit_distrib(
    obs = test_obs,
    pred = test_pred,
    observation_model = "dnorm"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))

  result <- .fit_distrib(
    obs = test_obs,
    pred = test_pred,
    observation_model = "dgamma"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))
})
