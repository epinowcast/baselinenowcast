# Sample data setup
test_obs <- matrix(
  c(
    4, 15, 10,
    8, 16, 6
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

test_that("fit_obs_vs_pred: Basic functionality with valid inputs", {
  result <- fit_obs_vs_pred(
    obs = test_obs,
    pred = test_pred,
    density_function = dnbinom
  )
})
