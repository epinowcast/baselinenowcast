test_that(".get_exp_and_obs_as_of_t function works correctly", {
  # Setup
  set.seed(123)
  matr_observed <- matrix(c(
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  delay_pmf <- c(0.5, 0.3, 0.15, 0.05)
  n_horizons <- length(delay_pmf) - 1

  # Test 1: Basic functionality
  result <- .get_exp_and_obs_as_of_t(
    t = 3,
    matr_observed = matr_observed,
    delay_pmf = delay_pmf
  )

  expect_is(result, "data.frame")
  expect_identical(nrow(result), as.integer(n_horizons))
  expect_named(result, c("exp_to_add", "to_add", "t", "d"))

  # Test 2: Check if t is respected
  result_t2 <- .get_exp_and_obs_as_of_t(
    t = 2,
    matr_observed = matr_observed,
    delay_pmf = delay_pmf
  )
  expect_true(all(result_t2$t == 2))
  expect_true(all(result_t2$d == 1:n_horizons))

  # Test 3: Check if NAs are handled correctly, there shoudl be none
  expect_false(anyNA(result$exp_to_add))
  expect_false(anyNA(result$to_add))

  # Test 4: Check if the function handles edge case (t = 1)
  result_t1 <- .get_exp_and_obs_as_of_t(
    t = 1,
    matr_observed = matr_observed,
    delay_pmf = delay_pmf
  )
  expect_identical(nrow(result_t1), as.integer(n_horizons))
  expect_identical(result_t1$to_add, c(50, 30, 20))
  # Do the compute for t=1 bc its easy
  expect_identical(
    result_t1$exp_to_add,
    matr_observed[1, 1] / delay_pmf[1] *
      delay_pmf[2:length(delay_pmf)]
  )


  # Test 5: Check if the function handles edge case (t = nrow(matr_observed))
  result_t_max <- .get_exp_and_obs_as_of_t(
    t = nrow(matr_observed),
    matr_observed = matr_observed,
    delay_pmf = delay_pmf
  )
  expect_identical(nrow(result_t_max), as.integer(n_horizons))
  # For the last time point, none are observed bc all NAs so everything should
  # be 0s
  expect_identical(result_t_max$to_add, rep(0, length(delay_pmf) - 1))
  expect_identical(result_t_max$exp_to_add, rep(0, length(delay_pmf) - 1))

  # Test 6: Check if the function errors with invalid inputs
  expect_error(.get_exp_and_obs_as_of_t(
    t = 0,
    matr_observed = matr_observed,
    delay_pmf = delay_pmf
  ))
  expect_error(.get_exp_and_obs_as_of_t(
    t = 3,
    matr_observed = matr_observed,
    delay_pmf = c(0.5, 0.5)
  ))
})
