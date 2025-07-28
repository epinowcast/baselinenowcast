point_nowcast_matrix <- matrix(
  c(
    100, 50, 30, 20,
    90, 45, 25, 16.8,
    80, 40, 21.2, 19.5,
    70, 34.5, 15.4, 9.1
  ),
  nrow = 4,
  byrow = TRUE
)
dispersion <- c(0.8, 12.4, 9.1)
reporting_triangle <- construct_triangle(point_nowcast_matrix)


test_that(
  "sample_nowcasts: returns a dataframe with correct structure",
  {
    result <- sample_nowcasts(
      point_nowcast_matrix, reporting_triangle, dispersion,
      draws = 100
    )

    expect_is(result, "data.frame")
    expect_identical(
      nrow(result),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_identical(ncol(result), 3L)
    expect_true(all(c("pred_count", "time", "draw") %in% names(result)))
    expect_length(unique(result$draw), 100L)
    expect_identical(nrow(result), as.integer(100 * nrow(point_nowcast_matrix)))
    expect_true(all(is.finite(result$pred_count)))
  }
)

test_that("sample_nowcasts: draws are distinct and properly indexed", {
  # Setup test data
  dispersion <- c(0.8, 12.4)
  reporting_triangle <- construct_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 5

  # Force seed for reproducibility
  set.seed(123)
  result <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion,
    draws = n_draws
  )

  # Check draws are indexed correctly
  expect_identical(sort(unique(result$draw)), as.integer(1:n_draws))

  # Check that we have the correct number of rows per draw
  draws_count <- table(result$draw)
  expect_identical(
    as.vector(draws_count), rep(nrow(point_nowcast_matrix), n_draws)
  )

  # Check that draws are distinct (should have stochasticity)
  # Group by draw and compare values
  draw_vals <- list()
  for (i in 1:n_draws) {
    draw_vals[[i]] <- result$pred_count[result$draw == i]
  }

  # At least some of the draws should be different (with very high probability)
  distinct_draws <- 0
  for (i in 1:(n_draws - 1)) {
    for (j in (i + 1):n_draws) {
      if (!identical(draw_vals[[i]], draw_vals[[j]])) {
        distinct_draws <- distinct_draws + 1
      }
    }
  }

  # With random draws, we expect most pairs to be different
  expected_pairs <- (n_draws * (n_draws - 1)) / 2
  expect_gt(distinct_draws, expected_pairs / 2)
})

test_that("sample_nowcasts: time index is correctly assigned", {
  # Setup test data
  dispersion <- c(0.8, 12.4)
  reporting_triangle <- construct_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 3

  result <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion,
    draws = n_draws
  )

  # For each draw, time should go from 1 to nrow(matrix)
  for (i in 1:n_draws) {
    draw_data <- result[result$draw == i, ]
    expect_identical(
      as.integer(draw_data$time),
      as.integer(seq_len(nrow(point_nowcast_matrix)))
    )
    # Check data is ordered by time within each draw
    expect_identical(draw_data$time, sort(draw_data$time))
  }
})

test_that("sample_nowcasts: function works with different number of draws", {
  # Setup test data
  point_nowcast_matrix <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, 16.8
    ),
    nrow = 2,
    byrow = TRUE
  )
  dispersion <- c(0.8, 0.2)
  reporting_triangle <- construct_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 100

  result <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion,
    draws = n_draws
  )

  # Check that all draws were created
  expect_length(unique(result$draw), as.integer(n_draws))
  expect_identical(
    nrow(result),
    as.integer(n_draws * nrow(point_nowcast_matrix))
  )
})

test_that(
  "sample_nowcasts: ingests k and fun to aggregate appropriately",
  {
    point_nowcast_matrix <- matrix(
      c(
        50, 80, 100, 40,
        100, 50, 30, 20,
        90, 45, 25, 16.8,
        80, 40, 21.2, 19.5,
        70, 34.5, 15.4, 9.1
      ),
      nrow = 5,
      byrow = TRUE
    )
    dispersion <- c(0.8, 12.4, 9.1)
    reporting_triangle <- construct_triangle(point_nowcast_matrix)

    result_with_rolling_sum <- sample_nowcasts(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
      draws = 100,
      aggregator = zoo::rollsum,
      aggregator_args = list(k = 2, align = "right")
    )
    result <- sample_nowcasts(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
      draws = 100
    )

    # Test that result_with_rolling_sum is roughly double,
    # which should be true for both the observations and the
    # predictions
    sum_result <- sum(result$pred_count[result$time != 1])
    sum_result_rolling_sum <- sum(
      result_with_rolling_sum$pred_count[result_with_rolling_sum$time != 1],
      na.rm = TRUE
    )
    expect_equal(sum_result_rolling_sum / sum_result, 2, tol = 0.2)

    # The sum of the first two time points should be the same as the value
    # at t=2 for the rolling sum
    expect_identical(
      sum(result$pred_count[result$time <= 2]),
      sum(result_with_rolling_sum$pred_count[result_with_rolling_sum$time == 2])
    )
    # All draws should be the same
    expect_identical(
      result_with_rolling_sum$pred_count[
        result_with_rolling_sum$time == 2 & result_with_rolling_sum$draw == 2
      ],
      result_with_rolling_sum$pred_count[
        result_with_rolling_sum$time == 2 & result_with_rolling_sum$draw == 1
      ]
    )
  }
)

test_that("sample_nowcasts: longer k aggregates correctly", {
  set.seed(123)
  rep_mat <- matrix(sample.int(1, 20, size = 4 * 10, replace = TRUE),
    nrow = 10, ncol = 4
  )
  triangle <- construct_triangle(rep_mat)

  pt_nowcast_mat <- fill_triangle(triangle)
  dispersion <- c(10, 10, 10)

  expected_mean_mat <- zoo::rollsum(pt_nowcast_mat,
    k = 5,
    fill = NA,
    align = "right"
  )
  expected_mean <- rowSums(expected_mean_mat, na.rm = TRUE)

  result_with_rolling_sum <- sample_nowcasts(
    point_nowcast_matrix = pt_nowcast_mat,
    reporting_triangle = triangle,
    uncertainty_params = dispersion,
    draws = 100,
    aggregator = zoo::rollsum,
    aggregator_args = list(k = 5, align = "right")
  )

  # First 4 rows are NA because of right alignment
  expect_true(all(is.na(result_with_rolling_sum$pred_count[1:4])))

  # First two components of the rolling 5 day sum are the same for different
  # draws because they are all based on observations
  expect_identical(
    result_with_rolling_sum$pred_count[7],
    result_with_rolling_sum$pred_count[17]
  )
  expect_identical(
    result_with_rolling_sum$pred_count[6],
    result_with_rolling_sum$pred_count[16]
  )
  # in subsequent time points, the draws differ
  expect_false(result_with_rolling_sum$pred_count[8] ==
    result_with_rolling_sum$pred_count[18])

  # Mean is about the same as a draw of the last time point
  draw_of_result_last_time <- result_with_rolling_sum$pred_count[10]
  expect_equal(expected_mean[10], draw_of_result_last_time, tol = 5)
})

test_that("sample_nowcasts errors appropriately when passed an observation model that isn't supported", { # nolint
  expect_error(
    sample_nowcasts(
      point_nowcast_matrix = point_nowcast_matrix,
      reporting_triangle = reporting_triangle,
      uncertainty_params = dispersion,
      draws = 100,
      error_args = list(observation_model_name = "bernoulli")
    ),
    regexp = "not supported by `sample_distribution` error model."
  )
})

test_that("sample_nowcasts produces different draws with different sampling models", { # nolint
  set.seed(123)
  result_nb <- sample_nowcasts(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    uncertainty_params = dispersion,
    draws = 1
  )
  set.seed(123)
  result_normal <- sample_nowcasts(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    uncertainty_params = dispersion,
    draws = 1,
    error_args = list(observation_model_name = "normal")
  )
  set.seed(123)
  result_gamma <- sample_nowcasts(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    uncertainty_params = dispersion,
    draws = 1,
    error_args = list(observation_model_name = "gamma")
  )
  expect_false(all(result_nb$pred_count == result_normal$pred_count))
  expect_false(all(result_nb$pred_count == result_gamma$pred_count))
  expect_false(all(result_gamma$pred_count == result_normal$pred_count))

  set.seed(123)
  result_gamma2 <- sample_nowcasts(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    uncertainty_params = dispersion,
    draws = 1,
    error_args = list(observation_model_name = "gamma")
  )
  expect_true(all(result_gamma$pred_count == result_gamma2$pred_count))
})
