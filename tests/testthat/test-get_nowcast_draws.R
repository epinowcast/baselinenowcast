test_that(
  "get_nowcast_draws: returns a dataframe with correct structure",
  {
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
    reporting_triangle <- generate_triangle(point_nowcast_matrix)

    result <- get_nowcast_draws(
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
  }
)

test_that("get_nowcast_draws: draws are distinct and properly indexed", {
  # Setup test data
  point_nowcast_matrix <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, 16.8,
      80, 40, 21.2, 19.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  dispersion <- c(0.8, 12.4)
  reporting_triangle <- generate_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 5

  # Force seed for reproducibility
  set.seed(123)
  result <- get_nowcast_draws(
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

test_that("get_nowcast_draws: time index is correctly assigned", {
  # Setup test data
  point_nowcast_matrix <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, 16.8,
      80, 40, 21.2, 19.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  dispersion <- c(0.8, 12.4)
  reporting_triangle <- generate_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 3

  result <- get_nowcast_draws(
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

test_that("get_nowcast_draws: function works with different number of draws", {
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
  reporting_triangle <- generate_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 100

  result <- get_nowcast_draws(
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
  "get_nowcast_draws: ingests k and fun to aggregate appropriately",
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
    reporting_triangle <- generate_triangle(point_nowcast_matrix)

    result_with_rolling_sum <- get_nowcast_draws(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
      draws = 100,
      fun_to_aggregate = sum,
      k = 2
    )
    result <- get_nowcast_draws(
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
    expect_equal(
      sum(result$pred_count[result$time <= 2]),
      sum(result_with_rolling_sum$pred_count[result_with_rolling_sum$time == 2])
    )
    # All draws should be the same
    expect_equal(
      result_with_rolling_sum$pred_count[result_with_rolling_sum$time == 2 & result_with_rolling_sum$draw == 2],
      result_with_rolling_sum$pred_count[result_with_rolling_sum$time == 2 & result_with_rolling_sum$draw == 1]
    )
  }
)
