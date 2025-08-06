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
  "sample_predictions: returns a dataframe with correct structure",
  {
    result <- sample_predictions(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
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
    expect_false(all(is.na(result$pred_count)))
  }
)

test_that(
  "sample_predictions: can handle different observation models",
  {
    sd <- c(0.8, 12.4, 9.1)
    reporting_triangle <- construct_triangle(point_nowcast_matrix)

    result_normal <- sample_predictions(
      point_nowcast_matrix,
      reporting_triangle,
      sd,
      error_args = list(observation_model_name = "normal"),
      draws = 100
    )

    expect_is(result_normal, "data.frame")
    expect_identical(
      nrow(result_normal),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_identical(ncol(result_normal), 3L)
    expect_true(all(c("pred_count", "time", "draw") %in% names(result_normal)))
    expect_length(unique(result_normal$draw), 100L)
    expect_identical(
      nrow(result_normal),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_false(all(is.na(result_normal$pred_count)))

    result_gamma <- sample_predictions(
      point_nowcast_matrix,
      reporting_triangle,
      sd,
      error_args = list(observation_model_name = "gamma"),
      draws = 100
    )

    expect_is(result_gamma, "data.frame")
    expect_identical(
      nrow(result_gamma),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_identical(ncol(result_gamma), 3L)
    expect_true(all(c("pred_count", "time", "draw") %in% names(result_gamma)))
    expect_length(unique(result_gamma$draw), 100L)
    expect_identical(
      nrow(result_gamma),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_false(all(is.na(result_gamma$pred_count)))

    result_nb <- sample_predictions(
      point_nowcast_matrix,
      reporting_triangle,
      sd,
      error_args = list(observation_model_name = "negative binomial"),
      draws = 100
    )

    expect_is(result_nb, "data.frame")
    expect_identical(
      nrow(result_nb),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_identical(ncol(result_nb), 3L)
    expect_true(all(c("pred_count", "time", "draw") %in% names(result_nb)))
    expect_length(unique(result_nb$draw), 100L)
    expect_identical(
      nrow(result_nb),
      as.integer(100 * nrow(point_nowcast_matrix))
    )
    expect_false(all(is.na(result_nb$pred_count)))

    expect_false(all(result_nb == result_normal))
    expect_false(all(result_normal == result_gamma))
    expect_false(all(result_gamma == result_nb))

    # I think its okay that these aren't the same?
    mean_normal <- mean(result_normal$pred_count[result_normal$time == 4])
    mean_gamma <- mean(result_gamma$pred_count[result_gamma$time == 4])
    mean_nb <- mean(result_nb$pred_count[result_nb$time == 4])

    median_normal <- quantile(
      result_normal$pred_count[result_normal$time == 4],
      0.5
    )
    median_gamma <- quantile(
      result_gamma$pred_count[result_gamma$time == 4],
      0.5
    )
    median_nb <- quantile(
      result_nb$pred_count[result_nb$time == 4],
      0.5
    )
    expect_equal(mean_normal, mean_gamma, tolerance = 20)
    expect_equal(mean_normal, mean_nb, tolerance = 20)
    expect_equal(mean_nb, mean_gamma, tolerance = 20)
    expect_equal(median_normal, median_gamma, tolerance = 10)
    expect_equal(median_normal, median_nb, tolerance = 10)
    expect_equal(median_nb, median_gamma, tolerance = 10)
  }
)

test_that("sample_predictions: draws are distinct and properly indexed", {
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
  reporting_triangle <- construct_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 5

  # Force seed for reproducibility
  set.seed(123)
  result <- sample_predictions(
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

test_that("sample_predictions: time index is correctly assigned", {
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
  reporting_triangle <- construct_triangle(point_nowcast_matrix, structure = 2)
  n_draws <- 3

  result <- sample_predictions(
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

test_that("sample_predictions works with different number of draws", {
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

  result <- sample_predictions(
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

test_that("sample_predictions: errors when too many or too few uncertainty parameters", { # nolint
  expect_error(
    sample_predictions(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion[1:2],
      draws = 10
    ),
    regexp = "Vector of uncertainty parameter is less than the number "
  )

  expect_error(sample_predictions(
    point_nowcast_matrix,
    reporting_triangle,
    c(dispersion, rep(3, 3)),
    draws = 10
  ))
})
