test_that("function returns a dataframe with correct structure", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, NA,
      NA, NA, NA, 16.8,
      NA, NA, 21.2, 19.5
    ),
    nrow = 4,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4, 9.1)
  n_draws <- 10

  result <- get_nowcast_pred_draws(point_nowcast_pred_matrix, disp, n_draws)

  expect_is(result, "data.frame")
  expect_identical(nrow(result), as.integer(n_draws * nrow(point_nowcast_pred_matrix))) # nolint
  expect_identical(ncol(result), 3L)
  expect_true(all(c("pred_count", "time", "draw") %in% names(result)))
})

test_that("function handles the default n_draws parameter", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 16.8
    ),
    nrow = 2,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4)

  result <- get_nowcast_pred_draws(
    point_nowcast_pred_matrix,
    disp
  )


  # Check that 1000 draws were created
  expect_length(unique(result$draw), 1000L)
})


test_that("draws are correctly indexed", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 16.8,
      NA, NA, 21.2, 19.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4)
  n_draws <- 5

  # Create predictable outputs for each draw
  draw_outputs <- list()
  for (i in 1:n_draws) {
    draw_outputs[[i]] <- rep(i, nrow(point_nowcast_pred_matrix))
  }

  result <- get_nowcast_pred_draws(point_nowcast_pred_matrix, disp, n_draws)

  # Check that each draw has the correct pred_count values
  for (i in 1:n_draws) {
    draw_data <- result[result$draw == i, ]
    expect_identical(as.integer(draw_data$draw), as.integer(draw_outputs[[i]]))
  }
})

test_that("time index is correctly assigned", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 16.8,
      NA, NA, 21.2, 19.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  disp <- c(0.8, 12.4)
  n_draws <- 3
  result <- get_nowcast_pred_draws(point_nowcast_pred_matrix, disp, n_draws)
  # For each draw, time should go from 1 to nrow(matrix)
  for (i in 1:n_draws) {
    draw_data <- result[result$draw == i, ]
    expect_identical(as.integer(draw_data$time), as.integer(seq_along(1:nrow(point_nowcast_pred_matrix)))) # nolint
  }
})

test_that("function works with different number of draws", {
  # Setup test data
  point_nowcast_pred_matrix <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 16.8
    ),
    nrow = 2,
    byrow = TRUE
  )
  disp <- 0.8
  n_draws <- 100

  result <- get_nowcast_pred_draws(point_nowcast_pred_matrix, disp, n_draws)

  # Check that all draws were created
  expect_length(unique(result$draw), as.integer(n_draws))
  expect_identical(nrow(result), as.integer(n_draws * nrow(point_nowcast_pred_matrix))) # nolint
})

test_that("function handles single-row matrix", {
  # Setup test data with a single row
  point_nowcast_pred_matrix <- matrix(
    c(NA, NA, NA, 16.8),
    nrow = 1,
    byrow = TRUE
  )
  disp <- 0.8
  n_draws <- 5

  result <- get_nowcast_pred_draws(point_nowcast_pred_matrix, disp, n_draws)


  # Check that result has correct structure
  expect_identical(nrow(result), as.integer(n_draws))
  expect_identical(result$time, as.integer(rep(1, n_draws)))
})
