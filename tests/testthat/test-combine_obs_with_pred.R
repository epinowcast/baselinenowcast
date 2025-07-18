test_that("combine_obs_with_pred: correctly combines observed and predicted", {
  # Create simple test data
  predicted_counts <- c(10, 20, 30, 40)
  reporting_matrix <- matrix(
    c(
      5, 3, 2, 1,
      6, 4, 3, 2,
      7, 5, 3, 1,
      8, 6, 4, 2
    ),
    nrow = 4,
    byrow = TRUE
  )
  reporting_triangle <- construct_triangle(reporting_matrix)

  result <- combine_obs_with_pred(predicted_counts, reporting_triangle)

  # Calculate expected result
  obs_counts <- rowSums(reporting_triangle, na.rm = TRUE)
  expected <- obs_counts + predicted_counts

  expect_identical(result, expected)
})

test_that("combine_obs_with_pred: handles aggregation function properly", {
  # Create simple test data
  predicted_counts <- c(0, 0, 20, 30, 40)
  reporting_matrix <- matrix(
    c(
      7, 6, 3, 2,
      5, 3, 2, 1,
      6, 4, 3, 2,
      7, 5, 3, 1,
      8, 6, 4, 2
    ),
    nrow = 5,
    byrow = TRUE
  )
  reporting_triangle <- construct_triangle(reporting_matrix)

  result <- combine_obs_with_pred(predicted_counts,
    reporting_triangle,
    k = 2
  )

  # Calculate expected result
  obs_counts <- rowSums(reporting_triangle, na.rm = TRUE)
  expected <- rollapply(obs_counts,
    2,
    sum,
    fill = NA,
    align = "right"
  ) + predicted_counts

  expect_identical(result, expected)
})

test_that("combine_obs_with_pred: errors if invalid `fun_to_aggregate` is passed", { # nolint
  # Create simple test data
  predicted_counts <- c(0, 0, 20, 30, 40)
  reporting_matrix <- matrix(
    c(
      7, 6, 3, 2,
      5, 3, 2, 1,
      6, 4, 3, 2,
      7, 5, 3, 1,
      8, 6, 4, 2
    ),
    nrow = 5,
    byrow = TRUE
  )
  reporting_triangle <- construct_triangle(reporting_matrix)

  expect_error(combine_obs_with_pred(predicted_counts,
    reporting_triangle,
    k = 2,
    fun_to_aggregate = mean
  ))
})
