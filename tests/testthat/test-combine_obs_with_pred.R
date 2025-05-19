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
  reporting_triangle <- generate_triangle(reporting_matrix)

  result <- combine_obs_with_pred(predicted_counts, reporting_triangle)

  # Calculate expected result
  obs_counts <- rowSums(reporting_triangle, na.rm = TRUE)
  expected <- obs_counts + predicted_counts

  expect_identical(result, expected)
})
