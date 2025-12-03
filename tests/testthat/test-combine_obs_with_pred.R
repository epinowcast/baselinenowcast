test_that("combine_obs_with_pred: correctly combines observed and predicted", {
  # Create simple test data
  predicted_counts <- c(0, 3, 4, 12)
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
  reporting_triangle <- apply_reporting_structure(
    make_test_triangle(data = reporting_matrix)
  )

  result <- combine_obs_with_pred(predicted_counts, reporting_triangle)

  # Calculate expected result
  obs_counts <- rowSums(reporting_triangle, na.rm = TRUE)
  expected <- obs_counts + predicted_counts

  expect_identical(result, expected)
})

test_that("combine_obs_with_pred: handles aggregation function properly", {
  # Create simple test data
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
  aggr_reporting_matrix <- zoo::rollsum(reporting_matrix,
    k = 2,
    align = "right"
  )
  pred_counts <- c(0, 3, 9, 21)
  reporting_triangle <- apply_reporting_structure(
    make_test_triangle(data = reporting_matrix)
  )
  aggr_reporting_triangle <- zoo::rollsum(reporting_triangle,
    k = 2,
    align = "right"
  )
  obs_counts <- rowSums(aggr_reporting_triangle, na.rm = TRUE)
  # First test that the logic of the function works as we'd expect, without
  # using the function
  expect_identical(obs_counts + pred_counts, rowSums(aggr_reporting_matrix))

  # Calculate expected results
  expected <- obs_counts + pred_counts

  # Use function
  result <- combine_obs_with_pred(pred_counts,
    reporting_triangle,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
  )


  expect_identical(result, expected)
})
