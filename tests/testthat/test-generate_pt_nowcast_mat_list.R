test_triangle_1 <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, NA,
    82, 42, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
)

test_triangle_2 <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, NA,
    95, 45, NA, NA,
    82, NA, NA, NA
  ),
  nrow = 6,
  byrow = TRUE
)

retro_rts_list <- list(test_triangle_1, test_triangle_2)
delay_pmf <- c(0.1, 0.1, 0.5, 0.3)

###  Basic Functionality
test_that("Function returns correctly structured output", {
  result <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list
  )

  # Output has same number of elements as input
  expect_length(result, 2)

  # Each output matrix has same dimensions as input
  for (i in seq_along(retro_rts_list)) {
    expect_identical(dim(result[[i]]), dim(retro_rts_list[[i]]))
  }

  # Check that NAs are replaced (lower-right corner filled)
  # This assumes your method removes all NAs
  expect_false(anyNA(result[[1]]))
  expect_false(anyNA(result[[2]]))
})

### Function takes in separate delay_pmf
test_that("Function takes in delay_pmf as vector or list", {
  result1 <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = delay_pmf
  )

  result2 <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = rep(list(delay_pmf), length(retro_rts_list))
  )

  expect_identical(result1, result2)

  expect_error(generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = rep(list(delay_pmf), length(retro_rts_list) - 1)
  ))
})

### Default n_history_delay Calculation
test_that("Default n_history_delay uses minimum rows", {
  # Input matrices have 7 and 7 rows → min = 6
  result_default <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list
  )
  result_custom_w_def <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    n = 6
  )
  result_custom <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    n = 5
  )

  # Ensure default matches explicit use of min rows
  expect_identical(result_default, result_custom_w_def)
  # Because estimates are made from different observations, expect these
  # not to be identical
  expect_false(all(result_default[[1]] == result_custom[[1]]))
})

### Test 3: Custom n_history_delay
test_that("Custom n_history_delay is respected", {
  result <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    n = 5
  )
  # Compare to estimate using n=5
  first_triangle <- result[[1]]
  delay_pmf <- get_delay_estimate(retro_rts_list[[1]],
    n = 5
  )
  exp_first_triangle <- apply_delay(retro_rts_list[[1]], delay_pmf)
  expect_equal(first_triangle, exp_first_triangle, tol = 0.0001)

  expect_error(generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    n = 3
  ))
  expect_error(generate_pt_nowcast_mat_list(
    reporting_triangle_list = retro_rts_list,
    n = 8
  ))
})

### Test 4: Error Handling
test_that("Invalid inputs throw errors", {
  # Non-list input
  expect_error(generate_pt_nowcast_mat_list(
    reporting_triangle_list = "not_a_list"
  ))

  # List contains non-matrix elements
  bad_list <- list(test_triangle_1, "not_a_matrix")
  expect_error(generate_pt_nowcast_mat_list(
    reporting_triangle_matrix_list = bad_list
  ))

  # Invalid n_history_delay values
  expect_error(generate_pt_nowcast_mat_list(retro_rts_list, n = -1))
  expect_error(generate_pt_nowcast_mat_list(retro_rts_list, n = "two"))
})

### Test 5: Edge Case - All Matrices Same Size
test_that("Identical-sized matrices work", {
  same_size_list <- list(test_triangle_1[2:7, ], test_triangle_2)
  result <- generate_pt_nowcast_mat_list(same_size_list)

  # Number of rows of each matrix should be identical (6 and 6)
  expect_identical(sapply(result, nrow), c(6L, 6L))
})
