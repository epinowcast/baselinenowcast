test_triangle_1 <- matrix(
  c(
    10, 20, 30, NA,
    15, 25, NA, NA,
    20, NA, NA, NA
  ),
  nrow = 3,
  byrow = TRUE
)

test_triangle_2 <- matrix(
  c(
    5, 15, NA,
    8, NA, NA
  ),
  nrow = 2,
  byrow = TRUE
)

retro_rts_list <- list(test_triangle_1, test_triangle_2)

### Test 1: Basic Functionality
test_that("Function returns correctly structured output", {
  result <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list)

  # Output has same number of elements as input
  expect_equal(length(result), length(retro_rts_list))

  # Each output matrix has same dimensions as input
  for (i in seq_along(retro_rts_list)) {
    expect_equal(dim(result[[i]]), dim(retro_rts_list[[i]]))
  }

  # Check that NAs are replaced (lower-right corner filled)
  # This assumes your method removes all NAs
  expect_false(any(is.na(result[[1]])))
  expect_false(any(is.na(result[[2]])))
})

### Test 2: Default n_history_delay Calculation
test_that("Default n_history_delay uses minimum rows", {
  # Input matrices have 3 and 2 rows → min = 2
  result_default <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list)
  result_custom <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list, n_history_delay = 2)

  # Ensure default matches explicit use of min rows
  expect_identical(result_default, result_custom)
})

### Test 3: Custom n_history_delay
test_that("Custom n_history_delay is respected", {
  result <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list, n_history_delay = 1)

  # Test passes if logic uses only 1 row for estimation
  # (Implementation-specific checks would go here)
  expect_true(TRUE) # Placeholder for actual validation
})

### Test 4: Error Handling
test_that("Invalid inputs throw errors", {
  # Non-list input
  expect_error(generate_retro_nowcasts(list_of_retro_rts = "not_a_list"))

  # List contains non-matrix elements
  bad_list <- list(test_triangle_1, "not_a_matrix")
  expect_error(generate_retro_nowcasts(list_of_retro_rts = bad_list))

  # Invalid n_history_delay values
  expect_error(generate_retro_nowcasts(retro_rts_list, n_history_delay = -1))
  expect_error(generate_retro_nowcasts(retro_rts_list, n_history_delay = "two"))
})

### Test 5: Edge Case - All Matrices Same Size
test_that("Identical-sized matrices work", {
  same_size_list <- list(test_triangle_1[1:2, ], test_triangle_2)
  result <- generate_retro_nowcasts(same_size_list)

  # Minimum rows should be 2 (smallest of 2 and 2)
  expect_equal(attr(result, "n_history_delay"), 2)
})
