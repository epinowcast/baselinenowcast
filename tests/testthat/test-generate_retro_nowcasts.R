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

### Test 1: Basic Functionality
test_that("Function returns correctly structured output", {
  result <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list)

  # Output has same number of elements as input
  expect_identical(length(result), length(retro_rts_list))

  # Each output matrix has same dimensions as input
  for (i in seq_along(retro_rts_list)) {
    expect_identical(dim(result[[i]]), dim(retro_rts_list[[i]]))
  }

  # Check that NAs are replaced (lower-right corner filled)
  # This assumes your method removes all NAs
  expect_false(any(is.na(result[[1]])))
  expect_false(any(is.na(result[[2]])))
})

### Test 2: Default n_history_delay Calculation
test_that("Default n_history_delay uses minimum rows", {
  # Input matrices have 7 and 7 rows → min = 6
  result_default <- generate_retro_nowcasts(list_of_retro_rts = retro_rts_list)
  result_custom_w_def <- generate_retro_nowcasts(
    list_of_retro_rts = retro_rts_list,
    n_history_delay = 6
  )
  result_custom <- generate_retro_nowcasts(
    list_of_retro_rts = retro_rts_list,
    n_history_delay = 5
  )

  # Ensure default matches explicit use of min rows
  expect_identical(result_default, result_custom_w_def)
  # Because estimates are made from different observations, expect these
  # not to be identical
  expect_false(all(result_default[[1]] == result_custom[[1]]))
})

### Test 3: Custom n_history_delay
test_that("Custom n_history_delay is respected", {
  result <- generate_retro_nowcasts(
    list_of_retro_rts = retro_rts_list,
    n_history_delay = 5
  )
  # Custom n_history_delay is too high
  expect_error(
    generate_retro_nowcasts(
      list_of_retro_rts = retro_rts_list,
      n_history_delay = 8
    ),
    regexp = "The number of observations specified for delay estimation is greater"
  ) # nolint
  # Custom n_history_delay is too low
  expect_error(
    generate_retro_nowcasts(
      list_of_retro_rts = retro_rts_list,
      n_history_delay = 3
    ),
    regexp = "The number of observations specified for delay estimation is less"
  ) # nolint
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
  same_size_list <- list(test_triangle_1[2:7, ], test_triangle_2)
  result <- generate_retro_nowcasts(same_size_list)

  # Number of rows of each matrix should be identical (6 and 6)
  expect_identical(sapply(result, nrow), c(6L, 6L))
})
