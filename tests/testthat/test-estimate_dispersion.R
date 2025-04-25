# Sample data setup
test_triangle <- matrix(
  c(
    10, 7, 1,
    15, 12, 2,
    14, 16, 3,
    11, 21, 5,
    10, 20, NA,
    15, NA, NA
  ),
  nrow = 6,
  byrow = TRUE
)

nowcast1 <- matrix(
  c(
    10, 7, 1,
    15, 12, 2,
    14, 16, 3,
    11, 21, 8.1,
    10, 15.3, 3.5
  ),
  nrow = 5,
  byrow = TRUE
)
nowcast2 <- matrix(
  c(
    10, 7, 1,
    15, 12, 2.3,
    14, 16, 2.5,
    11, 25.5, 2.1
  ),
  nrow = 4,
  byrow = TRUE
)
nowcast3 <- matrix(
  c(
    10, 7, 1,
    15, 12, 2.0,
    14, 14.1, 1.9
  ),
  nrow = 3,
  byrow = TRUE
)


valid_nowcasts <- list(nowcast1, nowcast2, nowcast3)

valid_trunc_rts <- list(
  test_triangle[1:5, ],
  test_triangle[1:4, ],
  test_triangle[1:3, ]
)
### Test 1: Basic Functionality ------------------------------------------------
test_that("Basic functionality with valid inputs", {
  result <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_mat_list = valid_trunc_rts,
    n = 3
  )

  # Verify output structure
  expect_type(result, "double")
  expect_length(result, ncol(valid_nowcasts[[1]]) - 1)
  expect_true(all(is.finite(result)))
})

### Test 2: Default Parameter Handling -----------------------------------------
test_that("Default n parameter works correctly", {
  result_default <- estimate_dispersion(valid_nowcasts, valid_trunc_rts)
  result_explicit <- estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = 2)
  expect_identical(result_default, result_explicit)
})

### Test 3: Error Conditions ---------------------------------------------------
test_that("Error conditions are properly handled", {
  # Invalid input types
  expect_error(estimate_dispersion(list("not_a_matrix"), valid_trunc_rts))
  expect_error(estimate_dispersion(valid_nowcasts, list("not_a_matrix")))

  # Mismatched list lengths but still possible to estimate using only
  # the nowcasts
  expect_silent(estimate_dispersion(valid_nowcasts[1], valid_trunc_rts))
  expect_error(
    estimate_dispersion(valid_nowcasts[1], valid_trunc_rts, 2)
  ) # nolint

  # Invalid n values
  expect_error(estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = -1))
  expect_error(estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = 1.5))
  expect_error(estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = 4))
})

### Test 4: Edge Cases ---------------------------------------------------------
test_that("Edge cases are handled properly", {
  # Empty lists
  expect_error(estimate_dispersion(list(), list(), n = 0))

  # NA-filled matrices (This should error!)
  na_nowcasts <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  na_trunc <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  expect_error(estimate_dispersion(na_nowcasts, na_trunc, n = 2))
})

### Test 5: Dimension Validation -----------------------------------------------
test_that("Matrix dimension validation works", {
  # Mismatched dimensions between nowcasts and trunc_rts
  bad_trunc_rts <- list(
    test_triangle[1:5, ],
    test_triangle[1:3, ]
  )
  expect_error(estimate_dispersion(valid_nowcasts, bad_trunc_rts)) # nolint
})

## Test 6: fit_nb returns NA if nothing passed to it---------------------------
test_that("Passing in empty vector returns NA", {
  x <- NULL
  NA_result <- .fit_nb(x, mu = 1)
  expect_true(is.na(NA_result))
})
