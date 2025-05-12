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
    11, 21, 8,
    10, 15.3, 3.5
  ),
  nrow = 5,
  byrow = TRUE
)
nowcast2 <- matrix(
  c(
    10, 7, 0.8,
    15, 12, 2.0,
    14, 16, 2.5,
    11, 25.5, 2
  ),
  nrow = 4,
  byrow = TRUE
)


valid_nowcasts <- list(nowcast1, nowcast2)

valid_trunc_rts <- list(
  test_triangle[1:5, ],
  test_triangle[1:4, ]
)
test_that("Basic functionality with valid inputs", {
  result <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    n = 2
  )

  # Verify output structure
  expect_type(result, "double")
  expect_length(result, ncol(valid_nowcasts[[1]]) - 1)
  expect_true(all(is.finite(result)))
})

test_that("Function can handle rolling sum with k=2", {
  result1 <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 2
  )

  result2 <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    n = 2
  )

  expect_failure(expect_equal(result1, result2, tol = 0.01))
})

test_that("Function throws an error if function to aggregate is not valid", {
  expect_error(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    n = 2,
    fun_to_aggregate = summary,
    k = 2
  ))
  # Mean doesn't work right now because we haven't added another error model
  expect_error(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    n = 2,
    fun_to_aggregate = mean,
    k = 2
  ))
})
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
  expect_error(estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = 3))

  # pt nowcast contains NAs (use trunc rts) or is empty
  expect_error(estimate_dispersion(valid_trunc_rts, valid_trunc_rts))
  expect_error(estimate_dispersion(list(), valid_trunc_rts))

  # trunc rep mat list does not contain NAs (use pt nowcasts)
  expect_error(estimate_dispersion(valid_nowcasts, list()))

  # observations contain non-integers
  test_triangle_decimal <- test_triangle + 0.1
  non_integer_trunc_rts <- list(
    test_triangle_decimal[1:5, ],
    test_triangle_decimal[1:4, ]
  )
  expect_error(estimate_dispersion(valid_nowcasts, non_integer_trunc_rts))
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
  expect_error(
    estimate_dispersion(valid_nowcasts, bad_trunc_rts)
  )
})

## Test 6: fit_nb returns NA if nothing passed to it---------------------------
test_that("Passing in empty vector returns NA", {
  x <- NULL
  NA_result <- .fit_nb(x, mu = 1)
  expect_true(is.na(NA_result))
})
