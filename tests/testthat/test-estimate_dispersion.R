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


test_that("Default n parameter works correctly", {
  result_default <- estimate_dispersion(valid_nowcasts, valid_trunc_rts)
  result_explicit <- estimate_dispersion(valid_nowcasts, valid_trunc_rts, n = 2)
  expect_identical(result_default, result_explicit)
})


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


test_that("Edge cases are handled properly", {
  # Empty lists
  expect_error(expect_warning(estimate_dispersion(list(), list(), n = 0)))

  # No NAs---------------------------------------------------------------------
  expect_warning(estimate_dispersion(
    valid_nowcasts,
    lapply(valid_nowcasts, round)
  ))

  # NA-filled matrices (This should error!)
  na_nowcasts <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  na_trunc <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  expect_error(estimate_dispersion(na_nowcasts, na_trunc, n = 2))
})


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


test_that("Passing in empty vector returns NA", {
  x <- NULL
  NA_result <- .fit_nb(x, mu = 1)
  expect_true(is.na(NA_result))
})

test_that("Passing in a NULL for a nowcast still returns an estimate", {
  nowcasts_with_null <- list(nowcast1, NULL)
  # This should work, using only the first nowcast and first valid_trunc_rts
  result1 <- estimate_dispersion(nowcasts_with_null, valid_trunc_rts)
  result_to_compare <- estimate_dispersion(
    list(nowcast1),
    list(valid_trunc_rts[[1]])
  )
  expect_identical(result1, result_to_compare)
})

test_that("Passing in only NULLs for nowcasts returns an error", {
  expect_error(estimate_dispersion(list(NULL, NULL), valid_trunc_rts))
})

test_that("Output of generate_pt_nowcast_mat_list is accepted", {
  base_tri <- matrix(
    c(
      89, 54, 10, 5,
      65, 46, 21, 7,
      70, 40, 20, 5,
      80, 50, 10, 10,
      100, 40, 31, 20,
      95, 45, 21, NA,
      82, 42, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 8,
    byrow = TRUE
  )

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
  triangle3 <- matrix(
    c(
      0, 40, 20, 5,
      0, 50, 10, 10,
      0, 40, 31, NA,
      0, 45, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )

  retro_rts_list <- list(test_triangle_1, test_triangle_2, triangle3)

  pt_nowcast_list <- expect_message(
    generate_pt_nowcast_mat_list(retro_rts_list)
  )
  trunc_rep_tri_list <- truncate_triangles(base_tri)
  expect_no_error(estimate_dispersion(pt_nowcast_list, trunc_rep_tri_list))
})
