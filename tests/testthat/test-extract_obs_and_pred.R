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


valid_rts <- construct_triangles(valid_trunc_rts)

test_that("extract_obs_and_pred appropriately warns when k is too large for some of the triangles", { # nolint
  expect_warning(extract_obs_and_pred(
    pt_nowcast_matrices = valid_nowcasts,
    trunc_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 4
  ))
})

test_that("extract_obs_and_pred appropriately errors when k is too large for all the triangles", { # nolint
  expect_error(extract_obs_and_pred(
    pt_nowcast_matrices = valid_nowcasts,
    trunc_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 5
  ))
})


test_that("extract_obs_and_pred throws an error if function to aggregate is not valid", { # nolint
  # Function shouldn't be a character
  expect_error(extract_obs_and_pred(
    pt_nowcast_matrices = valid_nowcasts,
    trunc_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    n = 2,
    fun_to_aggregate = "sum",
    k = 2
  ))
  # Mean doesn't work right now because we haven't added another error model
  expect_error(extract_obs_and_pred(
    pt_nowcast_matrices = valid_nowcasts,
    trunc_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    n = 2,
    fun_to_aggregate = mean,
    k = 2
  ))
})

test_that("extract_obs_and_pred works correctly with default and n parameters", { # nolint
  result_default <- extract_obs_and_pred(
    valid_nowcasts,
    valid_trunc_rts,
    valid_rts
  )
  result_explicit <- extract_obs_and_pred(valid_nowcasts,
    valid_trunc_rts,
    valid_rts,
    n = 2
  )
  expect_identical(result_default, result_explicit)
})


test_that("extract_obs_and_pred: Error conditions are properly handled", {
  # Invalid input types
  expect_error(extract_obs_and_pred(
    list("not_a_matrix"), valid_trunc_rts, valid_rts
  ))
  expect_error(extract_obs_and_pred(
    valid_nowcasts, list("not_a_matrix"), valid_rts
  ))

  # Invalid retro_reporting_triangles
  expect_error(extract_obs_and_pred(
    valid_nowcasts, valid_trunc_rts, list("not_a_matrix")
  ))
  expect_error(extract_obs_and_pred(valid_nowcasts, valid_trunc_rts, list()))

  # Mismatched list lengths
  expect_error(
    extract_obs_and_pred(valid_nowcasts[1], valid_trunc_rts, valid_rts, n = 2)
  )
  expect_error(
    extract_obs_and_pred(valid_nowcasts, valid_trunc_rts[1], valid_rts, n = 2)
  )
  expect_error(
    extract_obs_and_pred(valid_nowcasts, valid_trunc_rts, valid_rts[1], n = 2)
  )

  # Invalid n values
  expect_error(extract_obs_and_pred(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = -1
  ))
  expect_error(extract_obs_and_pred(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = 1.5
  ))
  expect_error(extract_obs_and_pred(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = 3
  ))

  # pt nowcast contains NAs or is empty
  expect_error(extract_obs_and_pred(
    valid_trunc_rts,
    valid_trunc_rts,
    valid_rts
  ))
  expect_error(extract_obs_and_pred(list(), valid_trunc_rts, valid_rts))

  # trunc rep mat list is empty
  expect_error(extract_obs_and_pred(valid_nowcasts, list(), valid_rts))

  # observations contain non-integers
  test_triangle_decimal <- test_triangle + 0.1
  non_integer_trunc_rts <- list(
    test_triangle_decimal[1:5, ],
    test_triangle_decimal[1:4, ]
  )
  expect_error(extract_obs_and_pred(
    valid_nowcasts, non_integer_trunc_rts, valid_rts
  ))
})


test_that("extract_obs_and_pred: Edge cases are handled properly", {
  # Empty lists
  expect_error(extract_obs_and_pred(list(), list(), list(), n = 0))

  # No NAs in truncated reporting triangles
  expect_warning(extract_obs_and_pred(
    valid_nowcasts,
    lapply(valid_nowcasts, round),
    valid_rts
  ))

  # NA-filled matrices
  na_nowcasts <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  na_trunc <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  expect_error(extract_obs_and_pred(na_nowcasts, na_trunc, valid_rts, n = 2))

  # Invalid type passed, nowcasts, truncated reporting triangles, and reporting
  # triangles must be lists
  expect_error(extract_obs_and_pred(
    valid_nowcasts[[1]],
    valid_trunc_rts[[1]],
    valid_rts[[1]]
  ))
})

test_that("extract_obs_and_pred: Matrix dimension validation works", {
  # Mismatched dimensions between nowcasts and trunc_rts
  bad_trunc_rts <- list(
    test_triangle[1:5, ],
    test_triangle[1:3, ]
  )
  expect_error(
    extract_obs_and_pred(valid_nowcasts, bad_trunc_rts, valid_rts)
  )
})
