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


valid_rts <- generate_triangles(valid_trunc_rts)

test_that("estimate_dispersion: Basic functionality with valid inputs", {
  result <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2
  )

  # Verify output structure
  expect_type(result, "double")
  expect_length(result, ncol(valid_nowcasts[[1]]) - 1)
  expect_true(all(is.finite(result)))
})


test_that("estimate_dispersion can handle rolling sum with k=2", {
  result1 <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 2
  )

  result2 <- estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2
  )
  # Ensures that these are different
  expect_failure(expect_equal(result1, result2, tol = 0.01))
})

test_that("estimate_dispersion appropriately warns when k is too large for some of the triangles", { # nolint
  expect_warning(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 4
  ))
})

test_that("estimate_dispersion appropriately errors when k is too large for all the triangles", { # nolint
  expect_error(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2,
    fun_to_aggregate = sum,
    k = 5
  ))
})


test_that("estimate_dispersion throws an error if function to aggregate is not valid", { # nolint
  # Function shouldn't be a character
  expect_error(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2,
    fun_to_aggregate = "sum",
    k = 2
  ))
  # Mean doesn't work right now because we haven't added another error model
  expect_error(estimate_dispersion(
    pt_nowcast_mat_list = valid_nowcasts,
    trunc_rep_tri_list = valid_trunc_rts,
    reporting_triangle_list = valid_rts,
    n = 2,
    fun_to_aggregate = mean,
    k = 2
  ))
})

test_that("estimate_dispersion works correctly with default and n parameters", {
  result_default <- estimate_dispersion(
    valid_nowcasts,
    valid_trunc_rts,
    valid_rts
  )
  result_explicit <- estimate_dispersion(valid_nowcasts,
    valid_trunc_rts,
    valid_rts,
    n = 2
  )
  expect_identical(result_default, result_explicit)
})


test_that("estimate_dispersion: Error conditions are properly handled", {
  # Invalid input types
  expect_error(estimate_dispersion(
    list("not_a_matrix"), valid_trunc_rts, valid_rts
  ))
  expect_error(estimate_dispersion(
    valid_nowcasts, list("not_a_matrix"), valid_rts
  ))

  # Invalid reporting_triangle_list
  expect_error(estimate_dispersion(
    valid_nowcasts, valid_trunc_rts, list("not_a_matrix")
  ))
  expect_error(estimate_dispersion(valid_nowcasts, valid_trunc_rts, list()))

  # Mismatched list lengths
  expect_error(
    estimate_dispersion(valid_nowcasts[1], valid_trunc_rts, valid_rts, n = 2)
  )
  expect_error(
    estimate_dispersion(valid_nowcasts, valid_trunc_rts[1], valid_rts, n = 2)
  )
  expect_error(
    estimate_dispersion(valid_nowcasts, valid_trunc_rts, valid_rts[1], n = 2)
  )

  # Invalid n values
  expect_error(estimate_dispersion(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = -1
  ))
  expect_error(estimate_dispersion(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = 1.5
  ))
  expect_error(estimate_dispersion(
    valid_nowcasts, valid_trunc_rts, valid_rts,
    n = 3
  ))

  # pt nowcast contains NAs or is empty
  expect_error(estimate_dispersion(valid_trunc_rts, valid_trunc_rts, valid_rts))
  expect_error(estimate_dispersion(list(), valid_trunc_rts, valid_rts))

  # trunc rep mat list is empty
  expect_error(estimate_dispersion(valid_nowcasts, list(), valid_rts))

  # observations contain non-integers
  test_triangle_decimal <- test_triangle + 0.1
  non_integer_trunc_rts <- list(
    test_triangle_decimal[1:5, ],
    test_triangle_decimal[1:4, ]
  )
  expect_error(estimate_dispersion(
    valid_nowcasts, non_integer_trunc_rts, valid_rts
  ))
})


test_that("estimate_dispersion: Edge cases are handled properly", {
  # Empty lists
  expect_error(estimate_dispersion(list(), list(), list(), n = 0))

  # No NAs in truncated reporting triangles
  expect_warning(estimate_dispersion(
    valid_nowcasts,
    lapply(valid_nowcasts, round),
    valid_rts
  ))

  # NA-filled matrices
  na_nowcasts <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  na_trunc <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  expect_error(estimate_dispersion(na_nowcasts, na_trunc, valid_rts, n = 2))

  # Invalid type passed, nowcasts, truncated reporting triangles, and reporting
  # triangles must be lists
  expect_error(estimate_dispersion(
    valid_nowcasts[[1]],
    valid_trunc_rts[[1]],
    valid_rts[[1]]
  ))
})

test_that("estimate_dispersion: Matrix dimension validation works", {
  # Mismatched dimensions between nowcasts and trunc_rts
  bad_trunc_rts <- list(
    test_triangle[1:5, ],
    test_triangle[1:3, ]
  )
  expect_error(
    estimate_dispersion(valid_nowcasts, bad_trunc_rts, valid_rts)
  )
})


test_that(".fit_nb: Passing in empty vector returns NA", {
  x <- NULL
  NA_result <- .fit_nb(x, mu = 1)
  expect_true(is.na(NA_result))
})


test_that("estimate_dispersion returns an estimate if passing in a NULL for a nowcast", { # nolint
  nowcasts_with_null <- list(nowcast1, NULL)
  # This should work, using only the first nowcast and first valid_trunc_rts
  result1 <- estimate_dispersion(nowcasts_with_null, valid_trunc_rts, valid_rts)
  result_to_compare <- estimate_dispersion(
    list(nowcast1),
    list(valid_trunc_rts[[1]]),
    list(valid_rts[[1]])
  )
  expect_identical(result1, result_to_compare)
})

test_that("estimate_dispersion returns an error if passing in only NULLs", {
  expect_error(estimate_dispersion(list(NULL), valid_trunc_rts, valid_rts))
})

test_that("estimate_dispersion accepts output of generate_pt_nowcast_mat_list ", { # nolint
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
  rt_list <- generate_triangles(trunc_rep_tri_list)
  expect_no_error(estimate_dispersion(
    pt_nowcast_list,
    trunc_rep_tri_list,
    rt_list
  ))
})

test_that("estimate_dispersion: Works with ragged reporting triangles", {
  # Create a triangle with known delay PMF
  sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1)

  # Generate counts for each reference date
  counts <- c(
    30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150,
    160, 170, 180, 190, 200
  )

  # Create a complete triangle based on the known delay PMF
  complete_triangle <- lapply(counts, function(x) round(x * sim_delay_pmf))
  complete_triangle <- do.call(rbind, complete_triangle)

  # Create a reporting triangle with every other day reporting
  ragged_triangle <- generate_triangle(
    complete_triangle,
    structure = 2
  )

  # Create truncated triangles and retrospective triangles
  trunc_rts <- truncate_triangles(ragged_triangle)
  retro_rts <- generate_triangles(trunc_rts, structure = 2)

  # Generate nowcasts from the ragged triangles
  retro_nowcasts <- generate_pt_nowcast_mat_list(retro_rts)

  # Estimate dispersion parameters
  disp_params <- estimate_dispersion(
    pt_nowcast_mat_list = retro_nowcasts,
    trunc_rep_tri_list = trunc_rts,
    reporting_triangle_list = retro_rts,
    n = 2
  )

  # Test that the function returns the expected result
  expect_is(disp_params, "numeric")
  expect_length(disp_params, ncol(ragged_triangle) - 1)
  expect_true(all(disp_params > 0))
})

test_that("estimate_dispersion: works as expected with perfect data"{ #nolint
  set.seed(123)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  triangle <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle <- do.call(rbind, triangle)
  triangle <- rbind(triangle, triangle)

  pt_nowcast_mat <- generate_pt_nowcast_mat(triangle)
  trunc_rts <- truncate_triangles(triangle)
  retro_rts <- generate_triangles(trunc_rts)

  pt_nowcast_list <- generate_pt_nowcast_mat_list(retro_rts_list)

  dispersion <- estimate_dispersion(
    pt_nowcast_list,
    trunc_rep_tri_list,
    rt_list
  )

  expect_equal(dispersion[1], 999, tol = 1)
  expect_equal(dispersion[2], 999, tol = 1)
  expect_equal(dispersion[3], 999, tol = 1)

}
)
