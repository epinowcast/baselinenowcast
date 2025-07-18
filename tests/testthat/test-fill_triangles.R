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

test_that("fill_triangles returns correctly structured output", {
  result <- fill_triangles(
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

test_that("fill_triangles takes in delay_pmf as vector or list", {
  result1 <- fill_triangles(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = delay_pmf
  )

  result2 <- fill_triangles(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = rep(list(delay_pmf), length(retro_rts_list))
  )

  expect_identical(result1, result2)

  expect_error(fill_triangles(
    reporting_triangle_list = retro_rts_list,
    delay_pmf = rep(list(delay_pmf), length(retro_rts_list) - 1)
  ))
})

test_that(
  "fill_triangles default n_history_delay uses minimum rows",
  {
    # Input matrices have 7 and 7 rows → min = 6
    result_default <- fill_triangles(
      reporting_triangle_list = retro_rts_list
    )
    result_custom_w_def <- fill_triangles(
      reporting_triangle_list = retro_rts_list,
      n = 6
    )
    result_custom <- fill_triangles(
      reporting_triangle_list = retro_rts_list,
      n = 5
    )

    # Ensure default matches explicit use of min rows
    expect_identical(result_default, result_custom_w_def)
    # Because estimates are made from different observations, expect these
    # not to be identical
    expect_false(all(result_default[[1]] == result_custom[[1]]))
  }
)

test_that("fill_triangles custom n_history_delay is respected", {
  result <- fill_triangles(
    reporting_triangle_list = retro_rts_list,
    n = 5
  )
  # Compare to estimate using n=5
  first_triangle <- result[[1]]
  delay_pmf <- estimate_delay(retro_rts_list[[1]],
    n = 5
  )
  exp_first_triangle <- apply_delay(retro_rts_list[[1]], delay_pmf)
  expect_equal(first_triangle, exp_first_triangle, tol = 0.0001)

  # Custom n_history_delay is too high
  expect_error(
    fill_triangles(
      reporting_triangle_list = retro_rts_list,
      n = 8
    )
  ) # nolint
  # Custom n_history_delay is too low
  expect_error(
    fill_triangles(
      reporting_triangle_list = retro_rts_list,
      n = 3
    )
  )
})

test_that("fill_triangles invalid inputs throw errors", {
  # Non-list input
  expect_error(fill_triangles(
    reporting_triangle_list = "not_a_list"
  ))

  # List contains non-matrix elements
  bad_list <- list(test_triangle_1, "not_a_matrix")
  expect_error(fill_triangles(
    reporting_triangle_matrix_list = bad_list
  ))

  # Invalid n_history_delay values
  expect_error(fill_triangles(retro_rts_list, n = -1))
  expect_error(fill_triangles(retro_rts_list, n = "two"))
})

test_that("fill_triangles identical-sized matrices work", {
  same_size_list <- list(test_triangle_1[2:7, ], test_triangle_2)
  result <- fill_triangles(same_size_list)

  # Number of rows of each matrix should be identical (6 and 6)
  expect_identical(sapply(result, nrow), c(6L, 6L))
})

test_that("fill_triangles handles a single triangle with 0s for first column appropriately", { # nolint

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

  result <- expect_message(fill_triangles(retro_rts_list))
  expect_null(result[[3]])
})

test_that("fill_triangles errors if only contains triangles with first column 0", { # nolint
  triangle1 <- matrix(
    c(
      0, 46, 21, 7,
      0, 40, 20, 5,
      0, 50, 10, 10,
      0, 40, 31, 20,
      0, 45, 21, NA,
      0, 42, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 7,
    byrow = TRUE
  )

  triangle2 <- matrix(
    c(
      0, 46, 21, 7,
      0, 40, 20, 5,
      0, 50, 10, 10,
      0, 40, 31, NA,
      0, 45, NA, NA,
      0, NA, NA, NA
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

  retro_rts_list <- list(triangle1, triangle2, triangle3)

  expect_error(expect_message(fill_triangles(retro_rts_list)))
})

test_that("fill_triangles uses full number of rows in n_history_delay", { # nolint
  sim_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)

  # Generate counts for each reference date
  counts <- c(100, 150, 200, 250, 300, 100, 90)

  # Create a complete triangle based on the known delay PMF
  complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
  complete_triangle <- do.call(rbind, complete_triangle)

  check_pmf <- estimate_delay(complete_triangle, n = 7)
  check_pmf

  # Create a reporting triangle with NAs in the lower right
  triangle <- generate_triangle(complete_triangle)
  triangle

  slight_dif_triangle <- fill_triangle(triangle, n = 7)

  expect_equal(slight_dif_triangle, complete_triangle, tol = 0.5)

  # Change entry in first row so that when used it wont estimate same delay
  triangle[1, 4] <- 3 * triangle[1, 4]
  triangle


  truncated_rts <- truncate_triangles(triangle, n = 2)
  truncated_rts[1:2]
  # These will always have the first row at the top. First one will be with
  # last row cut off, second will be with last 2 rows cut off

  retro_rts <- generate_triangles(truncated_rts)
  retro_rts[1:2]
  # These look the same but with NAs in bottom right

  retro_pt_nowcast_mat_list <- fill_triangles(
    reporting_triangle_list = retro_rts,
    n = 5
  )
  # Get the empirical pmfs in your two pt nowcast matrices with the first row
  # included
  pmf_list <- lapply(retro_pt_nowcast_mat_list, estimate_delay)
  expect_equal(pmf_list[[1]], sim_delay_pmf, tol = 0.02) # Here we get back
  # what we put in bc it doesn't use the first row
  expect_failure(expect_equal(pmf_list[[2]], sim_delay_pmf, tol = 0.02)) # Here
  # we don't bc we use the first row

  # Now change n_history_delay to only use last 4 rows, we should never use
  # first row so both will be equal
  retro_pt_nowcast_mat_list2 <- fill_triangles(
    reporting_triangle_list = retro_rts,
    n = 4
  )

  pmf_list2 <- lapply(retro_pt_nowcast_mat_list2, estimate_delay)
  # Both are returning the original pmf bc they dont use the modified one
  expect_equal(pmf_list2[[1]], sim_delay_pmf, tol = 0.02)
  expect_equal(pmf_list2[[2]], sim_delay_pmf, tol = 0.02)
})
