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
  make_test_triangle(data = test_triangle[1:5, ]),
  make_test_triangle(data = test_triangle[1:4, ])
)


valid_rts <- construct_triangles(valid_trunc_rts)

test_that("estimate_uncertainty: Basic functionality with valid inputs", {
  result <- estimate_uncertainty(
    point_nowcast_matrices = valid_nowcasts,
    truncated_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    n = 2
  )

  # Verify output structure
  expect_type(result, "double")
  expect_length(result, ncol(valid_nowcasts[[1]]) - 1)
  expect_true(all(is.finite(result)))
})


test_that("estimate_uncertainty can handle rolling sum with k=3", {
  result <- estimate_uncertainty(
    point_nowcast_matrices = valid_nowcasts,
    truncated_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 3, align = "right")
  )

  expect_true(all(is.finite(result)))
})


test_that("estimate_uncertainty works correctly with default and n parameters", { # nolint
  result_default <- estimate_uncertainty(
    valid_nowcasts,
    valid_trunc_rts,
    valid_rts
  )
  result_explicit <- estimate_uncertainty(valid_nowcasts,
    valid_trunc_rts,
    valid_rts,
    n = 2
  )
  expect_identical(result_default, result_explicit)
})

test_that("estimate_uncertainty: Edge cases are handled properly", {
  # Empty lists
  expect_error(estimate_uncertainty(list(), list(), list(), n = 0))

  # No NAs in truncated reporting triangles
  expect_warning(estimate_uncertainty(
    valid_nowcasts,
    lapply(valid_nowcasts, round),
    valid_rts
  ))

  # NA-filled matrices
  na_nowcasts <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  na_trunc <- list(matrix(NA, 2, 3), matrix(NA, 1, 3))
  expect_error(estimate_uncertainty(na_nowcasts, na_trunc, valid_rts, n = 2))

  # Invalid type passed, nowcasts, truncated reporting triangles, and reporting
  # triangles must be lists
  expect_error(estimate_uncertainty(
    valid_nowcasts[[1]],
    valid_trunc_rts[[1]],
    valid_rts[[1]]
  ))
})

test_that("estimate_uncertainty: Matrix dimension validation works", {
  # Mismatched dimensions between nowcasts and trunc_rts
  bad_trunc_rts <- list(
    test_triangle[1:5, ],
    test_triangle[1:3, ]
  )
  expect_error(
    estimate_uncertainty(valid_nowcasts, bad_trunc_rts, valid_rts)
  )
})



test_that("estimate_uncertainty returns an estimate if passing in a NULL for a nowcast", { # nolint
  nowcasts_with_null <- list(nowcast1, NULL)
  # This should work, using only the first nowcast and first valid_trunc_rts
  # Will warn that only the first one is being used
  result1 <- expect_warning(estimate_uncertainty(
    nowcasts_with_null,
    valid_trunc_rts,
    valid_rts
  ))
  result_to_compare <- estimate_uncertainty(
    list(nowcast1),
    list(valid_trunc_rts[[1]]),
    list(valid_rts[[1]])
  )
  expect_identical(result1, result_to_compare)
})

test_that("estimate_uncertainty returns an error if passing in only NULLs", {
  expect_error(estimate_uncertainty(list(NULL), valid_trunc_rts, valid_rts))
})

test_that("estimate_uncertainty accepts output of fill_triangles ", { # nolint
  base_tri <- make_test_triangle(data = matrix(
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
  ))

  test_triangle_1 <- make_test_triangle(data = matrix(
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
  ))

  test_triangle_2 <- make_test_triangle(data = matrix(
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
  ))
  # Triangle 3 can't be used to generate a point nowcast because first column is all zeros
  # We create it as a plain matrix and convert later with suppress
  triangle3_mat <- matrix(
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
  # Create triangle3 by suppressing the validation warning
  # This triangle is intentionally invalid for testing error handling
  triangle3 <- structure(
    triangle3_mat,
    class = c("reporting_triangle", "matrix", "array"),
    max_delay = 3L,
    reference_dates = seq(as.Date("1900-01-01"), by = "days", length.out = 5),
    delays_unit = "days"
  )
  retro_rts_list <- list(test_triangle_1, test_triangle_2, triangle3)

  pt_nowcast_list <- expect_message(
    fill_triangles(retro_rts_list)
  )
  truncated_reporting_triangles <- truncate_triangles(base_tri)
  rt_list <- construct_triangles(truncated_reporting_triangles)
  # Since only two point nowcasts are non-null, this will warn
  expect_warning(estimate_uncertainty(
    pt_nowcast_list,
    truncated_reporting_triangles,
    rt_list
  ))
})

test_that("estimate_uncertainty: Works with ragged reporting triangles", {
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
  ragged_triangle <- make_test_triangle(data = complete_triangle) |>
    construct_triangle(structure = 2)

  # Create truncated triangles and retrospective triangles
  trunc_rts <- truncate_triangles(ragged_triangle)
  retro_rts <- construct_triangles(trunc_rts, structure = 2)

  # Generate nowcasts from the ragged triangles
  retro_nowcasts <- fill_triangles(retro_rts)

  # Estimate dispersion parameters
  disp_params <- estimate_uncertainty(
    point_nowcast_matrices = retro_nowcasts,
    truncated_reporting_triangles = trunc_rts,
    retro_reporting_triangles = retro_rts,
    n = 2
  )
  n_horizons <- sum(is.na(rowSums(ragged_triangle)))
  # Test that the function returns the expected result
  expect_is(disp_params, "numeric")
  expect_length(disp_params, n_horizons)
  expect_true(all(disp_params > 0))
})

test_that("estimate_uncertainty: works as expected with perfect data", {
  set.seed(123)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
  rep_mat <- do.call(rbind, rep_mat_rows)
  triangle <- make_test_triangle(data = rep_mat) |>
    construct_triangle()
  reporting_triangle <- rbind(rep_mat, triangle)
  reporting_triangle <- make_test_triangle(data = reporting_triangle)

  pt_nowcast_mat <- fill_triangle(reporting_triangle)
  truncated_reporting_triangles <- truncate_triangles(reporting_triangle)
  retro_reporting_triangles <- construct_triangles(truncated_reporting_triangles) # nolint

  point_nowcast_matrices <- fill_triangles(retro_reporting_triangles)

  dispersion <- estimate_uncertainty(
    point_nowcast_matrices,
    truncated_reporting_triangles,
    retro_reporting_triangles
  )

  expect_equal(dispersion[1], 999, tol = 1)
  expect_equal(dispersion[2], 999, tol = 1)
  expect_equal(dispersion[3], 999, tol = 1)
})

test_that("estimate_uncertainty: works as expected with some dispersion for both ks", { # nolint
  skip_if_not_installed("zoo")
  set.seed(123)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
  rep_mat <- do.call(rbind, rep_mat_rows)
  triangle <- make_test_triangle(data = rep_mat) |>
    construct_triangle()
  reporting_triangle <- rbind(rep_mat, triangle)
  reporting_triangle <- make_test_triangle(data = reporting_triangle)


  pt_nowcast_mat <- fill_triangle(reporting_triangle)


  # in order from horizon 1 to 4, set as a high value to approximate Poisson
  disp_params <- c(500, 500, 500, 500)

  # Create a reporting triangle that is jumbled
  max_t <- nrow(reporting_triangle)
  rep_tri_new <- reporting_triangle
  for (i in seq_along(disp_params)) {
    rep_tri_new[(max_t - i + 1), 1:i] <- rnbinom(
      n = i,
      size = disp_params[i],
      mu = reporting_triangle[(max_t - i + 1), 1:i]
    )
  }
  for (i in 1:6) {
    rep_tri_new[i, ] <- rnbinom(
      n = 5,
      size = disp_params[1],
      mu = reporting_triangle[i, ]
    )
  }

  truncated_reporting_triangles <- truncate_triangles(rep_tri_new)
  retro_reporting_triangles <- construct_triangles(truncated_reporting_triangles) # nolint

  point_nowcast_matrices <- fill_triangles(retro_reporting_triangles)

  dispersion <- estimate_uncertainty(
    point_nowcast_matrices,
    truncated_reporting_triangles,
    retro_reporting_triangles
  )
  expect_lt(dispersion[1], 500)
  expect_true(all(is.finite(dispersion)))

  # Fewer reporting matrices can be included here because we are summing.
  dispersion2 <- estimate_uncertainty(
    point_nowcast_matrices[1:4],
    truncated_reporting_triangles[1:4],
    retro_reporting_triangles[1:4],
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 3, align = "right")
  )
  expect_lt(dispersion2[1], 500)
  expect_true(all(dispersion2 > 0.1))
  expect_true(all(is.finite(dispersion)))


  expect_estimates_differ(dispersion, dispersion2, tol = 0.001)

  # We'll get a warning if we are trying to use all of them
  expect_warning(
    estimate_uncertainty(
      point_nowcast_matrices,
      truncated_reporting_triangles,
      retro_reporting_triangles,
      ref_time_aggregator = function(x) zoo::rollsum(x, k = 3, align = "right")
    ),
    regexp = "Only the first 4 retrospective nowcast times were used."
  )
})

test_that("estimate_uncertainty: returns known dispersion parameters", { # nolint
  # Note, this test covers that we can approximately recover high dispersion,
  # it will not be able to distinguish between high dispersion values.
  skip_if_not_installed("zoo")
  set.seed(123)
  delay_pmf <- c(0.2, 0.2, 0.2, 0.1, 0.2)
  partial_counts <- c(500, 800, 600, 900, 800)

  # Create a complete triangle based on the known delay PMF
  rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
  rep_mat <- do.call(rbind, rep_mat_rows)
  triangle <- make_test_triangle(data = rep_mat) |>
    construct_triangle()
  reporting_triangle <- rbind(rep_mat, rep_mat, rep_mat, rep_mat, triangle)
  reporting_triangle <- make_test_triangle(data = reporting_triangle)


  pt_nowcast_mat <- fill_triangle(reporting_triangle)

  # Create truncated reporting triangles by sampling elements of triangle
  # from Poisson distribution
  max_t <- nrow(reporting_triangle)
  truncated_reporting_triangles <- list()
  point_nowcast_matrices <- list()
  retro_reporting_triangles <- list()
  disp_param <- 10000
  for (i in 1:20) {
    trunc_rep_tri_orig <- reporting_triangle[1:(max_t - i), ]
    trunc_pt_nowcast_mat <- pt_nowcast_mat[1:(max_t - i), ]
    retro_rep_tri <- construct_triangle(trunc_rep_tri_orig)
    # For the last 4 horizons, replace each row with negative binomial draws
    # with a mean of the point nowcast matrix
    trunc_rep_tri <- trunc_rep_tri_orig
    # Add uncertainty to each horizon 1:4
    for (j in 1:4) {
      max_t_loop <- nrow(trunc_rep_tri_orig)
      trunc_rep_tri[max_t_loop - j + 1, ] <- rnbinom(
        n = ncol(trunc_rep_tri_orig),
        mu = trunc_pt_nowcast_mat[max_t_loop - j + 1, ],
        size = disp_param
      )
    }
    trunc_rep_tri[is.na(trunc_rep_tri_orig)] <- NA

    truncated_reporting_triangles <- append(
      truncated_reporting_triangles,
      list(trunc_rep_tri)
    )
    point_nowcast_matrices <- append(
      point_nowcast_matrices,
      list(trunc_pt_nowcast_mat)
    )
    retro_reporting_triangles <- append(
      retro_reporting_triangles,
      list(retro_rep_tri)
    )
  }

  dispersion <- estimate_uncertainty(
    point_nowcast_matrices,
    truncated_reporting_triangles,
    retro_reporting_triangles
  )

  expect_true(all(dispersion > 700)) # Can't distinguish more specific
  # dispersion values
})

test_that("estimate_uncertainty errors when k is too large for data", {
  expect_error(
    estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      n = 2,
      ref_time_aggregator = function(x) zoo::rollmean(x, k = 8, align = "right")
    ),
    regexp = "No valid retrospective nowcast times after reference time aggregation." # nolint
  ) # nolint
})

test_that("estimate_uncertainty: can handle weekday filter with large ragged triangle", { # nolint
  skip_if_not_installed("dplyr") # Is in Suggests so CI should have installed
  skip_if_not_installed("tidyr")
  skip_if_not_installed("lubridate")

  # Use the covid data to test, using only one age group and filtering to
  # a single weekday
  covid_data <- germany_covid19_hosp |>
    dplyr::filter(
      age_group == "00+",
      lubridate::wday(reference_date) == 1
    )

  # Create a ragged triangle
  ragged_triangle <- covid_data |>
    dplyr::select(reference_date, delay, count) |>
    tidyr::pivot_wider(
      names_from = delay,
      values_from = count
    ) |>
    dplyr::select(-reference_date) |>
    as.matrix()

  short_ragged_triangle <- ragged_triangle[(nrow(ragged_triangle) - 15):nrow(ragged_triangle), ] # nolint
  short_ragged_triangle <- make_test_triangle(data = short_ragged_triangle)

  # Create truncated and retrospective reporting triangles
  trunc_rts <- truncate_triangles(short_ragged_triangle, n = 5)
  retro_rts <- construct_triangles(trunc_rts,
    structure = c(2, 7, 7, 7, 7, 7)
  )

  retro_nowcasts <- fill_triangles(retro_rts, n = 10) # Use 10 reference times

  disp_params <- estimate_uncertainty(
    point_nowcast_matrices = retro_nowcasts,
    truncated_reporting_triangles = trunc_rts,
    retro_reporting_triangles = retro_rts,
    n = 5
  )
  expect_true(all(is.finite(disp_params)))
  expect_true(all(disp_params > 0.01))
})

test_that("estimate_uncertainty: can handle weekday filter with small ragged triangle", { # nolint
  sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1)

  # Generate counts for each reference date
  counts <- c(
    150,
    160, 170, 200, 100, 400
  )

  # Create a complete triangle based on the known delay PMF and add some noise
  complete_triangle <- lapply(counts, function(x) round(x * sim_delay_pmf))
  complete_triangle <- do.call(rbind, complete_triangle)
  complete_triangle <- complete_triangle + rnbinom(length(complete_triangle),
    size = 20,
    mu = 10
  )

  # Create a reporting triangle with every other day reporting
  ragged_triangle <- make_test_triangle(data = complete_triangle) |>
    construct_triangle(structure = 2)

  # Create truncated triangles and retrospective triangles
  trunc_rts <- truncate_triangles(ragged_triangle, n = 2)
  retro_rts <- construct_triangles(trunc_rts, structure = 2)

  # Generate nowcasts from the ragged triangles
  retro_nowcasts <- fill_triangles(retro_rts, n = 4)

  # No longer errors due to ncol > nrow
  disp_params <- estimate_uncertainty(
    point_nowcast_matrices = retro_nowcasts,
    truncated_reporting_triangles = trunc_rts,
    retro_reporting_triangles = retro_rts,
    n = 2
  )

  expect_true(all(is.finite(disp_params)))
  expect_true(all(disp_params > 0.01))
})

test_that("estimate_uncertainty: errors if ref_time_aggregator isn't appropriate", { # nolint
  expect_error(
    estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      ref_time_aggregator = function(x) rowSums(x, na.rm = TRUE)
    ),
    regexp = "`ref_time_aggregator` must return a matrix with"
  )
})

test_that("estimate_uncertainty: errors when ref_time_aggregator changes column count", { # nolint
  bad_aggregator <- function(x) x[, 1:2] # Removes columns
  expect_error(
    estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      ref_time_aggregator = bad_aggregator
    ),
    "`ref_time_aggregator` must return a matrix with"
  )
})

test_that("estimate_uncertainty: errors when delay_aggregator changes row count", { # nolint
  bad_aggregator <- function(x) x[1:2, ] # Removes rows
  expect_error(
    estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      delay_aggregator = bad_aggregator
    ),
    "`delay_aggregator` must return a vector of length"
  )
})

test_that("estimate_uncertainty: errors if insufficient data", {
  skip_if_not_installed("zoo")
  expect_error(
    estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      ref_time_aggregator = function(x) zoo::rollsum(x, k = 9, align = "right")
    )
  )
})

test_that("fit_nb rejects negative observed values with clear error", {
  # Create test data with negative observations
  x_negative <- c(10, 15, -5, 20)
  mu <- c(12, 14, 8, 18)

  expect_error(
    fit_nb(x_negative, mu),
    regexp = "Negative values detected in observations"
  )

  # Error message should mention preprocessing option
  expect_error(
    fit_nb(x_negative, mu),
    regexp = "preprocess = preprocess_negative_values"
  )
})

test_that("fit_nb rejects negative predicted values with clear error", {
  # Create test data with negative predictions
  x <- c(10, 15, 5, 20)
  mu_negative <- c(12, 14, -8, 18)

  expect_error(
    fit_nb(x, mu_negative),
    regexp = "Negative values detected in predictions"
  )

  # Error message should mention preprocessing
  expect_error(
    fit_nb(x, mu_negative),
    regexp = "This may indicate an issue with the delay estimation or preprocessing" # nolint
  )
})

test_that("fit_nb works with valid non-negative inputs", {
  # Create valid test data
  x <- c(10, 15, 8, 20, 12)
  mu <- c(12, 14, 9, 18, 13)

  # Should work without error
  result <- fit_nb(x, mu)

  # Result should be a positive number (dispersion parameter)
  expect_type(result, "double")
  expect_gt(result, 0)
  expect_true(is.finite(result))
})

test_that("fit_nb handles zero values correctly", {
  # Zero values should be acceptable (not negative)
  x <- c(0, 15, 8, 20, 12)
  mu <- c(1, 14, 9, 18, 13)

  result <- fit_nb(x, mu)
  expect_type(result, "double")
  expect_gt(result, 0)
})
