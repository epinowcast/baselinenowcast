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


valid_rts <- apply_reporting_structures(valid_trunc_rts)

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


test_that("estimate_uncertainty returns numeric vector of length n_horizons", { # nolint
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
    valid_rts,
    validate = FALSE
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

test_that("estimate_uncertainty accepts output of estimate_and_apply_delays ", { # nolint
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
  # Triangle 3 can't be used to generate a point nowcast
  # because first column is all zeros
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
    estimate_and_apply_delays(retro_rts_list)
  )
  truncated_reporting_triangles <- truncate_to_rows(base_tri)
  rt_list <- apply_reporting_structures(truncated_reporting_triangles)
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
    apply_reporting_structure(structure = 2)

  # Create truncated triangles and retrospective triangles
  trunc_rts <- truncate_to_rows(ragged_triangle)
  retro_rts <- apply_reporting_structures(trunc_rts, structure = 2)

  # Generate nowcasts from the ragged triangles
  retro_nowcasts <- estimate_and_apply_delays(retro_rts)

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

test_that(
  "estimate_uncertainty returns near-zero dispersion for perfect predictions",
  {
    set.seed(123)
    delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
    partial_counts <- c(80, 100, 180, 80, 140)

    # Create a complete triangle based on the known delay PMF
    rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
    rep_mat <- do.call(rbind, rep_mat_rows)
    triangle <- make_test_triangle(data = rep_mat) |>
      apply_reporting_structure()
    reporting_triangle <- rbind(rep_mat, triangle)
    reporting_triangle <- make_test_triangle(data = reporting_triangle)

    pt_nowcast_mat <- estimate_and_apply_delay(reporting_triangle)
    truncated_reporting_triangles <- truncate_to_rows(reporting_triangle)
    retro_reporting_triangles <- apply_reporting_structures(truncated_reporting_triangles) # nolint

    point_nowcast_matrices <- estimate_and_apply_delays(
      retro_reporting_triangles
    )

    dispersion <- estimate_uncertainty(
      point_nowcast_matrices,
      truncated_reporting_triangles,
      retro_reporting_triangles
    )

    expect_equal(dispersion[1], 999, tol = 1)
    expect_equal(dispersion[2], 999, tol = 1)
    expect_equal(dispersion[3], 999, tol = 1)
  }
)

test_that("estimate_uncertainty estimates positive dispersion for noisy predictions", { # nolint
  skip_if_not_installed("zoo")
  set.seed(123)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  rep_mat_rows <- lapply(partial_counts, function(x) x * delay_pmf)
  rep_mat <- do.call(rbind, rep_mat_rows)
  triangle <- make_test_triangle(data = rep_mat) |>
    apply_reporting_structure()
  reporting_triangle <- rbind(rep_mat, triangle)
  reporting_triangle <- make_test_triangle(data = reporting_triangle)


  pt_nowcast_mat <- estimate_and_apply_delay(reporting_triangle)


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

  truncated_reporting_triangles <- truncate_to_rows(rep_tri_new)
  retro_reporting_triangles <- apply_reporting_structures(truncated_reporting_triangles) # nolint

  point_nowcast_matrices <- estimate_and_apply_delays(retro_reporting_triangles)

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
    apply_reporting_structure()
  reporting_triangle <- rbind(rep_mat, rep_mat, rep_mat, rep_mat, triangle)
  reporting_triangle <- make_test_triangle(data = reporting_triangle)


  pt_nowcast_mat <- estimate_and_apply_delay(reporting_triangle)

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
    retro_rep_tri <- apply_reporting_structure(trunc_rep_tri_orig)
    # For the last 4 horizons, replace each row with negative binomial draws
    # with a mean of the point nowcast matrix
    # Work with the underlying matrix to avoid validation issues
    trunc_rep_tri_mat <- unclass(trunc_rep_tri_orig)
    # Add uncertainty to each horizon 1:4
    for (j in 1:4) {
      max_t_loop <- nrow(trunc_rep_tri_orig)
      trunc_rep_tri_mat[max_t_loop - j + 1, ] <- rnbinom(
        n = ncol(trunc_rep_tri_orig),
        mu = trunc_pt_nowcast_mat[max_t_loop - j + 1, ],
        size = disp_param
      )
    }
    trunc_rep_tri_mat[is.na(unclass(trunc_rep_tri_orig))] <- NA

    # Reconstruct as reporting_triangle
    # Need to convert matrix to reporting_triangle first
    trunc_rep_tri <- as_reporting_triangle(
      data = trunc_rep_tri_mat,
      reference_dates = get_reference_dates(trunc_rep_tri_orig)
    )
    trunc_rep_tri <- apply_reporting_structure(trunc_rep_tri)

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
  covid_data <- covid19_data |>
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
  trunc_rts <- truncate_to_rows(short_ragged_triangle, n = 5)
  retro_rts <- apply_reporting_structures(trunc_rts,
    structure = c(2, 7, 7, 7, 7, 7)
  )

  # Use 10 reference times
  retro_nowcasts <- estimate_and_apply_delays(retro_rts, n = 10)

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
    apply_reporting_structure(structure = 2)

  # Create truncated triangles and retrospective triangles
  trunc_rts <- truncate_to_rows(ragged_triangle, n = 2)
  retro_rts <- apply_reporting_structures(trunc_rts, structure = 2)

  # Generate nowcasts from the ragged triangles
  retro_nowcasts <- estimate_and_apply_delays(retro_rts, n = 4)

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
    regexp = "preprocess_negative_values\\(\\)"
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

test_that("fit_nb returns NA for zero-length input", {
  expect_identical(fit_nb(numeric(0), numeric(0)), NA)
})

test_that("fit_nb error for negative values suggests alternative models", {
  expect_error(
    fit_nb(c(10, -5), c(12, 8)),
    regexp = "fit_normal\\(\\) or fit_skellam\\(\\)" # nolint
  )
})

test_that("fit_normal returns zero when observations equal expectations", {
  x <- c(5, 10, 15)
  expect_identical(fit_normal(x, x), 0)
})

test_that("fit_normal returns the mean squared error", {
  x <- c(1, 2, 3)
  mu <- c(0, 0, 0)
  expect_identical(fit_normal(x, mu), mean((x - mu)^2))

  # Negative observations and expectations are supported
  x_neg <- c(-1, -2, -3)
  mu_neg <- c(0, -1, 1)
  expect_identical(fit_normal(x_neg, mu_neg), mean((x_neg - mu_neg)^2))

  x_fractional <- c(0.05, -0.02, 1.35)
  mu_fractional <- c(0.03, -0.01, 1.4)
  expect_identical(
    fit_normal(x_fractional, mu_fractional),
    mean((x_fractional - mu_fractional)^2)
  )
})

test_that("fit_normal handles empty and missing inputs", {
  expect_identical(fit_normal(numeric(0), numeric(0)), NA)
  expect_identical(fit_normal(c(NA, NA), c(1, 2)), NA)

  # Pairs with a missing value are dropped
  expect_identical(fit_normal(c(1, NA, 3), c(0, 0, 0)), mean(c(1, 9)))
  expect_identical(fit_normal(c(1, 2, 3), c(0, NA, 0)), mean(c(1, 9)))
})

test_that(".dskellam_log matches the closed form of the Skellam PMF", {
  x <- 1
  lambda1 <- 3
  lambda2 <- 2
  expected <- log(
    exp(-(lambda1 + lambda2)) * (lambda1 / lambda2)^(x / 2) *
      besselI(2 * sqrt(lambda1 * lambda2), nu = abs(x))
  )
  expect_equal(.dskellam_log(x, lambda1, lambda2), expected, tolerance = 1e-10)
})

test_that(".dskellam_log returns finite log probabilities and is vectorised", {
  out <- .dskellam_log(c(-2, 0, 2), 3, 5)
  expect_length(out, 3)
  expect_true(all(is.finite(out)))
  expect_true(all(out <= 0))

  # Probabilities over a wide support sum to approximately one
  support <- -50:50
  expect_equal(sum(exp(.dskellam_log(support, 4, 2))), 1, tolerance = 1e-8)
})

test_that(".log_bessel_i matches besselI where it does not underflow", {
  z <- c(0.5, 2, 10, 100, 1000)
  nu <- c(0, 1, 3, 10, 50)
  expect_equal(
    .log_bessel_i(z, nu),
    log(besselI(z, nu = nu, expon.scaled = TRUE)) + z,
    tolerance = 1e-10
  )
})

test_that(".log_bessel_i is finite and accurate where besselI underflows", {
  # Large order relative to the argument (e.g. a large residual)
  out <- .log_bessel_i(4.7, 693)
  expect_true(is.finite(out))
  # Argument larger than besselI supports
  out_large_z <- .log_bessel_i(2e5, 5)
  expect_true(is.finite(out_large_z))
  # The asymptotic expansion is accurate near the switch over
  expect_equal(
    .log_bessel_i(9e4, 5),
    log(besselI(9e4, nu = 5, expon.scaled = TRUE)) + 9e4,
    tolerance = 1e-8
  )
  expect_silent(.log_bessel_i(c(4.7, 2), c(693, 1)))
})

test_that("fit_skellam converges with large counts and outliers", {
  # A single large residual underflows besselI(), which previously made the
  # likelihood infinite for every candidate variance
  x <- c(693, 5, 12, 0, 3)
  mu <- c(10.4, 4.2, 9.8, 1.2, 2.5)
  result <- expect_no_error(fit_skellam(x, mu))
  expect_true(is.finite(result))
  expect_gt(result, max(abs(mu)))

  # Large counts
  set.seed(42)
  x_large <- rpois(200, 60000) - rpois(200, 50000)
  result_large <- fit_skellam(x_large, rep(10000, 200))
  expect_equal(result_large, 110000, tolerance = 0.05)
})

test_that("fit_skellam recovers the variance of simulated data", {
  set.seed(42)
  n <- 1000
  x <- rpois(n, 3) - rpois(n, 1)
  mu <- rep(2, n)
  expect_equal(fit_skellam(x, mu), 4, tolerance = 0.05)
  expect_equal(fit_skellam(x, mu, method = "mom"), 4, tolerance = 0.05)
})

test_that("fit_skellam method of moments matches fit_normal", {
  x <- c(-2L, 1L, 3L, 0L, -1L)
  mu <- c(-1.5, 0.8, 2.5, 0.2, -0.5)
  expect_identical(fit_skellam(x, mu, method = "mom"), fit_normal(x, mu))
})

test_that("fit_skellam returns a variance greater than max(|mu|)", {
  x <- c(10, 12, 8)
  mu <- c(9, 11, 7)
  result <- fit_skellam(x, mu)
  expect_gt(result, max(abs(mu)))

  # The fitted variance gives valid Poisson rates for the expectations
  expect_true(all((mu + result) / 2 > 0))
  expect_true(all((result - mu) / 2 > 0))
})

test_that("fit_skellam handles all negative observations", {
  result <- fit_skellam(c(-3, -5, -1, -4), c(-2, -4, -2, -3))
  expect_true(is.finite(result))
  expect_gt(result, 0)
})

test_that("fit_skellam handles empty and missing inputs", {
  expect_identical(fit_skellam(numeric(0), numeric(0)), NA)
  expect_identical(fit_skellam(c(NA, NA), c(1, 2)), NA)
  expect_identical(
    fit_skellam(c(1, NA, 3), c(0, 0, 0), method = "mom"),
    mean(c(1, 9))
  )
})

test_that("fit_skellam rejects invalid inputs", {
  expect_error(fit_skellam(c(1.5, 2), c(1, 2)), regexp = "integerish")
  expect_error(fit_skellam(c(1, 2), c(1, 2), method = "bayes"),
    regexp = "method"
  )
})

test_that("estimate_uncertainty works with normal and Skellam models", {
  for (fit_model in list(fit_normal, fit_skellam)) {
    result <- estimate_uncertainty(
      point_nowcast_matrices = valid_nowcasts,
      truncated_reporting_triangles = valid_trunc_rts,
      retro_reporting_triangles = valid_rts,
      uncertainty_model = fit_by_horizon_with(fit_model)
    )
    expect_type(result, "double")
    expect_length(result, ncol(valid_nowcasts[[1]]) - 1)
    expect_true(all(is.finite(result)))
    expect_true(all(result >= 0))
  }
})

test_that("estimate_uncertainty supports downward corrections with normal and Skellam models", { # nolint
  # Negative counts at delay 2 from downward corrections
  test_triangle_neg <- test_triangle
  test_triangle_neg[, 3] <- -test_triangle_neg[, 3]
  nowcast1_neg <- nowcast1
  nowcast1_neg[, 3] <- -nowcast1_neg[, 3]
  nowcast2_neg <- nowcast2
  nowcast2_neg[, 3] <- -nowcast2_neg[, 3]
  trunc_rts_neg <- list(
    make_test_triangle(data = test_triangle_neg[1:5, ]),
    make_test_triangle(data = test_triangle_neg[1:4, ])
  )
  neg_args <- list(
    point_nowcast_matrices = list(nowcast1_neg, nowcast2_neg),
    truncated_reporting_triangles = trunc_rts_neg,
    retro_reporting_triangles = apply_reporting_structures(trunc_rts_neg)
  )

  expect_error(
    do.call(estimate_uncertainty, neg_args),
    regexp = "Negative values detected"
  )

  for (fit_model in list(fit_normal, fit_skellam)) {
    args <- c(
      neg_args,
      list(uncertainty_model = fit_by_horizon_with(fit_model))
    )
    result <- do.call(estimate_uncertainty, args)
    expect_length(result, ncol(nowcast1_neg) - 1)
    expect_true(all(is.finite(result)))
    expect_true(all(result >= 0))
  }
})

test_that("estimate_uncertainty rejects malformed inputs", {
  nowcasts_with_na <- valid_nowcasts
  nowcasts_with_na[[1]][1, 1] <- NA

  mismatched_nowcasts <- list(
    nowcast1[, 1:2],
    nowcast2[, 1:2]
  )

  cases <- list(
    list(
      args = list(point_nowcast_matrices = "not a list"),
      regex = "must be a list"
    ),
    list(
      args = list(
        point_nowcast_matrices = list(nowcast1),
        n = 5
      ),
      regex = "Insufficient elements"
    ),
    list(
      args = list(point_nowcast_matrices = nowcasts_with_na),
      regex = "contains NAs"
    ),
    list(
      args = list(point_nowcast_matrices = mismatched_nowcasts),
      regex = "Dimensions of the first"
    ),
    list(
      args = list(ref_time_aggregator = function(x) {
        return(matrix(as.character(x), nrow = nrow(x)))
      }),
      regex = "must return a numeric matrix"
    ),
    list(
      args = list(delay_aggregator = function(x) {
        return(rep("a", nrow(x)))
      }),
      regex = "must return a numeric vector"
    )
  )

  base_args <- list(
    point_nowcast_matrices = valid_nowcasts,
    truncated_reporting_triangles = valid_trunc_rts,
    retro_reporting_triangles = valid_rts
  )

  for (case in cases) {
    args <- base_args
    for (k in names(case$args)) args[[k]] <- case$args[[k]]
    expect_error(do.call(estimate_uncertainty, args), regexp = case$regex)
  }
})
