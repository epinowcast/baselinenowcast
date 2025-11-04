test_that("estimate_uncertainty_parameters works with default parameters", {
  triangle <- matrix(
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

  result <- estimate_uncertainty_parameters(triangle)

  expect_type(result, "double")
  expect_true(length(result) > 0)
  expect_true(all(result > 0))
})

test_that("estimate_uncertainty_parameters matches manual workflow", {
  triangle <- matrix(
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

  # Manual workflow
  n_retro <- 2
  max_delay <- ncol(triangle) - 1
  structure <- 1

  trunc_rep_tri_list <- truncate_triangles(triangle, n = n_retro)
  reporting_triangle_list <- construct_triangles(
    trunc_rep_tri_list,
    structure = structure
  )
  n_delay <- min(sapply(reporting_triangle_list, nrow))
  pt_nowcast_mat_list <- fill_triangles(
    reporting_triangle_list,
    max_delay = max_delay,
    n = n_delay
  )
  manual_result <- estimate_uncertainty(
    point_nowcast_matrices = pt_nowcast_mat_list,
    truncated_reporting_triangles = trunc_rep_tri_list,
    retro_reporting_triangles = reporting_triangle_list,
    n = n_retro
  )

  wrapper_result <- estimate_uncertainty_parameters(triangle)

  expect_identical(wrapper_result, manual_result)
})

test_that("estimate_uncertainty_parameters works with custom n_delay", {
  triangle <- matrix(
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

  result <- estimate_uncertainty_parameters(triangle, n_delay = 5)

  expect_type(result, "double")
  expect_true(length(result) > 0)
})

test_that("estimate_uncertainty_parameters works with custom n_retro", {
  triangle <- matrix(
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

  suppressWarnings({
    result <- estimate_uncertainty_parameters(triangle, n_retro = 3)
  })

  expect_type(result, "double")
  expect_true(length(result) > 0)
})

test_that("estimate_uncertainty_parameters works with custom max_delay", {
  triangle <- matrix(
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

  result <- estimate_uncertainty_parameters(triangle, max_delay = 3)

  expect_type(result, "double")
  expect_true(length(result) > 0)
})

test_that("estimate_uncertainty_parameters works with custom delay_pmf", {
  triangle <- matrix(
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

  custom_pmf <- c(0.4, 0.3, 0.2, 0.1)

  result <- estimate_uncertainty_parameters(triangle, delay_pmf = custom_pmf)

  expect_type(result, "double")
  expect_true(length(result) > 0)
})

test_that("estimate_uncertainty_parameters works with custom aggregators", {
  triangle <- matrix(
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

  if (requireNamespace("zoo", quietly = TRUE)) {
    result <- estimate_uncertainty_parameters(
      triangle,
      ref_time_aggregator = function(x) {
        zoo::rollsum(x, k = 2, align = "right")
      }
    )

    expect_type(result, "double")
    expect_true(length(result) > 0)
  } else {
    skip("zoo package not available")
  }
})

test_that(
  "estimate_uncertainty_parameters works with custom structure parameter",
  {
    triangle <- matrix(
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

    result <- estimate_uncertainty_parameters(triangle, structure = 2)

    expect_type(result, "double")
    expect_true(length(result) > 0)
  }
)

test_that("estimate_uncertainty_parameters validates triangle input", {
  expect_error(
    estimate_uncertainty_parameters(data.frame(a = 1:5, b = 6:10)),
    "matrix"
  )

  invalid_triangle <- matrix(
    c(10, -5, NA, 8, NA, NA),
    nrow = 2,
    byrow = TRUE
  )
  expect_error(
    estimate_uncertainty_parameters(invalid_triangle)
  )
})

test_that("estimate_uncertainty_parameters validates integer parameters", {
  triangle <- matrix(
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

  expect_error(
    estimate_uncertainty_parameters(triangle, n_delay = -1),
    "n_delay"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, n_retro = 0),
    "n_retro"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, max_delay = -1),
    "max_delay"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, structure = 0),
    "structure"
  )
})

test_that("estimate_uncertainty_parameters validates delay_pmf", {
  triangle <- matrix(
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

  expect_error(
    estimate_uncertainty_parameters(triangle, delay_pmf = c(0.3, 0.4, 0.2)),
    "sum to 1"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, delay_pmf = c(0.5, -0.2, 0.7)),
    "delay_pmf"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, delay_pmf = c(0.5, 1.5, -0.2)),
    "delay_pmf"
  )
})

test_that("estimate_uncertainty_parameters validates function arguments", {
  triangle <- matrix(
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

  expect_error(
    estimate_uncertainty_parameters(triangle, ref_time_aggregator = "mean"),
    "ref_time_aggregator"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, delay_aggregator = "sum"),
    "delay_aggregator"
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, uncertainty_model = "nb"),
    "uncertainty_model"
  )
})

test_that(
  "estimate_uncertainty_parameters works with all custom parameters",
  {
    triangle <- matrix(
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

    result <- estimate_uncertainty_parameters(
      reporting_triangle = triangle,
      n_delay = 5,
      n_retro = 2,
      max_delay = 3,
      delay_pmf = c(0.4, 0.3, 0.2, 0.1),
      ref_time_aggregator = identity,
      delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
      structure = 1,
      uncertainty_model = fit_by_horizon
    )

    expect_type(result, "double")
    expect_true(length(result) > 0)
  }
)

test_that("estimate_uncertainty_parameters errors when all nowcasts fail", {
  triangle <- matrix(
    c(
      10, 5, 2, 1,
      8, 6, 3, NA,
      12, 7, NA, NA,
      15, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  expect_error(
    estimate_uncertainty_parameters(triangle, n_retro = 1, n_delay = 2),
    "Errors occurred in all"
  )
})

test_that("estimate_uncertainty_parameters handles partial failures", {
  triangle <- matrix(
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

  suppressWarnings({
    result <- estimate_uncertainty_parameters(triangle, n_retro = 3)
  })

  expect_type(result, "double")
  expect_true(length(result) > 0)
})
