# Valid test matrix from examples
test_triangle <- make_test_triangle(data = matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
))

test_that("fill_triangle is deprecated with warning", {

  # Reset the lifecycle verbosity to ensure warning is shown
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    fill_triangle(test_triangle),
    regexp = "deprecated"
  )
  expect_warning(
    fill_triangle(test_triangle),
    regexp = "estimate_and_apply_delay"
  )
})

test_that("fill_triangle basic functionality with default parameters", { # nolint
  result <- suppressWarnings(fill_triangle(test_triangle))

  # Verify output structure
  expect_true(is.matrix(result))
  expect_identical(dim(result), dim(test_triangle))
  expect_false(anyNA(result))
})

test_that("fill_triangle custom delay PMF is used when provided", {
  custom_pmf <- c(0.4, 0.3, 0.2, 0.1)
  result <- suppressWarnings(
    fill_triangle(test_triangle, delay_pmf = custom_pmf)
  )

  # Verify structure remains consistent
  expect_identical(dim(result), dim(test_triangle))
})

test_that("fill_triangle invalid inputs throw errors", {
  # Non-matrix input
  expect_error(suppressWarnings(fill_triangle(as.data.frame(test_triangle))))

  # Invalid n values
  expect_error(suppressWarnings(fill_triangle(test_triangle, n = -1)))
  expect_error(suppressWarnings(fill_triangle(test_triangle, n = 1.5)))
})

test_that("fill_triangle default parameters work as expected", {
  # Test that fill_triangle works with defaults
  result_default <- suppressWarnings(fill_triangle(test_triangle))

  # Test n default
  result_n_default <- suppressWarnings(fill_triangle(test_triangle))
  result_n_explicit <- suppressWarnings(fill_triangle(test_triangle,
    n = nrow(test_triangle)
  ))
  expect_identical(result_n_default, result_n_explicit)
})

test_that("fill_triangle NA patterns are handled correctly", {
  # Matrix with strategic NAs
  strategic_na_tri <- make_test_triangle(data = matrix(
    c(
      30, 12, 8,
      10, 20, 12,
      15, 10, NA,
      7, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  ))

  result <- suppressWarnings(fill_triangle(strategic_na_tri))
  # Verify all NAs are replaced
  expect_false(anyNA(result))
})

test_that("fill_triangle: Output dimensions match input", {
  odd_dim_tri <- make_test_triangle(data = matrix(1:6, nrow = 3, ncol = 2)) |>
    apply_reporting_structure()
  result <- suppressWarnings(fill_triangle(odd_dim_tri))
  expect_identical(dim(result), c(3L, 2L))
})

test_that("fill_triangle errors when n is incompatible", {
  # Custom n_history_delay is too high
  expect_error(
    suppressWarnings(fill_triangle(
      test_triangle,
      n = 8
    ))
  )
  # Custom n_history_delay is too low
  expect_error(
    suppressWarnings(fill_triangle(
      test_triangle,
      n = 3
    ))
  )
})

test_that("fill_triangle can take in another delay PMF", {
  external_delay_pmf <- c(0.1, 0.1, 0.5, 0.3)
  result <- suppressWarnings(fill_triangle(
    test_triangle,
    delay_pmf = external_delay_pmf
  ))
  last_row <- result[5, ]
  pmf <- last_row / sum(last_row)
  expect_equal(unname(pmf), unname(external_delay_pmf), tolerance = 0.01)
})

test_that("fill_triangle generates the correct result on a ragged triangle", { # nolint
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  triangle_mat <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle_mat <- do.call(rbind, triangle_mat)
  triangle <- make_test_triangle(data = triangle_mat)
  triangle <- apply_reporting_structure(triangle, structure = c(1, 2))

  result <- suppressWarnings(fill_triangle(
    triangle
  ))
  cols <- colSums(result[3:5, ])
  pmf <- cols / sum(cols)
  expect_equal(unname(pmf), unname(delay_pmf), tolerance = 0.01)
})

test_that("fill_triangle generates the correct result on a ragged triangle with fewer rows than columns", { # nolint
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100, 180)

  # Create a complete triangle based on the known delay PMF
  triangle_mat <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle_mat <- do.call(rbind, triangle_mat)
  triangle <- make_test_triangle(data = triangle_mat)
  triangle <- apply_reporting_structure(triangle, structure = c(1, 2))

  result <- suppressWarnings(fill_triangle(
    triangle
  ))
  cols <- colSums(result[2:3, ])
  pmf <- cols / sum(cols)
  expect_equal(unname(pmf), unname(delay_pmf), tolerance = 0.01)
})

test_that("fill_triangle errors when there are insufficient observations", { # nolint
  delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100)

  # Create a complete triangle based on the known delay PMF
  triangle_mat <- lapply(partial_counts, function(x) x * delay_pmf)
  triangle_mat <- do.call(rbind, triangle_mat)

  # apply_reporting_structure creates all-NA columns which should fail
  # validation when passed to fill_triangle
  triangle <- make_test_triangle(data = triangle_mat) |>
    apply_reporting_structure(structure = c(1, 2))

  # fill_triangle should error because no rows have complete observations
  expect_error(
    suppressWarnings(fill_triangle(triangle)),
    regexp = "at least one row with no missing observations"
  )
})
