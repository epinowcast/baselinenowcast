# Valid test matrix from examples
test_triangle <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)

### Test 1: Basic Functionality ------------------------------------------------
test_that("Basic functionality with default parameters", {
  result <- generate_point_nowcast(test_triangle)

  # Verify output structure
  expect_true(is.matrix(result))
  expect_identical(dim(result), dim(test_triangle))
  expect_false(anyNA(result))
})

### Test 2: Custom Delay PMF ---------------------------------------------------
test_that("Custom delay PMF is used when provided", {
  custom_pmf <- c(0.4, 0.3, 0.2, 0.1)
  result <- generate_point_nowcast(test_triangle, delay_pmf = custom_pmf)

  # Verify structure remains consistent
  expect_identical(dim(result), dim(test_triangle))
})

### Test 3: Input Validation ---------------------------------------------------
test_that("Invalid inputs throw errors", {
  # Non-matrix input
  expect_error(generate_point_nowcast(as.data.frame(test_triangle)))

  # Invalid max_delay
  expect_error(generate_point_nowcast(test_triangle, max_delay = -1))
  expect_error(generate_point_nowcast(test_triangle,
    max_delay = ncol(test_triangle) + 1
  ))

  # Invalid n values
  expect_error(generate_point_nowcast(test_triangle, n = -1))
  expect_error(generate_point_nowcast(test_triangle, n = 1.5))
})

### Test 4: Edge Cases ---------------------------------------------------------
test_that("Edge cases are handled properly", {
  # All-NA matrix (except first row)
  na_triangle <- matrix(NA, nrow = 4, ncol = 3)
  na_triangle[1, ] <- c(10, 20, 30)
  expect_error(
    generate_point_nowcast(na_triangle),
    "Reporting triangle contains NA values in elements other than"
  )
})

### Test 5: Default Parameter Values -------------------------------------------
test_that("Default parameters work as expected", {
  # Test max_delay default
  result_default <- generate_point_nowcast(test_triangle)
  result_explicit <- generate_point_nowcast(test_triangle,
    max_delay = ncol(test_triangle) - 1
  )
  expect_identical(result_default, result_explicit)

  # Test n default
  result_n_default <- generate_point_nowcast(test_triangle)
  result_n_explicit <- generate_point_nowcast(test_triangle,
    n = nrow(test_triangle)
  )
  expect_identical(result_n_default, result_n_explicit)
})

### Test 6: NA Handling --------------------------------------------------------
test_that("NA patterns are handled correctly", {
  # Matrix with strategic NAs
  strategic_na_tri <- matrix(
    c(
      30, 12, 8,
      10, 20, 12,
      15, 10, NA,
      7, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  result <- generate_point_nowcast(strategic_na_tri)
  # Verify all NAs are replaced
  expect_false(anyNA(result))
})

### Test 7: Dimension Preservation ---------------------------------------------
test_that("Output dimensions match input", {
  odd_dim_tri <- matrix(1:6, nrow = 3, ncol = 2)
  result <- generate_point_nowcast(odd_dim_tri)
  expect_equal(dim(result), c(3, 2))
})
