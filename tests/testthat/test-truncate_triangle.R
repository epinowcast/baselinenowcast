# Example matrix for testing
rep_tri <- make_test_triangle(data = matrix(
  c(
    10, 20, 30,
    40, 50, NA,
    70, NA, NA
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
))

# Test 1: Basic functionality
test_that("truncate_triangle works with positive t", {
  t <- 1
  result <- truncate_triangle(t, rep_tri)
  expected_mat <- matrix(
    c(
      10, 20, 30,
      40, 50, NA
    ),
    nrow = 2,
    ncol = 3,
    byrow = TRUE
  )
  # Compare matrix values (strip attributes)
  result_mat <- unclass(result)
  attributes(result_mat) <- list(dim = dim(result_mat))
  expect_identical(result_mat, expected_mat)
  # Check it's a reporting_triangle
  expect_true(is_reporting_triangle(result))
  # Check structure is preserved from original
  expect_identical(attr(result, "structure"), attr(rep_tri, "structure"))
})

# Test 2: Edge case with t equal to nrow(rep_tri) fails
test_that("truncate_triangle throws an error when t is too large", {
  t <- nrow(rep_tri)
  expect_error(
    truncate_triangle(t, rep_tri),
    "The as of time point is greater than or equal to the number of"
  )
})

# Test 3: Negative t
test_that("truncate_triangle throws an error for a negative t", {
  t <- -1
  expect_error(
    truncate_triangle(t, rep_tri)
  )
})

# Test 4: Non-integer t
test_that("truncate_triangle throws an error for a non-integer t", {
  t <- 1.5
  expect_error(truncate_triangle(t, rep_tri))
})

# Test 5: Zero t
test_that("truncate_triangle handles zero t", {
  t <- 0L
  result <- truncate_triangle(t, rep_tri)
  expect_identical(result, rep_tri)
})

# Test 6: Empty matrix input throws an error
test_that("truncate_triangle handles empty matrix input", {
  t <- 1
  expect_error(
    make_test_triangle(data = matrix(nrow = 0, ncol = 0)),
    "Contains only missing values"
  )
})

# Test 7: Preserves reporting_triangle class
test_that("truncate_triangle preserves reporting_triangle class", {
  rep_tri_mat <- matrix(
    c(
      100, 50, 25, 10,
      80, 40, 20, NA,
      90, 45, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 4)
  rep_tri_obj <- as_reporting_triangle(
    data = rep_tri_mat,
    reference_dates = ref_dates,
    max_delay = 3
  )

  # Truncate by 1 row
  result <- truncate_triangle(t = 1, reporting_triangle = rep_tri_obj)

  # Check class is preserved
  expect_true(is_reporting_triangle(result))
  expect_s3_class(result, "reporting_triangle")
  expect_s3_class(result, "matrix")

  # Check dimensions
  expect_identical(nrow(result), 3L)
  expect_identical(ncol(result), 4L)

  # Check metadata is preserved
  expect_identical(get_max_delay(result), 3L)
  expect_identical(attr(result, "delays_unit"), "days")

  # Check reference dates are updated correctly
  result_dates <- get_reference_dates(result)
  expect_identical(length(result_dates), 3L)
  expect_equal(result_dates, ref_dates[1:3])

  # Check rownames are set correctly
  expect_identical(rownames(result), as.character(ref_dates[1:3]))
})

# Test 8: Plain matrix input errors
test_that("truncate_triangle errors with plain matrix input", {
  plain_mat <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(
    truncate_triangle(t = 1, reporting_triangle = plain_mat),
    "data must have class 'reporting_triangle'"
  )
})
