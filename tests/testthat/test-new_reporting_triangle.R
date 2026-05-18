# Helper used only in this file
make_valid_matrix <- function(nrow = 3, ncol = 4) {
  return(matrix(seq_len(nrow * ncol), nrow = nrow, ncol = ncol))
}

test_that("new_reporting_triangle() builds a valid reporting_triangle", {
  ref_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  mat <- make_valid_matrix(nrow = 3, ncol = 4)

  rt <- new_reporting_triangle(
    reporting_triangle_matrix = mat,
    reference_dates = ref_dates,
    delays_unit = "days"
  )

  expect_true(is_reporting_triangle(rt))
  expect_true(is.matrix(rt))
  expect_identical(dim(rt), dim(mat))
  expect_identical(get_delays_unit(rt), "days")
  expect_identical(get_reference_dates(rt), ref_dates)
  expect_identical(colnames(rt), as.character(0:3))
  expect_identical(rownames(rt), as.character(ref_dates))
  # Underlying values round-trip
  expect_identical(as.numeric(rt), as.numeric(mat))
})

test_that("new_reporting_triangle() attributes round-trip across units", {
  ref_dates <- as.Date(c("2024-01-01", "2024-01-08"))
  mat <- make_valid_matrix(nrow = 2, ncol = 3)

  lapply(c("days", "weeks", "months", "years"), function(unit) {
    tri <- new_reporting_triangle(mat, ref_dates, delays_unit = unit)
    expect_identical(get_delays_unit(tri), unit)
    return(expect_identical(get_reference_dates(tri), ref_dates))
  })
})

test_that("new_reporting_triangle() errors on duplicate reference_dates", {
  mat <- make_valid_matrix(nrow = 2, ncol = 3)
  dup_dates <- as.Date(c("2024-01-01", "2024-01-01"))
  expect_error(
    new_reporting_triangle(mat, dup_dates, "days"),
    regexp = "duplicated values"
  )
})

test_that("new_reporting_triangle() errors on zero-row matrix", {
  empty_mat <- matrix(numeric(0), nrow = 0, ncol = 4)
  expect_error(
    new_reporting_triangle(empty_mat, as.Date(character(0)), "days"),
    regexp = "Must have length >= 1"
  )
})

test_that("new_reporting_triangle() errors on invalid `delays_unit`", {
  mat <- make_valid_matrix(nrow = 2, ncol = 3)
  ref_dates <- as.Date(c("2024-01-01", "2024-01-02"))
  expect_error(
    new_reporting_triangle(mat, ref_dates, delays_unit = "fortnights"),
    regexp = "Must be element of set"
  )
})

test_that("new_reporting_triangle() errors on date length mismatch", {
  mat <- make_valid_matrix(nrow = 3, ncol = 4)
  expect_error(
    new_reporting_triangle(
      mat, as.Date(c("2024-01-01", "2024-01-02")), "days"
    ),
    regexp = "length"
  )
})
