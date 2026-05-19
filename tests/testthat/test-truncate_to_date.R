# Build a small reporting_triangle with known reference dates
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
rep_tri <- as_reporting_triangle(
  data = rep_tri_mat,
  reference_dates = ref_dates
)

test_that("truncate_to_date drops rows after the cutoff", {
  result <- truncate_to_date(rep_tri, reference_date = as.Date("2025-01-02"))

  expect_true(is_reporting_triangle(result))
  expect_identical(nrow(result), 2L)
  expect_identical(ncol(result), 4L)
  expect_equal(get_reference_dates(result), ref_dates[1:2]) # nolint: expect_identical_linter
})

test_that("truncate_to_date matches manual n_drop + truncate_to_row", {
  cutoff <- as.Date("2025-01-02")
  n_drop <- sum(get_reference_dates(rep_tri) > cutoff)

  expect_identical(
    truncate_to_date(rep_tri, reference_date = cutoff),
    truncate_to_row(rep_tri, t = n_drop)
  )
})

test_that("truncate_to_date is a no-op when cutoff is at or after the latest reference date", { # nolint
  expect_identical(
    truncate_to_date(rep_tri, reference_date = max(ref_dates)),
    rep_tri
  )
  expect_identical(
    truncate_to_date(rep_tri, reference_date = max(ref_dates) + 5),
    rep_tri
  )
})

test_that("truncate_to_date errors when the cutoff is before the earliest reference date", { # nolint
  expect_error(
    truncate_to_date(rep_tri, reference_date = as.Date("2024-12-01")),
    "before the earliest reference date"
  )
})

test_that("truncate_to_date validates that reference_date is a single Date", {
  expect_error(truncate_to_date(rep_tri, reference_date = "2025-01-02"))
  expect_error(truncate_to_date(rep_tri, reference_date = NA))
  expect_error(truncate_to_date(rep_tri, reference_date = ref_dates[1:2]))
})

test_that("truncate_to_date preserves class and metadata", {
  result <- truncate_to_date(rep_tri, reference_date = as.Date("2025-01-02"))

  expect_s3_class(result, "reporting_triangle")
  expect_s3_class(result, "matrix")
  expect_identical(get_max_delay(result), 3L)
  expect_identical(attr(result, "delays_unit"), "days")
  expect_identical(rownames(result), as.character(ref_dates[1:2]))
})

test_that("truncate_to_date errors with plain matrix input", {
  plain_mat <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(
    truncate_to_date(plain_mat, reference_date = as.Date("2025-01-02")),
    "data must have class 'reporting_triangle'"
  )
})
