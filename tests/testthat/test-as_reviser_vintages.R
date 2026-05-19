# Test data uses the NSSP data filtered to a manageable max_delay
data_as_of_df <- syn_nssp_df[
  syn_nssp_df$report_date <= "2026-04-01" &
    (syn_nssp_df$report_date - syn_nssp_df$reference_date) <= 25,
]

test_that(
  "as_reviser_vintages() converts reporting_triangle to reviser vintages",
  {
    skip_if_not_installed("reviser")

    rep_tri <- as_reporting_triangle(data_as_of_df)
    vintages <- as_reviser_vintages(rep_tri)

    expect_s3_class(vintages, "tbl_pubdate")
    expect_s3_class(vintages, "tbl_df")
    expect_true("time" %in% names(vintages))
    expect_identical(nrow(vintages), nrow(rep_tri))
    expect_identical(as.Date(vintages$time), get_reference_dates(rep_tri))
  }
)

test_that(
  "as_reviser_vintages() stores cumulative values across delays",
  {
    skip_if_not_installed("reviser")

    rep_tri <- as_reporting_triangle(data_as_of_df)
    vintages <- as_reviser_vintages(rep_tri)

    ref_dates <- get_reference_dates(rep_tri)
    max_delay <- get_max_delay(rep_tri)

    # For each reference date, the cumulative sum across delays should match
    # the values along the diagonal of vintages (where pub_date >= ref_date)
    for (i in seq_len(min(5L, nrow(rep_tri)))) {
      ref <- ref_dates[i]
      expected_cum <- cumsum(as.numeric(rep_tri[i, ]))
      pub_cols <- as.character(get_report_dates(
        ref,
        seq.int(0L, max_delay),
        "days"
      ))
      observed <- as.numeric(vintages[i, pub_cols])
      expect_identical(observed, expected_cum)
    }
  }
)

test_that(
  "round-trip preserves matrix, dates, structure, and NA cells",
  {
    skip_if_not_installed("reviser")

    # data_as_of_df is right-truncated, so the input triangle has NAs
    rep_tri <- as_reporting_triangle(data_as_of_df)
    expect_true(anyNA(rep_tri))

    vintages <- as_reviser_vintages(rep_tri)
    rep_tri_back <- as_reporting_triangle(data = vintages)

    expect_s3_class(rep_tri_back, "reporting_triangle")
    expect_no_error(assert_reporting_triangle(rep_tri_back))

    mat_in <- rep_tri
    dimnames(mat_in) <- NULL
    mat_back <- rep_tri_back
    dimnames(mat_back) <- NULL
    expect_identical(mat_back, mat_in)
    expect_identical(is.na(mat_back), is.na(mat_in))
    expect_identical(
      get_reference_dates(rep_tri_back),
      get_reference_dates(rep_tri)
    )
    expect_identical(ncol(rep_tri_back), ncol(rep_tri))
    expect_identical(
      attr(rep_tri_back, "delays_unit"),
      attr(rep_tri, "delays_unit")
    )
    expect_identical(
      get_reporting_structure(rep_tri_back),
      get_reporting_structure(rep_tri)
    )
  }
)

test_that(
  "round-trip works with weekly temporal units",
  {
    skip_if_not_installed("reviser")
    skip_if_not_installed("lubridate")
    skip_if_not_installed("dplyr")

    weekly_weekly <- data_as_of_df |>
      dplyr::filter( # nolint: namespace_linter
        lubridate::wday(reference_date) == 1, # nolint: namespace_linter
        lubridate::wday(report_date) == 1 # nolint: namespace_linter
      )

    rep_tri <- as_reporting_triangle(
      weekly_weekly,
      delays_unit = "weeks"
    )

    vintages <- as_reviser_vintages(rep_tri)
    # Inferred from the 7-day spacing of `time` values
    rep_tri_back <- as_reporting_triangle(data = vintages)

    expect_identical(attr(rep_tri_back, "delays_unit"), "weeks")
    mat_in <- rep_tri
    dimnames(mat_in) <- NULL
    mat_back <- rep_tri_back
    dimnames(mat_back) <- NULL
    expect_identical(mat_back, mat_in)
  }
)

test_that(
  "as_reporting_triangle.tbl_pubdate() infers daily unit even with weekly reference dates", # nolint
  {
    skip_if_not_installed("reviser")
    skip_if_not_installed("lubridate")
    skip_if_not_installed("dplyr")

    # Weekly reference dates but daily report dates
    mixed <- data_as_of_df |>
      dplyr::filter( # nolint: namespace_linter
        lubridate::wday(reference_date) == 1 # nolint: namespace_linter
      )
    rep_tri <- as_reporting_triangle(mixed, delays_unit = "days")
    vintages <- as_reviser_vintages(rep_tri)

    # Inference uses pub_date - time gaps, not time spacing, so it picks
    # "days" even though `time` values are 7 days apart.
    rep_tri_back <- as_reporting_triangle(data = vintages)
    expect_identical(attr(rep_tri_back, "delays_unit"), "days")
    mat_in <- rep_tri
    dimnames(mat_in) <- NULL
    mat_back <- rep_tri_back
    dimnames(mat_back) <- NULL
    expect_identical(mat_back, mat_in)
  }
)

test_that(
  "as_reporting_triangle.tbl_pubdate() honours an explicit delays_unit",
  {
    skip_if_not_installed("reviser")

    rep_tri <- as_reporting_triangle(data_as_of_df)
    vintages <- as_reviser_vintages(rep_tri)
    rep_tri_back <- as_reporting_triangle(data = vintages, delays_unit = "days")
    expect_identical(attr(rep_tri_back, "delays_unit"), "days")
  }
)

test_that("as_reviser_vintages() validates input", { # nolint
  skip_if_not_installed("reviser")

  expect_error(as_reviser_vintages(matrix(1:9, nrow = 3)))
  expect_error(as_reviser_vintages(list(a = 1, b = 2)))
})

test_that(
  "as_reviser_vintages() errors when reviser is not installed",
  {
    local_mocked_bindings(
      requireNamespace = function(...) FALSE,
      .package = "base"
    )
    expect_error(
      as_reviser_vintages(structure(list(), class = "reporting_triangle")),
      regexp = "reviser"
    )
  }
)

test_that(
  "as_reporting_triangle.tbl_pubdate() errors when reviser is not installed",
  {
    local_mocked_bindings(
      requireNamespace = function(...) FALSE,
      .package = "base"
    )
    dummy <- structure(
      data.frame(time = as.Date("2024-01-01")),
      class = c("tbl_pubdate", "tbl_df", "tbl", "data.frame")
    )
    expect_error(
      as_reporting_triangle.tbl_pubdate(dummy),
      regexp = "reviser"
    )
  }
)

test_that(".infer_delays_unit() infers days and weeks from gaps", {
  infer <- getFromNamespace(".infer_delays_unit", "baselinenowcast")
  t0 <- as.Date("2024-01-01")
  # Same time, delays at 0, 1, 2 days -> "days"
  expect_identical(infer(t0 + 0:2, rep(t0, 3)), "days")
  # Same time, delays at 0, 7, 14 days -> "weeks"
  expect_identical(infer(t0 + 7 * (0:2), rep(t0, 3)), "weeks")
  # Weekly times with daily delays -> "days" (the case Kaitlyn raised)
  expect_identical(
    infer(c(t0, t0 + 1, t0 + 7, t0 + 8), c(t0, t0, t0 + 7, t0 + 7)),
    "days"
  )
})

test_that(".infer_delays_unit() errors with no positive gaps", {
  infer <- getFromNamespace(".infer_delays_unit", "baselinenowcast")
  t0 <- as.Date("2024-01-01")
  expect_error(infer(rep(t0, 3), rep(t0, 3)), regexp = "no positive")
})

test_that(".infer_delays_unit() errors on unsupported gap size", {
  infer <- getFromNamespace(".infer_delays_unit", "baselinenowcast")
  t0 <- as.Date("2024-01-01")
  expect_error(
    infer(t0 + 14 * (1:2), rep(t0, 2)),
    regexp = "Cannot infer"
  )
})

test_that(".infer_delays_unit() errors on non-multiple gaps", {
  infer <- getFromNamespace(".infer_delays_unit", "baselinenowcast")
  t0 <- as.Date("2024-01-01")
  # Smallest gap is 1, but 3 is not a multiple... actually it is. Use 2 and 3.
  expect_error(
    infer(t0 + c(2, 3), rep(t0, 2)),
    regexp = "not multiples"
  )
})
