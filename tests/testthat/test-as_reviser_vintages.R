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
  "as_reporting_triangle.tbl_pubdate() converts vintages back",
  {
    skip_if_not_installed("reviser")

    rep_tri <- as_reporting_triangle(data_as_of_df)
    vintages <- as_reviser_vintages(rep_tri)
    rep_tri_2 <- as_reporting_triangle(
      data = vintages,
      delays_unit = attr(rep_tri, "delays_unit")
    )

    expect_s3_class(rep_tri_2, "reporting_triangle")
    expect_no_error(assert_reporting_triangle(rep_tri_2))

    mat_1 <- rep_tri
    dimnames(mat_1) <- NULL
    mat_2 <- rep_tri_2
    dimnames(mat_2) <- NULL
    expect_identical(mat_2, mat_1)
    expect_identical(
      get_reference_dates(rep_tri_2),
      get_reference_dates(rep_tri)
    )
    expect_identical(ncol(rep_tri_2), ncol(rep_tri))
    expect_identical(
      attr(rep_tri_2, "delays_unit"),
      attr(rep_tri, "delays_unit")
    )
    expect_identical(
      get_reporting_structure(rep_tri_2),
      get_reporting_structure(rep_tri)
    )
  }
)

test_that(
  "as_reviser_vintages() and as_reporting_triangle.tbl_pubdate() round-trip", # nolint
  {
    skip_if_not_installed("reviser")

    rep_tri_original <- as_reporting_triangle(
      data_as_of_df,
      delays_unit = "days"
    )

    vintages <- as_reviser_vintages(rep_tri_original)
    rep_tri_final <- as_reporting_triangle(
      data = vintages,
      delays_unit = "days"
    )

    mat_orig <- rep_tri_original
    dimnames(mat_orig) <- NULL
    mat_final <- rep_tri_final
    dimnames(mat_final) <- NULL
    expect_identical(mat_final, mat_orig)
    expect_identical(
      get_reference_dates(rep_tri_final),
      get_reference_dates(rep_tri_original)
    )
    expect_identical(
      attr(rep_tri_final, "delays_unit"),
      attr(rep_tri_original, "delays_unit")
    )
    expect_identical(
      get_reporting_structure(rep_tri_final),
      get_reporting_structure(rep_tri_original)
    )
  }
)

test_that(
  "reviser vintages works with weekly temporal units",
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
    rep_tri_2 <- as_reporting_triangle(
      data = vintages,
      delays_unit = "weeks"
    )

    expect_identical(attr(rep_tri_2, "delays_unit"), "weeks")
    mat_1 <- rep_tri
    dimnames(mat_1) <- NULL
    mat_2 <- rep_tri_2
    dimnames(mat_2) <- NULL
    expect_identical(mat_2, mat_1)
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
