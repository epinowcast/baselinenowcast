test_that("as_reporting_triangle_df validates required columns", {
  # Missing reference_date
  bad_data1 <- data_as_of_df
  names(bad_data1)[names(bad_data1) == "reference_date"] <- "ref_date"
  expect_error(
    as_reporting_triangle_df(bad_data1),
    "Required columns missing"
  )

  # Missing report_date
  bad_data2 <- data_as_of_df
  names(bad_data2)[names(bad_data2) == "report_date"] <- "rep_date"
  expect_error(
    as_reporting_triangle_df(bad_data2),
    "Required columns missing"
  )

  # Missing count
  bad_data3 <- data_as_of_df
  names(bad_data3)[names(bad_data3) == "count"] <- "cases"
  expect_error(
    as_reporting_triangle_df(bad_data3),
    "Required columns missing"
  )
})

test_that("as_reporting_triangle_df validates date types", {
  # reference_date not a Date
  bad_data1 <- data_as_of_df
  bad_data1$reference_date <- as.character(bad_data1$reference_date)
  expect_error(
    as_reporting_triangle_df(bad_data1),
    "Must be of class 'Date'"
  )

  # report_date not a Date
  bad_data2 <- data_as_of_df
  bad_data2$report_date <- as.character(bad_data2$report_date)
  expect_error(
    as_reporting_triangle_df(bad_data2),
    "Must be of class 'Date'"
  )
})
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that("as_reporting_triangle_df validates strata columns", {
  # Strata column doesn't exist
  expect_error(
    as_reporting_triangle_df(
      data_as_of_df,
      by = "region"
    ),
    "Column `region` doesn't exist"
  )

  # Required column in strata (should error)
  expect_error(
    as_reporting_triangle_df(
      data_as_of_df,
      by = c("reference_date", "count")
    ),
    "`by` columns cannot be among the required columns."
  )
})

test_that("as_reporting_triangle_df handles duplicate combinations correctly", {
  # Create data with duplicates
  dup_data <- rbind(data_as_of_df[1:10, ], data_as_of_df[1:10, ])

  expect_error(
    as_reporting_triangle_df(dup_data),
    "Data contains duplicate combinations"
  )
})
