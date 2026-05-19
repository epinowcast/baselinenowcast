report_date_cases <- list(
  days = list(
    reference_dates = as.Date("2024-12-28"),
    delays = c(0, 3, 7),
    expected = as.Date(c("2024-12-28", "2024-12-31", "2025-01-04"))
  ),
  weeks = list(
    reference_dates = as.Date("2024-12-28"),
    delays = c(0, 1, 2),
    expected = as.Date(c("2024-12-28", "2025-01-04", "2025-01-11"))
  ),
  months = list(
    reference_dates = as.Date("2024-12-15"),
    delays = c(0, 1, 3),
    expected = as.Date(c("2024-12-15", "2025-01-15", "2025-03-15"))
  ),
  years = list(
    reference_dates = as.Date("2023-06-15"),
    delays = c(0, 1, 2),
    expected = as.Date(c("2023-06-15", "2024-06-15", "2025-06-15"))
  )
)

test_that("get_report_dates() returns expected dates for each unit", {
  lapply(names(report_date_cases), function(unit) {
    case <- report_date_cases[[unit]]
    out <- get_report_dates(
      reference_dates = rep(case$reference_dates, length(case$delays)),
      delays = case$delays,
      delays_unit = unit
    )
    return(expect_identical(out, case$expected,
      info = paste("delays_unit =", unit)
    ))
  })
})

test_that("get_report_dates() errors on invalid `delays_unit`", {
  expect_error(
    get_report_dates(as.Date("2024-01-01"), 1, "fortnights"),
    regexp = "Must be element of set"
  )
})

test_that("get_report_dates() supports vectorised reference_dates", {
  ref <- as.Date(c("2024-01-01", "2024-06-30", "2024-12-31"))
  out <- get_report_dates(ref, delays = c(1, 1, 1), delays_unit = "days")
  expect_identical(
    out,
    as.Date(c("2024-01-02", "2024-07-01", "2025-01-01"))
  )
})
