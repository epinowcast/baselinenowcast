# Test cases per delays_unit. Cross-month and cross-year boundaries are
# included to catch arithmetic mistakes.
delay_cases <- list(
  days = list(
    reference_dates = as.Date("2024-12-28"),
    report_dates = as.Date(c("2024-12-28", "2024-12-31", "2025-01-04")),
    expected = c(0, 3, 7)
  ),
  weeks = list(
    reference_dates = as.Date("2024-12-28"),
    report_dates = as.Date(c("2024-12-28", "2025-01-04", "2025-01-11")),
    expected = c(0, 1, 2)
  ),
  months = list(
    reference_dates = as.Date("2024-12-15"),
    report_dates = as.Date(c("2024-12-15", "2025-01-15", "2025-03-15")),
    expected = c(0, 1, 3)
  ),
  years = list(
    reference_dates = as.Date("2023-06-15"),
    report_dates = as.Date(c("2023-06-15", "2024-06-15", "2025-06-15")),
    expected = c(0, 1, 2)
  )
)

test_that("get_delays_from_dates() returns expected delays for each unit", {
  lapply(names(delay_cases), function(unit) {
    case <- delay_cases[[unit]]
    out <- get_delays_from_dates(
      report_dates = case$report_dates,
      reference_dates = rep(case$reference_dates, length(case$report_dates)),
      delays_unit = unit
    )
    return(expect_equal(out, case$expected,
      info = paste("delays_unit =", unit)
    ))
  })
})

test_that("get_delays_from_dates() inverts get_report_dates()", {
  ref <- as.Date("2024-01-15")
  lapply(c("days", "weeks", "months", "years"), function(unit) {
    delays_in <- c(0, 1, 2, 5)
    reports <- get_report_dates(
      reference_dates = rep(ref, length(delays_in)),
      delays = delays_in,
      delays_unit = unit
    )
    delays_out <- get_delays_from_dates(
      report_dates = reports,
      reference_dates = rep(ref, length(delays_in)),
      delays_unit = unit
    )
    return(expect_equal(delays_out, as.numeric(delays_in),
      info = paste("delays_unit =", unit)
    ))
  })
})

test_that("get_delays_from_dates() errors on invalid `delays_unit`", {
  expect_error(
    get_delays_from_dates(
      as.Date("2024-01-08"), as.Date("2024-01-01"), "fortnights"
    ),
    regexp = "Must be element of set"
  )
})
