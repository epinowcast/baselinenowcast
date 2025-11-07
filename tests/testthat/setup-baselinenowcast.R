# Setup Script for baselinenowcast Tests
#
# This file contains shared data creation functions that are used across
# multiple test files. These functions run once before tests execute.
# Note: Only truly shared data creation should go here; test-specific
# constants should be defined at the top of individual test files.

#' Create COVID test data
#'
#' Creates a standardized COVID-19 hospitalization test dataset from the
#' germany_covid19_hosp package data. This is used across multiple test
#' files to ensure consistency.
#'
#' @param age_groups Character vector of age groups to include
#' @param add_weekday Whether to add weekday_ref_date column
#' @return Data frame with filtered COVID data
#' @keywords internal
create_covid_test_data <- function(
    age_groups = c("00+", "00-04", "60-79", "80+"),
    add_weekday = TRUE) {
  covid_data <- germany_covid19_hosp[
    germany_covid19_hosp$report_date <=
      max(germany_covid19_hosp$reference_date) &
      germany_covid19_hosp$age_group %in% age_groups,
  ]

  if (add_weekday) {
    covid_data$weekday_ref_date <- lubridate::wday(
      covid_data$reference_date,
      label = TRUE
    )
  }

  return(covid_data)
}
