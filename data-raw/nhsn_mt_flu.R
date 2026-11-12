# Script to generate the Montana NHSN influenza hospital admissions dataset,
# which contains downward revisions (negative counts).
# Run this script when the data needs to be updated.
#
# Data are the preliminary weekly NHSN Hospital Respiratory Data (HRD)
# confirmed influenza admissions for Montana, as archived by the Delphi
# Epidata API (https://cmu-delphi.github.io/delphi-epidata/). Each issue
# (report) contains cumulative counts by reference week, so differencing
# successive issues gives the incident counts by reference and report date.
# Decreases between issues (downward revisions) give negative counts.
library(readr)
library(dplyr)

# Saturday ending an MMWR epiweek in YYYYWW format. Week 1 is the
# Sunday-to-Saturday week that contains 4th January.
epiweek_to_saturday <- function(epiweek) {
  year <- epiweek %/% 100
  week <- epiweek %% 100
  jan4 <- as.Date(paste0(year, "-01-04"))
  week1_sunday <- jan4 - as.integer(format(jan4, "%w"))
  return(week1_sunday + 7 * (week - 1) + 6)
}

# The Delphi archive of preliminary NHSN data starts at issue 2024 week 47,
# so start from the first reference week whose full revision history is
# available (2024 week 46, first reported in 2024 week 47).
# nolint start: nonportable_path_linter
delphi_url <- paste0(
  "https://api.delphi.cmu.edu/epidata/covidcast/",
  "?data_source=nhsn",
  "&signals=confirmed_admissions_flu_ew_prelim",
  "&geo_type=state&time_type=week",
  "&geo_values=mt",
  "&time_values=202446-202535",
  "&issues=202447-202545",
  "&format=csv"
)
# nolint end
raw_data <- read_csv(delphi_url)

nhsn_mt_flu_df <- raw_data |>
  arrange(time_value, issue) |>
  group_by(time_value) |>
  mutate(count = value - lag(value, default = 0)) |>
  ungroup() |>
  mutate(
    reference_date = epiweek_to_saturday(time_value),
    report_date = epiweek_to_saturday(issue),
    count = as.integer(count)
  ) |>
  select(reference_date, report_date, count) |>
  as.data.frame()

usethis::use_data(nhsn_mt_flu_df, overwrite = TRUE)
