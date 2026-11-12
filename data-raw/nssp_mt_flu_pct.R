# Generate the Montana NSSP influenza ED-visit percentage revision dataset.
# Run from the package root with Rscript data-raw/nssp_mt_flu_pct.R.
# An optional first argument supplies a previously downloaded archive CSV.
# Published percentages are differenced into percentage-point increments;
# their cumulative sum reconstructs each successive published percentage.
library(readr)
library(dplyr)

# Saturday ending an MMWR epiweek; week 1 contains 4 January.
epiweek_to_saturday <- function(epiweek) {
  year <- epiweek %/% 100
  week <- epiweek %% 100
  jan4 <- as.Date(paste0(year, "-01-04"))
  week1_sunday <- jan4 - as.integer(format(jan4, "%w"))
  return(week1_sunday + 7 * (week - 1) + 6)
}

# The archive starts at issue 202445. Start at reference week 202444,
# whose first published value is in that issue, to retain the initial report.
# Use one location so the public query does not require an API key.
# nolint start: nonportable_path_linter
delphi_url <- paste0(
  "https://api.delphi.cmu.edu/epidata/covidcast/",
  "?data_source=nssp&signals=pct_ed_visits_influenza",
  "&geo_type=state&time_type=week&geo_values=mt",
  "&time_values=202444-202535&issues=202445-202545&format=csv"
)
# nolint end
args <- commandArgs(trailingOnly = TRUE)
raw_data <- read_csv(
  if (length(args) > 0) args[[1]] else delphi_url,
  show_col_types = FALSE
)
stopifnot(
  !anyDuplicated(raw_data[c("time_value", "issue")]),
  is.finite(raw_data$value),
  raw_data$missing_value == 0,
  raw_data$value >= 0 & raw_data$value <= 100
)

increments <- raw_data |>
  arrange(time_value, issue) |>
  group_by(time_value) |>
  mutate(
    count = value - lag(value, default = 0),
    reconstructed = cumsum(count)
  ) |>
  ungroup()
stopifnot(isTRUE(all.equal(
  increments$reconstructed, increments$value, tolerance = 1e-12
)))

nssp_mt_flu_pct_df <- increments |>
  mutate(
    reference_date = epiweek_to_saturday(time_value),
    report_date = epiweek_to_saturday(issue)
  ) |>
  select(reference_date, report_date, count) |>
  as.data.frame()

usethis::use_data(nssp_mt_flu_pct_df, overwrite = TRUE)
