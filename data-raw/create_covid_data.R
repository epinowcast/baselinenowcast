# Script to generate large covid dataset for baselinenowcast tests.
# Not currently used as package data.
# Run this script when the data needs to be updated
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required to run this script. Please install it with install.packages('readr').") # nolint
}
library(readr)
covid_url <- "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/11c745322c055cfbd4f0c8f72241642a50aea399/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv" # nolint
raw_data <- read_csv(covid_url) |>
  rename(value_81d = `value_>80d`)

raw_data_long <- pivot_longer(
  raw_data,
  cols = starts_with("value_"),
  names_to = "delay",
  values_to = "count",
  names_prefix = "value_"
) |>
  mutate(
    delay = as.integer(gsub("d.*$", "", delay)),
    report_date = date + days(delay)
  ) |>
  rename(reference_date = date) |>
  filter(
    location == "DE",
    delay <= 40
  )

# Save the data to fixtures folder in testthat
saveRDS(raw_data_long, file.path(
  "tests",
  "testthat",
  "fixtures",
  "covid_data.rds"
))
