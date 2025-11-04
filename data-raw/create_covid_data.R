# Script to generate large covid dataset with mutliple age groups.
# Used for tests and examples.
# Run this script when the data needs to be updated
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required to run this script. Please install it with install.packages('readr').") # nolint
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  stop("Package 'lubridate' is required to run this script. Please install it with install.packages('lubridate').") # nolint
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required to run this script. Please install it with install.packages('dplyr').") # nolint
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Package 'tidyr' is required to run this script. Please install it with install.packages('tidyr').") # nolint
}
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
covid_url <- "https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/11c745322c055cfbd4f0c8f72241642a50aea399/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv" # nolint
raw_data <- read_csv(covid_url) |>
  rename(value_81d = `value_>80d`)

germany_covid19_hosp <- pivot_longer(
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
    delay <= 40,
    report_date <= "2021-12-01"
  )

usethis::use_data(germany_covid19_hosp, overwrite = TRUE)
