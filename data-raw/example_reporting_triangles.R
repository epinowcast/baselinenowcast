# Script to generate example reporting_triangle objects for use in examples

# 1. Create a simple standard example reporting triangle ==================
example_reporting_triangle <- as_reporting_triangle(
  data = matrix(
    c(
      100, 55, 30, 12,
      70, 40, 24, 8,
      80, 50, 25, 10,
      100, 50, 20, NA,
      90, 45, NA, NA,
      110, NA, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 7,
    byrow = TRUE
  ),
  reference_dates = seq(as.Date("2024-01-01"), by = "day", length.out = 7),
  delays_unit = "days"
)

usethis::use_data(example_reporting_triangle, overwrite = TRUE)

# 2. Create example with downward corrections =============================
# This demonstrates a scenario where corrections result in net downward
# adjustments at a specific delay, producing a PMF with negative entries
# when estimated with preprocess = NULL
example_downward_corr_rt <- as_reporting_triangle(
  data = matrix(
    c(
      # Reference dates with day 0, 1, 2, 3 delays
      100, 60, -20, 10, # Day 1: 20 cases removed at delay 2
      120, 70, -25, 15, # Day 2: 25 cases removed at delay 2
      110, 65, -22, 12, # Day 3: 22 cases removed at delay 2
      130, 75, -28, 18, # Day 4: 28 cases removed at delay 2
      115, 68, -24, 14, # Day 5: 24 cases removed at delay 2
      125, 72, -26, NA, # Day 6: incomplete at delay 3
      105, 62, NA, NA, # Day 7: incomplete at delays 2-3
      95, NA, NA, NA # Day 8: incomplete at delays 1-3
    ),
    nrow = 8,
    byrow = TRUE
  ),
  reference_dates = seq(as.Date("2024-01-01"), by = "day", length.out = 8),
  delays_unit = "days"
)

usethis::use_data(example_downward_corr_rt, overwrite = TRUE)
