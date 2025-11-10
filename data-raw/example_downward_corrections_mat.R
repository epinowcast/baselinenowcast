# Script to generate synthetic example data with downward corrections
# This demonstrates a scenario where corrections result in net downward
# adjustments at a specific delay, producing a PMF with negative entries
# when estimated with preprocess = NULL

# Create a reporting triangle with systematic downward corrections at delay 2
# This represents a realistic scenario where data quality reviews at day 2
# consistently reclassify cases to earlier delays or remove false positives
example_downward_corr_mat <- matrix(
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
)

# Save the example data
usethis::use_data(example_downward_corr_mat, overwrite = TRUE)
