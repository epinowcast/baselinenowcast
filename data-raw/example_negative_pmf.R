# Script to generate synthetic example data with negative PMF entries
# This demonstrates a scenario where corrections result in net downward
# adjustments at a specific delay, producing a PMF with negative entries
# when estimated with preprocess = NULL

# Load package
library(baselinenowcast)

# Create a reporting triangle with systematic downward corrections at delay 2
# This represents a realistic scenario where data quality reviews at day 2
# consistently reclassify cases to earlier delays or remove false positives
example_negative_pmf <- matrix(
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

# Verify that this produces a PMF with negative entries
delay_pmf_null <- estimate_delay(
  reporting_triangle = example_negative_pmf,
  max_delay = 3,
  n = 5,
  preprocess = NULL
)

cat("PMF with preprocess = NULL:\n")
print(delay_pmf_null)
cat("\nSum of PMF:", sum(delay_pmf_null), "\n")

# Check for negative entries
if (any(delay_pmf_null < 0)) {
  cat(
    "\nSuccess: PMF contains negative entries at positions:",
    which(delay_pmf_null < 0), "\n"
  )
  cat("Negative values:", delay_pmf_null[delay_pmf_null < 0], "\n")
} else {
  warning(
    "PMF does not contain negative entries. Adjust triangle values.",
    call. = FALSE
  )
}

# Verify CDF behaviour
delay_cdf_null <- cumsum(delay_pmf_null)
cat("\nCDF with preprocess = NULL:\n")
print(delay_cdf_null)

# Check if CDF is not strictly increasing
cdf_diffs <- diff(delay_cdf_null)
if (any(cdf_diffs < 0)) {
  cat(
    "\nSuccess: CDF is not strictly increasing at positions:",
    which(cdf_diffs < 0), "\n"
  )
  cat("Negative differences:", cdf_diffs[cdf_diffs < 0], "\n")
} else {
  cat("\nNote: CDF is weakly increasing (no decreasing steps)\n")
}

# Compare with default preprocessing
delay_pmf_preprocessed <- estimate_delay(
  reporting_triangle = example_negative_pmf,
  max_delay = 3,
  n = 5,
  preprocess = preprocess_negative_values
)

cat("\nPMF with default preprocessing:\n")
print(delay_pmf_preprocessed)
cat("\nSum of PMF:", sum(delay_pmf_preprocessed), "\n")

# Document the scenario
cat("\n===== Scenario Documentation =====\n")
cat("This example represents a reporting process where:\n")
cat("1. Initial reports arrive at delays 0, 1, and 3\n")
cat("2. At delay 2, systematic data quality reviews occur\n")
cat("3. These reviews consistently identify false positives or")
cat(" reclassify cases\n")
cat("4. The result is net downward corrections at delay 2\n")
cat("5. When estimated with preprocess = NULL, this produces a")
cat(" negative PMF entry\n")
cat("6. The corresponding CDF is not strictly increasing\n")
cat("\nThis is a valid representation of the reporting process,")
cat(" and the negative\n")
cat("PMF reflects the net probability of downward correction at that delay.\n")

# Save the example data
usethis::use_data(example_negative_pmf, overwrite = TRUE)
cat("\nExample data saved to data/example_negative_pmf.rda\n")
