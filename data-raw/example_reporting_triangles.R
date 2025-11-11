# Script to generate example reporting_triangle objects for use in examples
# and tests
devtools::load_all()

# 1. Create a simple standard example reporting triangle ==================
example_reporting_triangle <- as_reporting_triangle(
  data = matrix(
    c(
      80, 50, 25, 10,
      100, 50, 20, NA,
      90, 45, NA, NA,
      110, NA, NA, NA,
      95, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  ),
  reference_dates = seq(as.Date("2024-01-01"), by = "day", length.out = 5),
  delays_unit = "days"
)

usethis::use_data(example_reporting_triangle, overwrite = TRUE)

# 2. Create example with downward corrections =============================
example_downward_corr_rt <- as_reporting_triangle(
  data = example_downward_corr_mat,
  reference_dates = seq(as.Date("2024-01-01"), by = "day", length.out = 8),
  delays_unit = "days"
)

usethis::use_data(example_downward_corr_rt, overwrite = TRUE)
