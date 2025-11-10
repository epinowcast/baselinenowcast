# Helper function to convert test matrices to reporting_triangle objects
to_reporting_triangle <- function(mat) {
  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = nrow(mat))
  as_reporting_triangle(
    data = mat,
    reference_dates = ref_dates,
    max_delay = ncol(mat) - 1
  )
}
