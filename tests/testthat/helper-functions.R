# Helper function to convert test matrices to reporting_triangle objects
to_reporting_triangle <- function(mat) {
  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = max(1, nrow(mat)))
  as_reporting_triangle(
    data = mat,
    reference_dates = ref_dates,
    max_delay = max(0, ncol(mat) - 1)
  )
}

# Helper function to strip all attributes except dim from a matrix
# for comparing just the matrix values
strip_attrs <- function(x) {
  d <- dim(x)
  attributes(x) <- NULL
  dim(x) <- d
  x
}
