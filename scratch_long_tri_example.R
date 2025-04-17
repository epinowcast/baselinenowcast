# Attempt a method for a long triangle (e.g. daily reference dates, weekly reports)

long_triangle <- matrix(
  c(
    1, 2, 3, 4,
    2, 4, 6, 8,
    1, 2, 3, NA,
    2, 4, 6, NA,
    1, 2, NA, NA,
    2, 4, NA, NA,
    1, NA, NA, NA,
    2, NA, NA, NA
  ),
  nrow = 8,
  byrow = TRUE
)

# Break into the number of symmetric triangles present
reporting_triangle <- long_triangle
n <- 8

# Break into the number of symmetric triangles present
# First need to get the interval by comparing ratio of NA cols to NA rows
cols_with_na <- sum(apply(reporting_triangle, 2, function(col) any(is.na(col))))
rows_with_na <- sum(apply(reporting_triangle, 1, function(row) any(is.na(row))))

int <- cols_with_na / rows_with_na
if (int < 1) {
  int <- rows_with_na / cols_with_na
  triangle_type <- "long"
} else if (int > 1) {
  triangle_type <- "short"
} else if (int == 1) {
  triangle_type <- "square"
}
if (isFALSE(assert_integerish(int))) {
  cli_abort(
    message = c(
      "Delays and reference times with different indexing must be ",
      "evenly spaced."
    )
  )
}

extract_nth_rows <- function(mat, n, start = 1) {
  # Get number of columns
  nrow_mat <- nrow(mat)

  # Generate indices for every nth column starting from 'start'
  row_indices <- seq(from = start, to = nrow_mat, by = n)

  # Extract the columns
  result <- mat[row_indices, , drop = FALSE]

  return(result)
}

tri_list <- lapply(1:int, function(start_pos) {
  return(extract_nth_rows(reporting_triangle, n = int, start = start_pos))
})

mat_list <- lapply(1:int, function(i) {
  return(fill_in_tri(tri_list[[i]]))
})

interleave_matrix_rows <- function(matrix_list) {
  # Check if list is empty
  if (length(matrix_list) == 0) {
    stop("No matrices provided")
  }

  # Determine how many rows to take from each matrix
  # (uses the first row of each matrix)
  n_matrices <- length(matrix_list)

  # Determine the number of columns (should be the same for all matrices)
  n_cols <- ncol(matrix_list[[1]])

  min_rows <- min(sapply(matrix_list, nrow))
  # Create the result matrix
  # We'll take the first row from each matrix
  result <- matrix(NA, nrow = n_matrices * n_cols, ncol = n_cols)

  # Fill the result matrix
  for (i in 1:min_rows) {
    for (j in 1:n_matrices) {
      result_row <- (i - 1) * n_matrices + j
      result[result_row, ] <- matrix_list[[j]][i, ]
    }
  }

  return(result)
}

comb_matr <- interleave_matrix_rows((mat_list))
