# workable method for a short triangle -- breaks into n symmetric triangles
# and binds them back together

short_triangle <- matrix(
  c(
    1, 2, 1, 2, 2, 3, 2, 4,
    1, 2, 1, 2, 2, 3, 2, 4,
    1, 2, 1, 2, 2, 3, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA,
    1, 2, NA, NA, NA, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)

reporting_triangle <- short_triangle
n <- 4


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


extract_nth_columns <- function(mat, n, start = 1) {
  # Get number of columns
  ncol_mat <- ncol(mat)

  # Generate indices for every nth column starting from 'start'
  col_indices <- seq(from = start, to = ncol_mat, by = n)

  # Extract the columns
  result <- mat[, col_indices, drop = FALSE]

  return(result)
}


tri_list <- lapply(1:int, function(start_pos) {
  return(extract_nth_columns(reporting_triangle, n = int, start = start_pos))
})


fill_in_tri <- function(reporting_triangle,
                        max_delay = ncol(reporting_triangle) - 1,
                        n = nrow(reporting_triangle)) {
  nr0 <- nrow(reporting_triangle)
  trunc_triangle <- reporting_triangle[(nr0 - n + 1):nr0, 1:(max_delay + 1)]
  rep_tri <- .handle_neg_vals(trunc_triangle)
  n_delays <- ncol(rep_tri)
  n_dates <- nrow(rep_tri)
  mult_factor <- vector(length = max_delay - 1)
  expectation <- rep_tri
  # Find the column to start filling in
  start_col <- which(colSums(is.na(rep_tri)) > 0)[1]


  # Only fill in reporting triangle if it is incomplete
  if (!is.na(start_col)) {
    for (co in start_col:(n_delays)) {
      block_top_left <- rep_tri[1:(n_dates - co + 1), 1:(co - 1), drop = FALSE]
      block_top <- rep_tri[1:(n_dates - co + 1), co, drop = FALSE]
      mult_factor[co - 1] <- sum(block_top) / max(sum(block_top_left), 1)
      block_bottom_left <- expectation[(n_dates - co + 2):n_dates, 1:(co - 1),
        drop = FALSE
      ]
      # We compute the expectation so that we can get the delay estimate
      expectation[(n_dates - co + 2):n_dates, co] <- mult_factor[co - 1] *
        rowSums(
          block_bottom_left
        )
    }
  }

  return(expectation)
}
mat_list <- lapply(1:int, function(i) {
  return(fill_in_tri(tri_list[[i]]))
})


interleave_matrices <- function(matrices) {
  # Get all matrices passed to the function
  n_matrices <- length(matrices)

  # Check if we have at least one matrix
  if (n_matrices == 0) {
    stop("No matrices provided")
  }

  # Determine the number of rows (should be the same for all matrices)
  n_rows <- nrow(matrices[[1]])

  # Determine how many columns to take from each matrix
  # (use the minimum number of columns across all matrices)
  min_cols <- min(sapply(matrices, ncol))

  # Create the result matrix
  result <- matrix(0, nrow = n_rows, ncol = n_matrices * min_cols)

  # Fill the result matrix
  for (i in 1:min_cols) {
    for (j in 1:n_matrices) {
      result_col <- (i - 1) * n_matrices + j
      result[, result_col] <- matrices[[j]][, i]
    }
  }

  return(result)
}
# Bind back together
comb_matr <- interleave_matrices((mat_list))
