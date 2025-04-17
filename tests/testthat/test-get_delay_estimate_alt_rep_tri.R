# This is a placeholder script -- these tests will be implemented into tests
# for get_delay_estimate()

short_triangle <- matrix(
  c(
    1, 2, 1, 2, 2, 3, 2, 4,
    1, 2, 1, 2, 2, 3, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA
  ),
  nrow = 4,
  byrow = TRUE
)


reporting_triangle <- short_triangle
n <- 4


nr0 <- nrow(reporting_triangle)
max_delay <- 7
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
  for (ro in 2:(n_delays)) {
    co_min <- which(colSums(is.na(expectation)) > 0)[1]
    co_max <- which(colSums(is.na(expectation)) > sum(is.na(expectation[, co_min])))[1] - 1
    co_max <- ifelse(is.na(co_max), n_delays, co_max)
    width <- co_max - co_min + 1
    block_top_left <- rep_tri[1:(n_dates - ro + 1), 1:(co_min - 1), drop = FALSE]
    block_top <- rep_tri[1:(n_dates - ro + 1), co_min:co_max, drop = FALSE]
    mult_factor[ro - 1] <- sum(block_top) / max(sum(block_top_left), 1)
    block_bottom_left <- expectation[(n_dates - ro + 2):n_dates, 1:(co_min - 1),
      drop = FALSE
    ]
    # We compute the expectation so that we can get the delay estimate
    # You don't want to just sum and average out here, you want to somehow
    # take the weekly pattern
    expectation[(n_dates - ro + 2):n_dates, co_min:co_max] <- mult_factor[ro - 1] *
      sum_columns_by_position(
        block_bottom_left, width
      )
  }
}





# Use the completed reporting matrix to get the point estimate of the
# delay distribution
pmf <- colSums(expectation) / sum(expectation)


rep_tri <- matrix(
  c(
    1, 2, 1, 2, 2, 3, 2, 4,
    1, 2, 1, 2, 2, 3, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA,
    1, 2, NA, NA, NA, NA, NA, NA
  ),
  nrow = 4,
  byrow = TRUE
)

exp <- matrix(
  c(
    1, 1, 2, 3, 2, 4,
    1, 1, 2, 3, NA, NA,
    1, 1, NA, NA, NA, NA,
    1, 1, NA, NA, NA, NA
  ),
  nrow = 4,
  byrow = TRUE
)

expectation <- matrix(
  c(
    1, 2, 1, 2, 2, 3, 2, 4,
    1, 2, 1, 2, 2, 3, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA,
    1, 2, 1, 2, NA, NA, NA, NA
  ),
  nrow = 4,
  byrow = TRUE
)
ro <- 3

sum_columns_by_position <- function(mat, group_size = 3) {
  # Get matrix dimensions
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)

  # Create result vector
  result <- numeric(group_size)

  # For each group position (0 to group_size-1)
  for (pos in 1:group_size) {
    # Get columns that have remainder (pos-1) when divided by group_size
    col_indices <- seq(from = pos, to = ncol_mat, by = group_size)

    # Sum all elements in these columns
    result[pos] <- sum(mat[, col_indices])
  }

  return(result)
}


# Example with your matrix
mat <- matrix(
  c(
    3, 2, 1, 3, 2, 1, 3, 2, 1,
    6, 4, 2, 6, 4, 2, 6, 4, 2,
    3, 2, 1, 3, 2, 1, 3, 2, 1
  ),
  nrow = 3, byrow = TRUE
)


# Sum every 3rd column (positions 3, 6, 9)
sum_columns_by_position(mat, 3)
