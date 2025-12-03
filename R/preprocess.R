#' Preprocess negative values in the reporting triangle
#'
#' @description
#' Takes in a reporting triangle and returns it with negative values of
#' reporting handled by redistributing them to earlier delays (from longer
#' delay to shorter). This is useful when dealing with reporting corrections
#' that can result in negative incremental counts.
#'
#' When negative values are detected, they are set to zero and the negative
#' amount is subtracted from the count at the next earlier delay (moving from
#' right to left in each row). This process continues until either the
#' negative value is fully absorbed or the first delay is reached.
#'
#' This code was adapted from code written (under an MIT license)
#' by the Karlsruhe Institute of Technology RESPINOW
#' German Hospitalization Nowcasting Hub.
#' Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R #nolint
#'
#' @param reporting_triangle A [reporting_triangle] object.
#' @inheritParams assert_reporting_triangle
#'
#' @return A [reporting_triangle] object with negative values handled via
#'   redistribution to earlier delays.
#'
#' @details
#' Use this function when:
#' - Your data contains reporting corrections that result in negative counts
#' - You want to preserve the total count while handling negatives
#' - You need a delay distribution that sums to 1 or a CDF that is weakly
#'   increasing
#'
#' Do not use this function when:
#' - Your data naturally has negative PMF entries (e.g., from differencing)
#' - You want to preserve the original structure including negatives
#' - You are working with corrections that should be reflected as negative
#'   probabilities
#'
#' @importFrom cli cli_alert_info
#' @family estimate_delay
#' @export
#'
#' @examples
#' # Using example dataset with negative values from corrections
#' # Preprocess to handle negatives
#' preprocessed <- preprocess_negative_values(example_downward_corr_rt)
#' preprocessed
preprocess_negative_values <- function(reporting_triangle, validate = TRUE) {
  assert_reporting_triangle(reporting_triangle, validate)

  # Convert to matrix for processing
  triangle_mat <- as.matrix(reporting_triangle)

  # Check if any negative values are present
  has_negatives <- any(triangle_mat < 0, na.rm = TRUE)
  if (has_negatives) {
    cli_alert_info(
      "Negative values detected in reporting triangle and will be corrected"
    )
  }

  integer_cols <- seq_len(ncol(triangle_mat))
  pos_triangle <- triangle_mat
  pos_triangle[is.na(pos_triangle)] <- 0 # Set NAs to 0 temporarily
  for (i in seq_len(nrow(triangle_mat))) {
    to_subtract <- 0
    row_i <- pos_triangle[i, ]
    # Loop over the columns starting from the last column back to max delay
    # column, and if there is a negative value, we add this to the
    # next day and set that one as 0.
    for (j in rev(integer_cols)) {
      value <- row_i[[j]]
      if (!is.na(value)) {
        # Either adds 0 or the previous days negative value
        value <- value + to_subtract
        if (value < 0) {
          # Want to subtract from subsequent day
          to_subtract <- value
          pos_triangle[i, j] <- 0 # Set the negative value in the RT to 0
        } else {
          pos_triangle[i, j] <- value
          to_subtract <- 0
        }
      }
    }
  }
  for (col in integer_cols) {
    pos_triangle[[col]] <- as.integer(pos_triangle[[col]])
  }

  # Return values that were NA back to NA
  pos_triangle[is.na(triangle_mat)] <- NA

  # Return reporting_triangle with preprocessed data
  return(.update_triangle_matrix(reporting_triangle, pos_triangle))
}
