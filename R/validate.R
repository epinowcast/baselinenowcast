#' Validate triangle
#' @description
#' Various checks to make sure that the reporting triangle passed in to
#' `estimate_delay()` is formatted properly.
#' @param triangle a dataframe formatted as a reporting triangle
#'
#' @returns
validate_triangle <- function(triangle) {
  # check column names includes reference date
  checkmate::assert_names(names(triangle), must.include = "reference_date")
  # check that the columns are integer digits
  checkmate::assert_integerish(
    as.numeric(grep("^\\d+$", names(triangle), value = TRUE))
  )
  # Check that this returns integers not decimals
  checkmate::assert_integerish(
    as.numeric(grep("^\\d+$", names(triangle), value = TRUE))
  )
  # Check that grep returns something of length greater than 1
  if (length(as.numeric(grep("^\\d+$", names(triangle), value = TRUE))) < 1) {
    cli::cli_abort(
      message = "Attempt to convert column names to integers failed"
    )
  }
}
