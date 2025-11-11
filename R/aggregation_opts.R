#' Custom Aggregation Options
#'
#' @description
#' Creates an aggregation options object that defines how to preprocess
#' data before uncertainty estimation. This allows custom transformations
#' of the reference time dimension and delay dimension.
#'
#' @param ref_time Function to apply to aggregate reference times.
#'   Must accept and return a matrix. Default is `identity`.
#' @param delay Function to apply to aggregate across delays.
#'   Must accept a matrix and return a matrix. Default is
#'   `function(x) rowSums(x, na.rm = TRUE)`.
#'
#' @returns An object of class `c("aggregation_custom", "aggregation_opts")`
#'   containing the reference time and delay aggregation functions.
#'
#' @family aggregation_opts
#' @seealso [aggregation_observed()]
#' @export
#'
#' @examples
#' # Create custom aggregation with rolling sums
#' agg <- aggregation_opts(
#'   ref_time = function(x) {
#'     # 3-day rolling sum (requires zoo package)
#'     if (requireNamespace("zoo", quietly = TRUE)) {
#'       zoo::rollsum(x, k = 3, align = "right", fill = NA)
#'     } else {
#'       x
#'     }
#'   },
#'   delay = function(x) rowSums(x, na.rm = TRUE)
#' )
#' print(agg)
aggregation_opts <- function(ref_time = identity,
                             delay = function(x) rowSums(x, na.rm = TRUE)) {
  # Validate inputs
  checkmate::assert_function(ref_time)
  checkmate::assert_function(delay)

  # Create the aggregation object with two-class hierarchy
  aggregation <- structure(
    list(
      ref_time = ref_time,
      delay = delay
    ),
    class = c("aggregation_custom", "aggregation_opts")
  )

  # Validate the constructed aggregation
  assert_aggregation_opts(aggregation)

  return(aggregation)
}

#' Standard Observed Aggregation
#'
#' @description
#' Creates an aggregation options object using the standard pattern for
#' observed data: identity transformation for reference times (no aggregation)
#' and row sums across delays (total observed counts per reference time).
#' This is the default aggregation used in uncertainty estimation.
#'
#' @returns An object of class `c("aggregation_observed", "aggregation_opts")`
#'   with identity reference time aggregation and row sum delay aggregation.
#'
#' @family aggregation_opts
#' @seealso [aggregation_opts()]
#' @export
#'
#' @examples
#' # Create standard observed aggregation
#' agg <- aggregation_observed()
#' print(agg)
#'
#' # This is equivalent to:
#' agg_manual <- aggregation_opts(
#'   ref_time = identity,
#'   delay = function(x) rowSums(x, na.rm = TRUE)
#' )
aggregation_observed <- function() {
  # Create aggregation object with observed pattern
  aggregation <- structure(
    list(
      ref_time = identity,
      delay = function(x) rowSums(x, na.rm = TRUE)
    ),
    class = c("aggregation_observed", "aggregation_opts")
  )

  # Validate the constructed aggregation
  assert_aggregation_opts(aggregation)

  return(aggregation)
}

#' Print Method for Aggregation Options
#'
#' @param x An object of class `aggregation_opts`.
#' @param ... Additional arguments (not used).
#'
#' @returns Invisibly returns the input object.
#' @family aggregation_opts
#' @export
#'
#' @examples
#' agg <- aggregation_observed()
#' print(agg)
#'
#' agg_custom <- aggregation_opts(
#'   ref_time = identity,
#'   delay = function(x) colSums(x, na.rm = TRUE)
#' )
#' print(agg_custom)
print.aggregation_opts <- function(x, ...) {
  cat("Aggregation Options:\n")

  # Determine reference time aggregation description
  if (inherits(x, "aggregation_observed")) {
    ref_desc <- "identity (no aggregation)"
  } else if (identical(x$ref_time, identity)) {
    ref_desc <- "identity (no aggregation)"
  } else {
    ref_desc <- "custom function"
  }

  # Determine delay aggregation description
  # Check if delay function behaves like default rowSums by comparing bodies
  default_delay <- function(x) rowSums(x, na.rm = TRUE)
  if (identical(body(x$delay), body(default_delay))) {
    delay_desc <- "rowSums (total per reference time)"
  } else {
    delay_desc <- "custom function"
  }

  cat("  Reference time: ", ref_desc, "\n", sep = "")
  cat("  Delay:          ", delay_desc, "\n", sep = "")

  invisible(x)
}
