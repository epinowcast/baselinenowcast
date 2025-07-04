% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_nowcast_pred_draws.R
\name{combine_obs_with_pred}
\alias{combine_obs_with_pred}
\title{Combine observed data with a single prediction draw}
\usage{
combine_obs_with_pred(
  predicted_counts,
  reporting_triangle,
  fun_to_aggregate = sum,
  k = 1
)
}
\arguments{
\item{predicted_counts}{Vector of predicted counts at each reference time}

\item{reporting_triangle}{Matrix of the reporting triangle, with rows
representing the time points of reference and columns representing the
delays. Can be a reporting matrix or incomplete reporting matrix.
Can also be a ragged reporting triangle, where multiple columns are
reported for the same row. (e.g. weekly reporting of daily data).}

\item{fun_to_aggregate}{Function that will operate along the nowcast
vectors after summing across delays. Eventually, we can add things like
mean, but for now since we are only providing a negative binomial
observation model, we can only allow sum. Currently supported
functions: \code{sum}.}

\item{k}{Integer indicating the number of reference times to apply the
\code{fun_to_aggregate} over to create target used to compute the nowcast
errors.}
}
\value{
A vector of predicted counts at each reference time
}
\description{
Internally it sums observed counts from the reporting triangle by reference
time and adds them to the predicted counts to form a single draw of the
nowcast for the final counts by reference time.
}
\examples{
pred_counts <- c(10, 20, 30, 40)
reporting_matrix <- matrix(
  c(
    7, 9, 4, 3,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12
  ),
  nrow = 4,
  byrow = TRUE
)
reporting_triangle <- generate_triangle(reporting_matrix)
combine_obs_with_pred(pred_counts, reporting_triangle)

# Another example with rolling sum
combine_obs_with_pred(pred_counts, reporting_triangle, k = 2)
}
