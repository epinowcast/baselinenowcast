% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_point_nowcasts.R
\name{generate_point_nowcasts}
\alias{generate_point_nowcasts}
\title{Generate retrospective nowcasts}
\usage{
generate_point_nowcasts(
  list_of_rts,
  max_delay = min(sapply(list_of_rts, ncol)) - 1,
  n = min(sapply(list_of_rts, nrow))
)
}
\arguments{
\item{list_of_rts}{List of reporting triangle matrices, in order
from most recent (most complete) to least recent. Bottom right of the
matrices should contain NAs.}

\item{max_delay}{Integer indicating the maximum delay to estimate, in units
of the delay. The default is to use one less than the minimum number of
rows of all of the matrices in the \code{list_of_rts}.}

\item{n}{Integer indicating the number of observations
(number of rows) to use to estimate the delay distribution for each
reporting triangle. Default is the minimum of the number of rows of
all the matrices in the \code{list_of_rts}.}
}
\value{
\code{list_of_nowcasts} List of the same number of elements as the input
\code{list_of_rts}but with each reporting triangle filled in based on the delay
estimated in that reporting triangle.
}
\description{
This function ingests a list of incomplete reporting triangles and
generates a list of reporting squares, or "complete"
point estimates of reporting triangles based on the delay estimated in
each triangle. It uses the specified \code{n} number of
observations to estimate the empirical delay for each retrospective
reporting triangle.
}
\examples{
triangle <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, NA,
    82, 42, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
)

trunc_rts <- truncate_triangles(
  triangle = triangle
)
retro_rts <- generate_triangles(
  list_of_trunc_rts = trunc_rts
)
retro_nowcasts <- generate_point_nowcasts(
  list_of_rts = retro_rts
)
print(retro_nowcasts[[1]])
}
