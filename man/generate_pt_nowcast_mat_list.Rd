% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_pt_nowcast_mat_list.R
\name{generate_pt_nowcast_mat_list}
\alias{generate_pt_nowcast_mat_list}
\title{Generate retrospective nowcasts}
\usage{
generate_pt_nowcast_mat_list(
  reporting_triangle_list,
  max_delay = min(sapply(reporting_triangle_list, ncol)) - 1,
  n = min(sapply(reporting_triangle_list, nrow)),
  delay_pmf = NULL
)
}
\arguments{
\item{reporting_triangle_list}{List of \code{n} truncated reporting triangle
matrices with as many rows as available given the truncation.}

\item{max_delay}{Integer indicating the maximum delay to estimate, in units
of the delay. The default is to use the whole reporting triangle,
\code{ncol(reporting_triangle) -1}.}

\item{n}{Integer indicating the number of reference times
(number of rows) to use to estimate the delay distribution for each
reporting triangle. Default is the minimum of the number of rows of
all the matrices in the \code{list_of_rts}.}

\item{delay_pmf}{Vector or list of vectors of delays assumed to be indexed
starting at the first delay column in each of the matrices in
\code{reporting_triangle_list}. If a list, must of the same length as
\code{reporting_triangle_list}, with elements aligning. Default is \code{NULL}}
}
\value{
\code{pt_nowcast_matr_list} List of the same number of elements as the
input \code{reporting_triangle_list}but with each reporting triangle filled
in based on the delay estimated in that reporting triangle.
}
\description{
This function ingests a list of incomplete reporting triangles and
generates a list of point nowcast matrices, based on the delay estimated in
each triangle or the corresponding delay passed in. It uses the specified
\code{n} number of reference times to estimate the delay in each retrospective
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

trunc_rts <- truncate_triangles(triangle)
retro_rts <- generate_triangles(trunc_rts)
retro_pt_nowcast_mat_list <- generate_pt_nowcast_mat_list(retro_rts)
retro_pt_nowcast_mat_list[1:3]
}
