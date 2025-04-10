% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_triangles.R
\name{generate_triangles}
\alias{generate_triangles}
\title{Generate retrospective reporting triangles}
\usage{
generate_triangles(list_of_trunc_rts)
}
\arguments{
\item{list_of_trunc_rts}{List of truncated reporting triangle matrices.}
}
\value{
\code{list_of_rts} List of retrospective reporting triangle matrices,
generated by removing the bottom right observations from the truncated
reporting triangle matrices.
}
\description{
This function ingests a reporting triangle/matrix and the number of
retrospective reporting triangles we want to create, \code{n}, and iteratively
generates the reporting triangle that would have been available as of the
maximum reference time, working from bottom to top for \code{n} snapshots.
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
  triangle = triangle,
  n = 2
)
retro_rts <- generate_triangles(
  list_of_trunc_rts = trunc_rts
)
print(retro_rts[[1]])
print(retro_rts[[2]])
}
