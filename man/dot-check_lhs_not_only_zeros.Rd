% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.check_lhs_not_only_zeros}
\alias{.check_lhs_not_only_zeros}
\title{Check if there are non-zero-values on the LHS of NAs}
\usage{
.check_lhs_not_only_zeros(mat)
}
\arguments{
\item{mat}{Matrix to check}
}
\value{
Boolean indicating whether or not there are non-zero values on the
LHS of the first NA (TRUE = has non-zeros, FALSE = only zeros)
}
\description{
Check if there are non-zero-values on the LHS of NAs
}
