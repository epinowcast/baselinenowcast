% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nowcast_list_to_df.R
\name{nowcast_list_to_df}
\alias{nowcast_list_to_df}
\title{Generate probabilistic nowcast dataframe}
\usage{
nowcast_list_to_df(list_of_nowcasts)
}
\arguments{
\item{list_of_nowcasts}{List of  matrices of expected
observed reporting squares.}
}
\value{
\code{nowcast_df} Dataframe containing observations and expected
observed nowcasts indexed by reference time and delay.
}
\description{
This function ingests a list of matrices that represent expected observed
nowcasts, and generates a long tidy dataframe indexed by time and delay.
}
\examples{
point_nowcast <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, 17.8,
    80, 40, 23.2, 15.9,
    70, 35, 20.3, 19.9
  ),
  nrow = 5,
  byrow = TRUE
)

list_of_exp_obs_nowcast <- add_obs_errors_to_nowcast(
  comp_rep_square = point_nowcast,
  disp = c(8, 1.4, 4),
  n_draws = 10
)

nowcast_df <- nowcast_list_to_df(
  list_of_nowcasts = list_of_exp_obs_nowcast
)
}
