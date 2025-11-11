# Package index

## High-level interface

Functions most users will interact with for typical nowcasting
workflows.

### Reporting triangle data structures

Functions for creating, converting, and validating reporting triangle
objects from matrices or data frames.

- [`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md)
  : Convert reporting_triangle to ChainLadder triangle format

- [`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md)
  :

  Create a `reporting_triangle` object

- [`as_reporting_triangle(`*`<data.frame>`*`)`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
  :

  Create a `reporting_triangle` object from a data.frame

- [`as_reporting_triangle(`*`<matrix>`*`)`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
  :

  Create a `reporting_triangle` from a matrix

- [`as_reporting_triangle(`*`<triangle>`*`)`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.triangle.md)
  : Convert ChainLadder triangle to reporting_triangle format

- [`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md)
  :

  Assert validity of `reporting_triangle` objects

- [`get_delay_unit()`](https://baselinenowcast.epinowcast.org/reference/get_delay_unit.md)
  : Get delays unit from a reporting triangle

- [`get_max_delay()`](https://baselinenowcast.epinowcast.org/reference/get_max_delay.md)
  : Get maximum delay from a reporting triangle

- [`get_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/get_reporting_structure.md)
  : Get the reporting structure from a reporting triangle

- [`get_structure()`](https://baselinenowcast.epinowcast.org/reference/get_structure.md)
  : Get structure from a reporting triangle

- [`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/new_reporting_triangle.md)
  :

  Class constructor for `reporting_triangle` objects

- [`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  [`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  : Reporting Triangle Object

### Nowcast data frames

The main nowcasting interface. Functions for creating and validating
nowcast data frame objects containing point or probabilistic nowcast
results.

- [`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md)
  :

  Assert validity of `baselinenowcast_df` objects

- [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
  : Generate a nowcast

- [`baselinenowcast(`*`<data.frame>`*`)`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md)
  : Create a dataframe of nowcast results from a dataframe of cases
  indexed by reference date and report date

- [`baselinenowcast(`*`<reporting_triangle>`*`)`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
  : Create a dataframe of nowcast results from a single reporting
  triangle

- [`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  [`baselinenowcast_df`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  : Nowcast Data.frame Object

- [`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)
  : Combine data from a nowcast dataframe, strata, and reference dates

### Workflow wrappers

High-level wrapper functions that combine multiple steps for end-to-end
nowcasting workflows.

- [`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md)
  : Allocate training volume based on combination of defaults and
  user-specified values for training volume for delay and uncertainty
  estimation.
- [`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md)
  : Estimate and apply delay from a reporting triangle
- [`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)
  : Estimate and apply uncertainty to a point nowcast matrix
- [`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)
  : Estimate uncertainty parameters using retrospective nowcasts

## Lower-level components

Modular functions for building custom nowcasting workflows and advanced
use cases.

### Estimate delay distributions

Functions for estimating empirical delay distributions from reporting
triangles.

- [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
  : Estimate a delay distribution from a reporting triangle
- [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md)
  : Preprocess negative values in the reporting triangle

### Generate retrospective data

Functions for creating retrospective reporting triangles by truncating
and generating incomplete data structures for validation and uncertainty
estimation.

- [`construct_triangle()`](https://baselinenowcast.epinowcast.org/reference/construct_triangle.md)
  : Generate a single retrospective reporting triangle
- [`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md)
  : Generate retrospective reporting triangles
- [`truncate_triangle()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangle.md)
  : Get a single truncated triangle
- [`truncate_triangles()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangles.md)
  : Generate truncated reporting triangles

### Generate point nowcasts

Functions for applying delay distributions to incomplete reporting data
to generate point nowcast estimates.

- [`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md)
  : Apply the delay to generate a point nowcast
- [`fill_triangle()`](https://baselinenowcast.epinowcast.org/reference/fill_triangle.md)
  : Generate point nowcast
- [`fill_triangles()`](https://baselinenowcast.epinowcast.org/reference/fill_triangles.md)
  : Generate retrospective nowcasts

### Estimate observation error

Functions for estimating observation error parameters from retrospective
nowcast performance to quantify uncertainty.

- [`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md)
  : Estimate uncertainty parameters
- [`fit_by_horizon()`](https://baselinenowcast.epinowcast.org/reference/fit_by_horizon.md)
  : Helper function that fits its each column of the matrix (horizon) to
  an observation model.
- [`fit_nb()`](https://baselinenowcast.epinowcast.org/reference/fit_nb.md)
  : Fit a negative binomial to a vector of observations and expectations

### Generate probabilistic nowcasts

Functions for generating probabilistic nowcasts by combining point
estimates with uncertainty quantification from observation error models.

- [`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md)
  : Combine observed data with a single prediction draw
- [`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md)
  : Sample from negative binomial model given a set of predictions
- [`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md)
  : Generate a single draw of a nowcast combining observed and predicted
  values
- [`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md)
  : Generate multiple draws of a nowcast combining observed and
  predicted values
- [`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md)
  : Get a draw of only the predicted elements of the nowcast vector
- [`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)
  : Get a dataframe of multiple draws of only the predicted elements of
  the nowcast vector

## Data

Example datasets included with the package.

- [`germany_covid19_hosp`](https://baselinenowcast.epinowcast.org/reference/germany_covid19_hosp.md)
  : Incident COVID-19 hospitalisations indexed by the date of positive
  test (reference date) and report date from Germany in 2021 and 2022.
- [`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md)
  : A synthetic dataset containing the number of incident cases indexed
  by reference date and report date. While data of this form could be
  from any source, this data is meant to represent the output of
  pre-processing the syn_nssp_line_list dataset, which is a synthetic
  patient-level line list data from the United State's National
  Syndromic Surveillance System (NSSP).
- [`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
  : A synthetic dataset resembling line-list (each row is a patient)
  data from the United States' National Syndromic Surveillance System
  (NSSP) accessed via the Essence platform. All entries are synthetic,
  formatted to look as close to the real raw data as possible.
