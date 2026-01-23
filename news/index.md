# Changelog

## baselinenowcast 0.2.0

This is the first stable release providing core functionality for
producing probabilistic or point nowcasts from a dataframe with incident
case counts indexed by reference dates and report dates. It supports
doing so across multiple strata, either through creating
`reporting_triangle` objects for each strata and nowcasting
independently, or nowcasting directly from a dataframe containing
multiple strata. The `reporting_triangle` object allows users to specify
the unit of the delay, and provides helpful `print` and `summary` S3
methods, as well as functionality to truncate to a specific delay or a
percentile of the observed delays. A paper validating and evaluating the
performance of the methods is available at
<https://wellcomeopenresearch.org/articles/10-614/v1>.

### Package

- `fill_triangle()` has been removed. Use
  [`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md)
  instead, which now supports optional delay estimation via the
  `delay_pmf` parameter
  ([\#334](https://github.com/epinowcast/baselinenowcast/issues/334)).
- `fill_triangles()` has been removed. Use
  [`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.md)
  instead
  ([\#334](https://github.com/epinowcast/baselinenowcast/issues/334)).
- Rename `truncate_triangle()` to
  [`truncate_to_row()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_row.md)
  and `truncate_triangles()` to
  [`truncate_to_rows()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_rows.md)
  to clarify that these functions truncate by row count and distinguish
  them from other truncation utilities such as
  [`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md)
  and
  [`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_delay.md)
  ([\#336](https://github.com/epinowcast/baselinenowcast/issues/336)).
- Use the `as_reporting_triangle` and `baselinenowcast` workflow in the
  vignette which walks through a nowcasting example applied to syndromic
  surveillance data in the U.S.
- Add `preprocess` parameter to
  [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
  to control handling of negative values in reporting triangles. Set to
  `preprocess_negative_values` by default to redistribute negative
  values to earlier delays, or set to `NULL` to preserve negative PMF
  entries. This allows the method to work with reporting corrections
  that result in net downward adjustments at specific delays
  ([\#278](https://github.com/epinowcast/baselinenowcast/issues/278)).
- Export
  [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md)
  function to allow users to manually handle negative values in
  reporting triangles by redistributing them to earlier delays
  ([\#278](https://github.com/epinowcast/baselinenowcast/issues/278)).
- Add a
  [`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md)
  method which ingests a data.frame with one or more strata to nowcast
  and returns a `baselinenowcast_df` object.
- Add converters between `reporting_triangle` and ChainLadder triangle
  formats via
  [`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md)
  and
  [`as_reporting_triangle.triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.triangle.md),
  enabling use of ChainLadder’s mature claims reserving methods
  alongside baselinenowcast’s nowcasting functionality.
- Add a
  [`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
  method which ingests a `reporting_triangle` object and produces a
  `baselinenowcast_df` object which is a data.frame with nowcasts and
  associated metadata.
- Add a
  [`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md)
  S3 method which ingests a data.frame or matrix and returns a
  `reporting_triangle` object, which will be used as an input to the
  eventual
  [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
  function.
- Add a vignette which demonstrates how to pre-process for nowcasting
  syndromic surveillance system data using the U.S. National Syndromic
  Surveillance Program (NSSP) data as a case study, and then use
  `baselinenowcast` to nowcast cases of a specific syndromic
  surveillance definition.
- Add intermediate-level grouping functions to generate a point nowcast
  from a reporting triangle and to take a point nowcast and estimate and
  apply uncertainty.
- Refactor `estimate_uncertainty` to take in an error model function, an
  aggregator function for aggregating across reference times, and an
  aggregator function for aggregating across delays.

### Documentation

- Add a vignette which walks through the low-level function interface on
  the same nowcasting problem as in the Getting Started vignette.
- Modify the Getting Started vignette to use the
  [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
  wrapper function.
- Improve PMF validation message to be more informative when the delay
  PMF does not sum to approximately 1. The message now shows the actual
  sum and clarifies that this may be expected when working with downward
  corrections or incomplete data
  ([\#148](https://github.com/epinowcast/baselinenowcast/issues/148),
  [\#278](https://github.com/epinowcast/baselinenowcast/issues/278)).
- Include the pre-print as well as the package in the citation.

## baselinenowcast 0.0.0.1.000

### Package

- Replace argument names with more complete versions where possible.
- Replace most function names with action-oriented function naming.
- Add a check to ensure that there are sufficient non-zero values in the
  reporting triangle.
- Add a check to ensure that sufficient `n` are specified for the delay
  estimate.
- Change the requirement so that the number of rows used for delay
  estimation need not be greater than or equal to the number of columns,
  but instead that at least one row contains a full set of observations.
- Add support for passing in a restricted set of functions to the
  `estimate_dispersion()` function to transform the “target” dataset
  across reference dates.
- Implements a safe iterator in the step where retrospective point
  nowcasts are generated from a list of retrospective reporting
  triangles, ensuring that the iterations continue even if not all
  retrospective point nowcasts can be generated.
- Replace function named `replace_lower_right_with_NA()` with
  `generate_triangle()`.
- Removes requirement that all elements of the reporting triangle are
  integers.
- Modify the method used to estimate dispersion. Instead of estimating
  dispersion on individual elements of the nowcast matrix, we now
  estimate dispersion on the predicted nowcasts summed across reporting
  dates as a function of forecast horizon.
- Standardises naming of objects that are matrices vs vectors and
  objects that contain observations, point estimates, and probabilistic
  draws.
- Modifies functions that estimate a delay and generate a point nowcast
  to ensure that they throw an error/warning if the first element of the
  delay PMF is 0.
- Adjusts function to estimate delay distribution to be able to handle
  complete and partially complete reporting triangles.
- Add function to convert a list of expected observed reporting squares
  to a long tidy dataframe indexed by reference time, delay, and a
  sample index.
- Implement zero-handling in the bottom left of the reporting triangle
  when applying the delay to generate a point nowcast.
- Add function to generate a list of expected observed reporting
  squares.
- Add function to generate an expected observed reporting square from a
  point nowcast and a vector of dispersion parameters.
- Add function to estimate dispersion parameters from a match list of
  nowcasts and observed reporting triangles.
- Add functions to generate retrospective nowcasts from a single
  reporting triangle.
- Refactor uncertainty estimation to use a user-facing function to
  generate retrospective reporting triangles.
- Introduced function to estimate the uncertainty from a triangle to be
  nowcasted and a delay distribution.
- Introduced functions to get the delay estimate and apply the delay,
  and used these in the Getting Started vignette.
- Added package skeleton.

### Documentation

- Modify vignette to be consistent with the decided upon defaults for
  the number of reference times used for delay estimation and
  uncertainty.
- Add new logo.
- Methods write-up as a separate vignette.

### Bug fixes

- Fix internal checks that ensure there is sufficient data for the
  specified target choice, using the number of NA rows rather than the
  number of columns as a proxy for the number of horizons.
- Bug fix to change the requirement so that the sum of the elements in
  the `structure` vector must not be greater than the number of columns.
- Introduced function to estimate the uncertainty from a triangle to be
  nowcasted and a delay distribution.
- Introduced functions to get the delay estimate and apply the delay,
  and used these in the Getting Started vignette.
- Added package skeleton.
