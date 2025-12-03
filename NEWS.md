# baselinenowcast 0.0.0.1000

## Breaking changes

-   `fill_triangle()` is deprecated in favour of `estimate_and_apply_delay()` which now supports optional delay estimation via the `delay_pmf` parameter (#334).
-   `fill_triangles()` is deprecated in favour of `estimate_and_apply_delays()` (#334).

## New features

-   `estimate_and_apply_delay()` now supports optional delay estimation.
    You can provide a pre-computed `delay_pmf` to skip estimation, or let the function estimate it automatically.
    Parameters `n` and `preprocess` are now explicit parameters rather than passed via `...` (#334).
-   New function `estimate_and_apply_delays()` provides batch processing of multiple reporting triangles, replacing `fill_triangles()` with a more consistent naming scheme (#334).

## Internal changes

-   Consolidated duplicate functionality between `fill_triangle()` and `estimate_and_apply_delay()` to reduce maintenance burden (#334).

## Other changes

-   Add a vignette which walks through the low-level function interface on the same nowcasting problem as in the Getting Started vignette.
-   Modify the Getting Started vignette to use the `baselinenowcast()` wrapper function.
-   Use the `as_reporting_triangle` and `baselinenowcast` workflow in the vignette which walks through a nowcasting example applied to syndromic surveillance data in the U.S.
-   Add `preprocess` parameter to `estimate_delay()` to control handling of negative values in reporting triangles. Set to `preprocess_negative_values` by default to redistribute negative values to earlier delays, or set to `NULL` to preserve negative PMF entries. This allows the method to work with reporting corrections that result in net downward adjustments at specific delays (#278).
-   Export `preprocess_negative_values()` function to allow users to manually handle negative values in reporting triangles by redistributing them to earlier delays (#278).
-   Improve PMF validation message to be more informative when the delay PMF does not sum to approximately 1. The message now shows the actual sum and clarifies that this may be expected when working with downward corrections or incomplete data (#148, #278).
-   Add `example_downward_corr_mat` data demonstrating a scenario with systematic downward corrections at a specific delay, producing a PMF with negative entries when estimated with `preprocess = NULL` (#278).
-   Add a `baselinenowcast.data.frame()` method which ingests a data.frame with one or more strata to nowcast and returns a `baselinenowcast_df` object.
-   Add converters between `reporting_triangle` and ChainLadder triangle formats via `as_ChainLadder_triangle()` and `as_reporting_triangle.triangle()`, enabling use of ChainLadder's mature claims reserving methods alongside baselinenowcast's nowcasting functionality.
-   Add a `baselinenowcast.reporting_triangle()` method which ingests a `reporting_triangle` object and produces a `baselinenowcast_df` object which is a data.frame with nowcasts and associated metadata.
-   Include the pre-print as well as the package in the citation.
-   Add a `as_reporting_triangle()` S3 method which ingests a data.frame or matrix and returns a `reporting_triangle` object, which will be used as an input to the eventual `baselinenowcast()` function.
-   Add a vignette which demonstrates how to pre-process for nowcasting syndromic surveillance system data using the U.S. National Syndromic Surveillance Program (NSSP) data as a case study, and then use `baselinenowcast` to nowcast cases of a specific syndromic surveillance definition.
-   Add intermediate-level grouping functions to generate a point nowcast from a reporting triangle and to take a point nowcast and estimate and apply uncertainty.
-   Refactor `estimate_uncertainty` to take in an error model function, an aggregator function for aggregating across reference times, and an aggregator function for aggregating across delays.
-   Fix internal checks that ensure there is sufficient data for the specified target choice, using the number of NA rows rather than the number of columns as a proxy for the number of horizons.
-   Replace argument names with more complete versions where possible.
-   Replace most function names with action-oriented function naming.
-   Add new logo.
-   Add a check to ensure that there are sufficient non-zero values in the reporting triangle.
-   Add a check to ensure that sufficient `n` are specified for the delay estimate.
-   Change the requirement so that the number of rows used for delay estimation need not be greater than or equal to the number of columns, but instead that at least one row contains a full set of observations.
-   Bug fix to change the requirement so that the sum of the elements in the `structure` vector must not be greater than the number of columns.
-   Add support for passing in a restricted set of functions to the `estimate_dispersion()` function to transform the "target" dataset across reference dates.
-   Implements a safe iterator in the step where retrospective point nowcasts are generated from a list of retrospective reporting triangles, ensuring that the iterations continue even if not all retrospective point nowcasts can be generated.
-   Modify vignette to be consistent with the decided upon defaults for the number of reference times used for delay estimation and uncertainty.
-   Replace function named `replace_lower_right_with_NA()` with `generate_triangle()`.
-   Removes requirement that all elements of the reporting triangle are integers.
-   Modify the method used to estimate dispersion. Instead of estimating dispersion on individual elements of the nowcast matrix, we now estimate dispersion on the predicted nowcasts summed across reporting dates as a function of forecast horizon.
-   Standardises naming of objects that are matrices vs vectors and objects that contain observations, point estimates, and probabilistic draws.
-   Modifies functions that estimate a delay and generate a point nowcast to ensure that they throw an error/warning if the first element of the delay PMF is 0.
-   Adjusts function to estimate delay distribution to be able to handle complete and partially complete reporting triangles.
-   Add function to convert a list of expected observed reporting squares to a long tidy dataframe indexed by reference time, delay, and a sample index.
-   Implement zero-handling in the bottom left of the reporting triangle when applying the delay to generate a point nowcast.
-   Add function to generate a list of expected observed reporting squares.
-   Add function to generate an expected observed reporting square from a point nowcast and a vector of dispersion parameters.
-   Add function to estimate dispersion parameters from a match list of nowcasts and observed reporting triangles.
-   Add functions to generate retrospective nowcasts from a single reporting triangle.
-   Refactor uncertainty estimation to use a user-facing function to generate retrospective reporting triangles.
-   Methods write-up as a separate vignette.
-   Introduced function to estimate the uncertainty from a triangle to be nowcasted and a delay distribution.
-   Introduced functions to get the delay estimate and apply the delay, and used these in the Getting Started vignette.
-   Added package skeleton.
