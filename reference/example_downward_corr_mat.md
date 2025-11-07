# Example reporting triangle with downward corrections

A synthetic reporting triangle demonstrating downward corrections at a
specific delay. This represents a realistic case where data quality
reviews at delay 2 consistently identify false positives or reclassify
cases, resulting in net downward adjustments that produce negative
values.

When estimated with `preprocess = NULL`, this triangle produces a PMF
with negative entries and a CDF that is not strictly increasing,
reflecting the downward correction process.

## Usage

``` r
example_downward_corr_mat
```

## Format

A matrix with 8 rows and 4 columns.

## Details

This example demonstrates relaxed assumptions for PMF and CDF when
working with downward corrections:

- With `preprocess = NULL`, the PMF can have negative entries

- The CDF may not be strictly increasing

- This reflects real reporting processes with systematic downward
  corrections

## See also

- [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
  with `preprocess = NULL` to preserve negative entries

- [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md)
  to handle negatives by redistribution

Example datasets
[`germany_covid19_hosp`](https://baselinenowcast.epinowcast.org/reference/germany_covid19_hosp.md),
[`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md),
[`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
