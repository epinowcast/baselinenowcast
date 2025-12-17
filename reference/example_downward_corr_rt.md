# Example reporting triangle with downward corrections

A
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object demonstrating how to handle systematic downward corrections in
reporting data. This represents a realistic case where data quality
reviews at delay 2 consistently identify false positives or reclassify
cases, producing negative values at that specific delay.

## Usage

``` r
example_downward_corr_rt
```

## Format

A
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object with 8 reference dates and 4 delays:

- reporting_triangle_matrix:

  8x4 matrix with negative values at delay 2

- reference_dates:

  8 dates starting from 2024-01-01

- delays_unit:

  "days"

## Details

Use this example to understand:

- How to work with negative corrections in delay distributions

- The impact of preprocessing negative values on delay estimation

- How PMFs and CDFs behave with systematic downward corrections

## See also

- [example_reporting_triangle](https://baselinenowcast.epinowcast.org/reference/example_reporting_triangle.md)
  for a clean example without corrections

- [`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
  specifically the `preprocess` argument for a description of how to
  remove negative values if desired.

Example datasets
[`example_reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/example_reporting_triangle.md),
[`germany_covid19_hosp`](https://baselinenowcast.epinowcast.org/reference/germany_covid19_hosp.md),
[`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md),
[`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)

## Examples

``` r
# View the example triangle with downward corrections
example_downward_corr_rt
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-08
#> Max delay: 3
#> Structure: 1
#> 
#>              0  1   2  3
#> 2024-01-01 100 60 -20 10
#> 2024-01-02 120 70 -25 15
#> 2024-01-03 110 65 -22 12
#> 2024-01-04 130 75 -28 18
#> 2024-01-05 115 68 -24 14
#> 2024-01-06 125 72 -26 NA
#> 2024-01-07 105 62  NA NA
#> 2024-01-08  95 NA  NA NA

# Estimate delay with and without preprocessing
delay_raw <- estimate_delay(example_downward_corr_rt, n = 5)
delay_processed <- estimate_delay(
  preprocess_negative_values(example_downward_corr_rt),
  n = 5
)
#> ℹ Negative values detected in reporting triangle and will be corrected

# Compare the resulting PMFs
delay_raw
#>           0           1           2           3 
#>  0.66544955  0.38806216 -0.14046823  0.08695652 
delay_processed
#>          0          1          2          3 
#> 0.64346536 0.26957812 0.00000000 0.08695652 
```
