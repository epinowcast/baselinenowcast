# Simple example reporting triangle for demonstrations

A basic
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object demonstrating standard structure with complete early reference
times and progressively incomplete recent times. Useful for simple
examples and tests.

## Usage

``` r
example_reporting_triangle
```

## Format

A
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object with 5 reference dates and 4 delays:

- reporting_triangle_matrix:

  5x4 matrix with counts

- reference_dates:

  5 dates starting from 2024-01-01

- delays_unit:

  "days"

## Details

This is a simple, clean example without complications like negative
values or unusual structures. Ideal for:

- Package examples demonstrating basic functionality

- Unit tests for standard cases

- Vignettes introducing nowcasting concepts

Use
[example_downward_corr_rt](https://baselinenowcast.epinowcast.org/reference/example_downward_corr_rt.md)
for examples with data quality corrections.

## See also

- [example_downward_corr_rt](https://baselinenowcast.epinowcast.org/reference/example_downward_corr_rt.md)
  for downward corrections example

- [`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md)
  to create reporting triangles

Example datasets
[`example_downward_corr_rt`](https://baselinenowcast.epinowcast.org/reference/example_downward_corr_rt.md),
[`germany_covid19_hosp`](https://baselinenowcast.epinowcast.org/reference/germany_covid19_hosp.md),
[`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md),
[`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)

## Examples

``` r
# View the example triangle
example_reporting_triangle
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-07
#> Max delay: 3
#> Structure: 1, 0, 1, 1
#> 
#>              0  1  2  3
#> 2024-01-01 100 55 30 12
#> 2024-01-02  70 40 24  8
#> 2024-01-03  80 50 25 10
#> 2024-01-04 100 50 20 NA
#> 2024-01-05  90 45 NA NA
#> 2024-01-06 110 NA NA NA
#> 2024-01-07  95 NA NA NA

if (FALSE) { # \dontrun{
# Use in nowcasting - requires complete rows for delay estimation
estimate_delay(example_reporting_triangle, n = 2)
} # }
```
