# Create a `reporting_triangle` from a matrix

This method takes a matrix in the format of a reporting triangle, with
rows as reference dates and columns as delays and elements as incident
case counts and creates a
[`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object. See other
[`as_reporting_triangle.data.frame`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)for
other data input options.

## Usage

``` r
# S3 method for class 'matrix'
as_reporting_triangle(
  data,
  max_delay,
  strata = NULL,
  delays_unit = "days",
  reference_dates,
  ...
)
```

## Arguments

- data:

  Matrix of a reporting triangle where rows are reference times, columns
  are delays, and entries are the incident counts.

- max_delay:

  Integer indicating the maximum delay.

- strata:

  Character string indicating the strata. Default is NULL.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. For the matrix
  method, this is simply passed as an item in the `reporting_triangle`
  object which will later be used to create a nowcast data.frame. For
  the data.frame method, this is used to compute the delay in terms of
  the specified unit, and to expand the combinations of reference dates
  and delays to the complete set of combinations in the reporting
  triangle. Default is `"days"`.

- reference_dates:

  Vector of character strings indicating the reference dates
  corresponding to each row of the reporting triangle matrix (`data`).

- ...:

  Additional arguments not used.

## Value

A
[`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object

## See also

Other reporting_triangle:
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)

## Examples

``` r
rep_tri_mat <- matrix(
  c(
    1, 3, 5, 7, 9,
    4, 7, 8, 0, NA,
    9, 10, 0, NA, NA,
    3, 0, NA, NA, NA,
    6, NA, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)

reference_dates <- seq(
  from = as.Date("2025-01-01"),
  to = as.Date("2025-01-05"),
  by = "day"
)
max_delay <- 4
rep_tri <- as_reporting_triangle(
  data = rep_tri_mat,
  reference_dates = reference_dates,
  max_delay = max_delay
)
rep_tri
#> $reporting_triangle_matrix
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    3    5    7    9
#> [2,]    4    7    8    0   NA
#> [3,]    9   10    0   NA   NA
#> [4,]    3    0   NA   NA   NA
#> [5,]    6   NA   NA   NA   NA
#> 
#> $reference_dates
#> [1] "2025-01-01" "2025-01-02" "2025-01-03" "2025-01-04" "2025-01-05"
#> 
#> $structure
#> [1] 1
#> 
#> $max_delay
#> [1] 4
#> 
#> $delays_unit
#> [1] "days"
#> 
#> $strata
#> NULL
#> 
#> attr(,"class")
#> [1] "reporting_triangle"
```
