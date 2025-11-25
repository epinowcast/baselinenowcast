# Preprocess negative values in the reporting triangle

Takes in a reporting triangle and returns it with negative values of
reporting handled by redistributing them to earlier delays (from longer
delay to shorter). This is useful when dealing with reporting
corrections that can result in negative incremental counts.

When negative values are detected, they are set to zero and the negative
amount is subtracted from the count at the next earlier delay (moving
from right to left in each row). This process continues until either the
negative value is fully absorbed or the first delay is reached.

This code was adapted from code written (under an MIT license) by the
Karlsruhe Institute of Technology RESPINOW German Hospitalization
Nowcasting Hub. Modified from
https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R
\#nolint

## Usage

``` r
preprocess_negative_values(reporting_triangle, validate = TRUE)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

A
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object with negative values handled via redistribution to earlier
delays.

## Details

Use this function when:

- Your data contains reporting corrections that result in negative
  counts

- You want to preserve the total count while handling negatives

- You need a delay distribution that sums to 1 or a CDF that is weakly
  increasing

Set `preprocess = NULL` in
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
when:

- Your data naturally has negative PMF entries (e.g., from differencing)

- You want to preserve the original structure including negatives

- You are working with corrections that should be reflected as negative
  probabilities

## See also

Delay distribution estimation functions
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)

## Examples

``` r
# Using example dataset with negative values from corrections
# Preprocess to handle negatives
preprocessed <- preprocess_negative_values(example_downward_corr_rt)
#> ℹ Negative values detected in reporting triangle and will be corrected
preprocessed
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-08
#> Max delay: 3
#> Structure: 1
#> 
#>              0  1  2  3
#> 2024-01-01 100 40  0 10
#> 2024-01-02 120 45  0 15
#> 2024-01-03 110 43  0 12
#> 2024-01-04 130 47  0 18
#> 2024-01-05 115 44  0 14
#> 2024-01-06 125 46  0 NA
#> 2024-01-07 105 62 NA NA
#> 2024-01-08  95 NA NA NA
```
