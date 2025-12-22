# Reporting Triangle Object

A `reporting_triangle` object contains the data and metadata needed for
nowcasting.

## Structure

A `reporting_triangle` is a matrix with class
`c("reporting_triangle", "matrix")`:

- Rows: Reference dates

- Columns: Delays (0, 1, 2, ...)

- Entries: Incident cases at each reference date and delay

- Row names: Reference dates as character

- Column names: Delays as character

Attributes:

- `delays_unit`: Character ("days", "weeks", "months", "years")

Reference dates are stored as row names and can be extracted using
[`get_reference_dates()`](https://baselinenowcast.epinowcast.org/reference/get_reference_dates.md).
The maximum delay can be obtained using
[`get_max_delay()`](https://baselinenowcast.epinowcast.org/reference/get_max_delay.md).
The structure can be computed using
[`get_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/get_reporting_structure.md).
See the corresponding
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
and
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
functions for more details on the required input formats to generate the
object.

## Working with reporting triangles

Reporting triangle objects provide:

**Inspection and display:**

- [`print()`](https://rdrr.io/r/base/print.html): Informative display
  with metadata

- [`summary()`](https://rdrr.io/r/base/summary.html): Statistics
  including completion, delays, and zeros

- [`head()`](https://rdrr.io/r/utils/head.html),
  [`tail()`](https://rdrr.io/r/utils/head.html): Extract first or last
  rows

- Standard matrix operations:
  [`rowSums()`](https://rdrr.io/r/base/colSums.html),
  [`colSums()`](https://rdrr.io/r/base/colSums.html)

**Subsetting and modification:**

- `[` and `[<-`: Extract or assign values with automatic validation

- Subsetting preserves class and attributes when result is a matrix

**Package functions:**

- [`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md):
  Estimate delay and generate point nowcast

- [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md):
  Extract delay distribution from triangle

- [`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md):
  Apply delay distribution for nowcasting

- [`truncate_to_row()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_row.md):
  Remove most recent rows

- [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md):
  Handle reporting corrections

## See also

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`as_reporting_triangle.triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.triangle.md),
[`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md),
[`get_delays_from_dates()`](https://baselinenowcast.epinowcast.org/reference/get_delays_from_dates.md),
[`get_delays_unit()`](https://baselinenowcast.epinowcast.org/reference/get_delays_unit.md),
[`get_max_delay()`](https://baselinenowcast.epinowcast.org/reference/get_max_delay.md),
[`get_mean_delay()`](https://baselinenowcast.epinowcast.org/reference/get_mean_delay.md),
[`get_quantile_delay()`](https://baselinenowcast.epinowcast.org/reference/get_quantile_delay.md),
[`get_reference_dates()`](https://baselinenowcast.epinowcast.org/reference/get_reference_dates.md),
[`get_report_dates()`](https://baselinenowcast.epinowcast.org/reference/get_report_dates.md),
[`get_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/get_reporting_structure.md),
[`head.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/head.reporting_triangle.md),
[`is_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/is_reporting_triangle.md),
[`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/new_reporting_triangle.md),
[`print.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/print.reporting_triangle.md),
[`summary.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/summary.reporting_triangle.md),
[`tail.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/tail.reporting_triangle.md),
[`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_delay.md),
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
# Create a reporting triangle from data
data <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data = data)
#> ℹ Using max_delay = 154 from data

# Use with low-level functions
filled <- estimate_and_apply_delay(rep_tri)
delay_pmf <- estimate_delay(rep_tri)
nowcast <- apply_delay(rep_tri, delay_pmf)

# Direct matrix operations
total_by_date <- rowSums(rep_tri, na.rm = TRUE)
total_by_delay <- colSums(rep_tri, na.rm = TRUE)

# Subsetting and inspection
recent <- tail(rep_tri, n = 10)
summary(rep_tri)
#> Reporting Triangle Summary
#> Dimensions: 159 x 155
#> Reference period: 2025-10-25 to 2026-04-01
#> Max delay: 154 days
#> Structure: 1
#> Most recent complete date: 2025-10-29 (442 cases)
#> Dates requiring nowcast: 154 (complete: 5)
#> Rows with negatives: 0
#> Zeros: 9905 (77.9% of non-NA values)
#> Zeros per row summary:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     0.0    18.5    63.0    62.3   100.5   145.0 
#> 
#> Mean delay summary (complete rows):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   3.615   4.298   4.416   4.414   4.679   5.059 
#> 
#> 99% quantile delay (complete rows):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    22.0    31.0    31.0    33.4    34.0    49.0 
```
