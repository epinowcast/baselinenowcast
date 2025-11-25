# Incident COVID-19 hospitalisations indexed by the date of positive test (reference date) and report date from Germany in 2021 and 2022.

Incident COVID-19 hospitalisations indexed by the date of positive test
(reference date) and report date from Germany in 2021 and 2022.

## Usage

``` r
germany_covid19_hosp
```

## Format

A data.frame with 140,630 rows and 6 columns.

- reference_date:

  Date of first positive COVID-19 test formatted in ISO8601 standards as
  YYYY-MM-DD.

- location:

  Character string indicating the location of the case counts

- age_group:

  Character string indicating the age group of the case counts.

- delay:

  Integer specifying the delay, in days, between the reference date and
  the report date

- count:

  Integer indicating the number of cases indexed by reference and report
  date.

- report_date:

  Date of case report, formatted in ISO8601 standards as YYYY-MM-DD.

## Source

This data comes directly from the preprocessed data in the [German
COVID-19 Nowcast
Hub](https://github.com/KITmetricslab/hospitalization-nowcast-hub/tree/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv).
\#nolint It contains incident case counts by age group in Germany.

## See also

Example datasets
[`example_downward_corr_rt`](https://baselinenowcast.epinowcast.org/reference/example_downward_corr_rt.md),
[`example_reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/example_reporting_triangle.md),
[`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md),
[`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
