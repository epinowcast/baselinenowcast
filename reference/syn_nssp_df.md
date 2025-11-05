# A synthetic dataset containing the number of incident cases indexed by reference date and report date. While data of this form could be from any source, this data is meant to represent the output of pre-processing the [syn_nssp_line_list](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md) dataset, which is a synthetic patient-level line list data from the United State's National Syndromic Surveillance System (NSSP).

A synthetic dataset containing the number of incident cases indexed by
reference date and report date. While data of this form could be from
any source, this data is meant to represent the output of pre-processing
the
[syn_nssp_line_list](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
dataset, which is a synthetic patient-level line list data from the
United State's National Syndromic Surveillance System (NSSP).

## Usage

``` r
syn_nssp_df
```

## Format

A data.frame with 3795 rows and 3 columns.

- reference_date:

  Date of the primary event occurred (e.g. date of hospital admissions,
  specimen collection date, symptom onset), formatted in ISO8601
  standards as YYYY-MM-DD.

- report_date:

  Date the event was reported into the surveillance system, formatted as
  YYYY-MM-DD.

- count:

  Number of incident events (e.g. cases) occurring on the specified
  reference date and reported on the report date.

## Source

Created for package demonstration, made to look like the output after
preprocessing the line-list data to obtain the number of incidence cases
of a specific syndromic surveillance definition, indexed by the date of
admission (reference date) and the date of the diagnoses being reported
to the surveillance system (report date) (e.g. a reporting triangle in
long format).

## See also

Example datasets
[`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
