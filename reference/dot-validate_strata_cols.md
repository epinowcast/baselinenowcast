# Validate each of the strata columns passed to baselinenowcast

Validate each of the strata columns passed to baselinenowcast

## Usage

``` r
.validate_strata_cols(strata_cols, data)
```

## Arguments

- strata_cols:

  Vector of character strings indicating the names of the columns in
  `data` that determine how to stratify the data for nowcasting. The
  unique combinations of the entries in the `strata_cols` denote the
  unit of a single nowcast. Within a strata, there can be no repeated
  unique combinations of reference dates and report dates. Default is
  `NULL` which assumes that the data.frame being passed in represents a
  single strata (only one nowcast will be produced). All columns that
  are not part of the `strata_cols` will be removed.

- data:

  Data.frame in a long tidy format with counts by reference date and
  report date for one or more strata. Must contain the following
  columns: - `reference_date`: Column of type `Date` containing the
  dates of the primary event occurrence.

  - `report_date`: Column of type `Date` containing the dates of report
    of the primary event.

  - `count`: Column of numeric or integer indicating the new confirmed
    counts pertaining to that reference and report date. Additional
    columns indicating the columns which set the unit of a single can be
    included. The user can specify these columns with the `strata_cols`
    argument, otherwise it will be assumed that the `data` contains only
    data for a single strata.
