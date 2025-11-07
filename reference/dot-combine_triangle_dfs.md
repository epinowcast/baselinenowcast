# Combine triangle data.frames

This function ingests a dataframe with case counts indexed by reference
dates and report dates for one or more strata and sums all the case
counts across the shared set of reference and report dates. Note that
this may be a subset of the unique set of reference and report date
combinations in the original data.

## Usage

``` r
.combine_triangle_dfs(data, strata_cols = NULL)
```

## Arguments

- data:

  Data.frame containing the incident count of cases by reference date
  and report date for one or more strata.

- strata_cols:

  Vector of character strings indicating the names of the columns in
  `data` that determine how to stratify the data for nowcasting. The
  unique combinations of the entries in the `strata_cols` denote the
  unit of a single nowcast. Within a strata, there can be no repeated
  unique combinations of reference dates and report dates. Default is
  `NULL` which assumes that the data.frame being passed in represents a
  single strata (only one nowcast will be produced). All columns that
  are not part of the `strata_cols` will be removed.

## Value

`result` Data.frame with the same column names for reference date,
report date, and case count as in `data` but summed across all strata in
the original data.
