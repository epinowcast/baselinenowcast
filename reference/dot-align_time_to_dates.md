# Align integer time indices with reference dates

Internal helper function that merges reference dates into a nowcast
dataframe, replacing integer time indices with actual dates.

## Usage

``` r
.align_time_to_dates(baselinenowcast_df, reference_dates)
```

## Arguments

- baselinenowcast_df:

  Data.frame containing nowcast information with an integer `time`
  column representing time indices.

- reference_dates:

  Vector of reference dates corresponding to the time indices in
  `baselinenowcast_df`.

## Value

Data.frame with `reference_date` column added and `time` column removed.
