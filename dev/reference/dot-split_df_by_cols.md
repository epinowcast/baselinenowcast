# Split dataframe into a list of dataframes by the entries in the specified columns

Split dataframe into a list of dataframes by the entries in the
specified columns

## Usage

``` r
.split_df_by_cols(long_df, col_names)
```

## Arguments

- long_df:

  Data.frame to be split into a list of dataframes.

- col_names:

  Character string indicating the column names to be used to create the
  new data.frames.

## Value

List of data.frames named by the concatenated entries in col_names
