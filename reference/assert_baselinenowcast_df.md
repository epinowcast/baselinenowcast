# Assert validity of `baselinenowcast_df` objects

Assert validity of `baselinenowcast_df` objects

## Usage

``` r
assert_baselinenowcast_df(data)
```

## Arguments

- data:

  A
  [baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  object to check for validity.

## Value

Returns `NULL` invisibly. Throws an error if validation fails.

## See also

Main nowcasting interface functions
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md),
[`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md),
[`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)

## Examples

``` r
# Create a valid baselinenowcast_df object
valid_df <- data.frame(
  reference_date = as.Date("2024-01-01") + 0:4,
  pred_count = c(10, 15, 12, 18, 20),
  draw = 1,
  output_type = "point"
)
class(valid_df) <- c("baselinenowcast_df", "data.frame")

# Validate the object
assert_baselinenowcast_df(valid_df)
#> NULL
```
