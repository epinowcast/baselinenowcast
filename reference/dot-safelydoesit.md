# Safe iterator

Safe iterator

## Usage

``` r
.safelydoesit(fun)
```

## Arguments

- fun:

  Function to wrap around

## Value

Function that returns a list with `result` and `error` components. On
success: `result` contains the function output and `error` is NULL. On
failure: `result` is NULL and `error` contains the error object.
