# Estimate and apply delays to generate retrospective nowcasts

This function ingests a list of incomplete reporting triangles and
generates a list of point nowcast matrices, based on the delay estimated
in each triangle or the corresponding delay passed in. It uses the
specified `n` number of reference times to estimate the delay in each
retrospective reporting triangle.

## Usage

``` r
estimate_and_apply_delays(
  retro_reporting_triangles,
  n = min(sapply(retro_reporting_triangles, nrow)),
  delay_pmf = NULL,
  validate = TRUE
)
```

## Arguments

- retro_reporting_triangles:

  List of reporting triangles to generate nowcasts for. Typically
  created by
  [`apply_reporting_structures()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structures.md).

- n:

  Integer indicating the number of reference times (number of rows) to
  use to estimate the delay distribution for each reporting triangle.
  Default is the minimum of the number of rows of all the matrices in
  `retro_reporting_triangles`.

- delay_pmf:

  Vector or list of vectors of delays assumed to be indexed starting at
  the first delay column in each of the matrices in
  `retro_reporting_triangles`. If a list, must be of the same length as
  `retro_reporting_triangles`, with elements aligning. Default is
  `NULL`.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`point_nowcast_matrices` List of the same number of elements as the
input `retro_reporting_triangles` but with each reporting triangle
filled in based on the delay estimated in that reporting triangle.

## See also

High-level workflow wrapper functions
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md),
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
# Generate retrospective nowcasts using larger triangle
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data_as_of) |>
  truncate_to_delay(max_delay = 25) |>
  tail(n = 50)
#> ℹ Using max_delay = 154 from data
#> ℹ Truncating from max_delay = 154 to 25.
trunc_rts <- truncate_to_rows(rep_tri, n = 2)
retro_rts <- apply_reporting_structures(trunc_rts)
retro_pt_nowcast_mat_list <- estimate_and_apply_delays(retro_rts, n = 30)
retro_pt_nowcast_mat_list[1:2]
#> [[1]]
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2026-02-11 to 2026-03-31
#> Max delay: 25
#> Structure: 0
#> 
#> Showing last 10 of 49 rows
#> Showing first 10 of 26 columns
#> 
#>              0         1        2        3        4        5        6
#> 2026-03-22 246  95.00000 57.00000 19.00000 38.00000 14.00000 10.00000
#> 2026-03-23 210 131.00000 34.00000 50.00000 35.00000 12.00000  1.00000
#> 2026-03-24 221  96.00000 22.00000 10.00000 13.00000  6.00000  0.00000
#> 2026-03-25 291 129.00000 17.00000 26.00000 42.00000 29.00000 23.00000
#> 2026-03-26 179  96.00000 22.00000 50.00000  9.00000  8.00000 11.83200
#> 2026-03-27 284  40.00000 41.00000 54.00000 28.00000 16.55964 15.06626
#> 2026-03-28 217  78.00000 46.00000 14.00000 15.66955 13.73347 12.49481
#> 2026-03-29 336 161.00000 62.00000 37.81203 26.33456 23.08154 21.00036
#> 2026-03-30 296  53.00000 34.74663 25.96444 18.08256 15.84851 14.41923
#> 2026-03-31 210  81.31027 29.00940 21.67661 15.09603 13.23077 12.03741
#>                   7        8         9
#> 2026-03-22  0.00000  0.00000 10.000000
#> 2026-03-23 25.00000 20.00000  8.524254
#> 2026-03-24  5.00000 12.90819  6.351134
#> 2026-03-25 18.96593 19.92891  9.805748
#> 2026-03-26 12.79932 13.44889  6.617189
#> 2026-03-27 16.29823 17.12562  8.426363
#> 2026-03-28 13.51636 14.20237  6.987948
#> 2026-03-29 22.71789 23.87156 11.745771
#> 2026-03-30 15.59825 16.39007  8.064427
#> 2026-03-31 13.02154 13.68240  6.732091
#> 
#> Use print(x, n_rows = NULL, n_cols = NULL) to see all data
#> 
#> [[2]]
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2026-02-11 to 2026-03-30
#> Max delay: 25
#> Structure: 0
#> 
#> Showing last 10 of 48 rows
#> Showing first 10 of 26 columns
#> 
#>              0        1        2        3        4        5        6
#> 2026-03-21 280 155.0000 83.00000 46.00000 15.00000 22.00000  3.00000
#> 2026-03-22 246  95.0000 57.00000 19.00000 38.00000 14.00000 10.00000
#> 2026-03-23 210 131.0000 34.00000 50.00000 35.00000 12.00000  1.00000
#> 2026-03-24 221  96.0000 22.00000 10.00000 13.00000  6.00000  0.00000
#> 2026-03-25 291 129.0000 17.00000 26.00000 42.00000 29.00000 17.52451
#> 2026-03-26 179  96.0000 22.00000 50.00000  9.00000 13.45815 12.12692
#> 2026-03-27 284  40.0000 41.00000 54.00000 18.61340 16.54126 14.90528
#> 2026-03-28 217  78.0000 46.00000 23.15962 16.17885 14.37758 12.95547
#> 2026-03-29 336 161.0000 48.11429 37.00906 25.85500 22.97716 20.70501
#> 2026-03-30 296 115.3655 39.83053 30.63657 21.40276 19.02027 17.13925
#>                   7        8         9
#> 2026-03-21  4.00000 25.00000  0.000000
#> 2026-03-22  0.00000  0.00000  7.920419
#> 2026-03-23 25.00000 16.27173  8.503475
#> 2026-03-24 12.72228 12.44109  6.501506
#> 2026-03-25 19.06357 18.64268  9.742577
#> 2026-03-26 13.19168 12.90015  6.741420
#> 2026-03-27 16.21418 15.85607  8.286240
#> 2026-03-28 14.09304 13.78165  7.202111
#> 2026-03-29 22.52355 22.02644 11.510995
#> 2026-03-30 18.64445 18.23280  9.528365
#> 
#> Use print(x, n_rows = NULL, n_cols = NULL) to see all data
#> 

# Using a pre-computed delay PMF
delay <- estimate_delay(rep_tri, n = 30)
retro_pt_nowcast_mat_list <- estimate_and_apply_delays(
  retro_rts,
  n = 30,
  delay_pmf = delay
)
retro_pt_nowcast_mat_list[1:2]
#> [[1]]
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2026-02-11 to 2026-03-31
#> Max delay: 25
#> Structure: 0
#> 
#> Showing last 10 of 49 rows
#> Showing first 10 of 26 columns
#> 
#>              0        1        2        3        4        5        6
#> 2026-03-22 246  95.0000 57.00000 19.00000 38.00000 14.00000 10.00000
#> 2026-03-23 210 131.0000 34.00000 50.00000 35.00000 12.00000  1.00000
#> 2026-03-24 221  96.0000 22.00000 10.00000 13.00000  6.00000  0.00000
#> 2026-03-25 291 129.0000 17.00000 26.00000 42.00000 29.00000 23.00000
#> 2026-03-26 179  96.0000 22.00000 50.00000  9.00000  8.00000 11.34767
#> 2026-03-27 284  40.0000 41.00000 54.00000 28.00000 17.02961 14.46418
#> 2026-03-28 217  78.0000 46.00000 14.00000 16.59793 14.15859 12.02551
#> 2026-03-29 336 161.0000 62.00000 36.36184 27.82697 23.73820 20.16250
#> 2026-03-30 296  53.0000 35.04539 24.98807 19.12222 16.31209 13.85471
#> 2026-03-31 210  80.5459 29.18217 20.80681 15.92216 13.58208 11.53582
#>                   7        8         9
#> 2026-03-22  0.00000  0.00000 10.000000
#> 2026-03-23 25.00000 20.00000  7.409071
#> 2026-03-24  5.00000 12.50892  5.514545
#> 2026-03-25 19.19661 19.32020  8.517505
#> 2026-03-26 12.93833 13.02132  5.740450
#> 2026-03-27 16.49192 16.59796  7.317322
#> 2026-03-28 13.71123 13.79924  6.083421
#> 2026-03-29 22.98942 23.13761 10.200525
#> 2026-03-30 15.79697 15.89851  7.008948
#> 2026-03-31 13.15287 13.23725  5.835651
#> 
#> Use print(x, n_rows = NULL, n_cols = NULL) to see all data
#> 
#> [[2]]
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2026-02-11 to 2026-03-30
#> Max delay: 25
#> Structure: 0
#> 
#> Showing last 10 of 48 rows
#> Showing first 10 of 26 columns
#> 
#>              0        1        2        3        4        5        6
#> 2026-03-21 280 155.0000 83.00000 46.00000 15.00000 22.00000  3.00000
#> 2026-03-22 246  95.0000 57.00000 19.00000 38.00000 14.00000 10.00000
#> 2026-03-23 210 131.0000 34.00000 50.00000 35.00000 12.00000  1.00000
#> 2026-03-24 221  96.0000 22.00000 10.00000 13.00000  6.00000  0.00000
#> 2026-03-25 291 129.0000 17.00000 26.00000 42.00000 29.00000 16.64416
#> 2026-03-26 179  96.0000 22.00000 50.00000  9.00000 13.56468 11.52104
#> 2026-03-27 284  40.0000 41.00000 54.00000 19.58783 16.70931 14.19211
#> 2026-03-28 217  78.0000 46.00000 22.18970 16.98053 14.48499 12.30276
#> 2026-03-29 336 161.0000 49.89047 35.57460 27.22447 23.22420 19.72590
#> 2026-03-30 296 113.4439 41.10819 29.31166 22.43122 19.13503 16.25253
#>                   7        8        9
#> 2026-03-21  4.00000 25.00000 0.000000
#> 2026-03-22  0.00000  0.00000 6.851399
#> 2026-03-23 25.00000 16.69900 7.361869
#> 2026-03-24 12.68518 12.76654 5.628121
#> 2026-03-25 18.97764 19.09981 8.420338
#> 2026-03-26 13.13601 13.22029 5.828172
#> 2026-03-27 16.18169 16.28572 7.179663
#> 2026-03-28 14.02736 14.11742 6.223700
#> 2026-03-29 22.49159 22.63655 9.979619
#> 2026-03-30 18.53109 18.65036 8.222184
#> 
#> Use print(x, n_rows = NULL, n_cols = NULL) to see all data
#> 
```
