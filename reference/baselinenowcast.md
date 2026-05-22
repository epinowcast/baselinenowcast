# Generate a nowcast

This function ingests data to be nowcasted and generates a a
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
which contains a probabilistic or point estimate of the final case
counts at each reference date in the `data`. See
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
for details on the input requirements.

## Usage

``` r
baselinenowcast(
  data,
  scale_factor = 3,
  prop_delay = 0.5,
  output_type = c("samples", "point"),
  draws = 1000,
  uncertainty_model = fit_by_horizon,
  uncertainty_sampler = sample_nb,
  ...
)
```

## Arguments

- data:

  Data to be nowcasted

- scale_factor:

  Numeric value indicating the multiplicative factor on the maximum
  delay to be used for estimation of delay and uncertainty. Default is
  `3`.

- prop_delay:

  Numeric value \<1 indicating what proportion of all reference times in
  the reporting triangle to be used for delay estimation. Default is
  `0.5`.

- output_type:

  Character string indicating whether the output should be samples
  (`"samples"`) from the estimate with full uncertainty or whether to
  return the point estimate (`"point"`). Default is `"samples"`. If
  `"point"`estimates are specified, the minimum number of reference
  times needed is the number needed for delay estimation, otherwise, if
  `"samples"` are specified, at least 2 additional reference times are
  required for uncertainty estimation.

- draws:

  Integer indicating the number of probabilistic draws to include if
  `output_type` is `"samples"`. Default is 1000.

- uncertainty_model:

  Function that ingests a matrix of observations and a matrix of
  predictions and returns a vector that can be used to apply uncertainty
  using the same error model. Default is `fit_by_horizon` with arguments
  of `obs` matrix of observations and `pred` the matrix of predictions
  that fits each column (horizon) to a negative binomial observation
  model by default. The user can specify a different fitting model by
  replacing the `fit_model` argument in `fit_by_horizon`.

- uncertainty_sampler:

  Function that ingests a vector or matrix of predictions and a vector
  of uncertainty parameters and generates draws from the observation
  model. Default is `sample_nb` which expects arguments `pred` for the
  vector of predictions and uncertainty parameters for the corresponding
  vector of uncertainty parameters, and draws from a negative binomial
  for each element of the vector.

- ...:

  Additional arguments passed to methods.

## Value

Data.frame of class
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)

## See also

Main nowcasting interface functions
[`as_forecast_point.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_point.baselinenowcast_df.md),
[`as_forecast_sample.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_sample.baselinenowcast_df.md),
[`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md),
[`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md),
[`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)

## Examples

``` r
# Generate a point nowcast from a reporting triangle
nowcast <- baselinenowcast(
  example_reporting_triangle,
  output_type = "point"
)
#> Warning: 7 reference times available and 9 are specified.
#> ℹ All 7 reference times will be used.
#> ℹ 0.5 reference times were specified for delay estimation but 0.857 of reference times used for delay estimation.
#> ℹ `prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.
nowcast
#>            reference_date pred_count draw output_type nowcast
#> 2024-01-01     2024-01-01   197.0000    1       point   FALSE
#> 2024-01-02     2024-01-02   142.0000    1       point   FALSE
#> 2024-01-03     2024-01-03   165.0000    1       point   FALSE
#> 2024-01-04     2024-01-04   180.5919    1       point    TRUE
#> 2024-01-05     2024-01-05   168.8218    1       point    TRUE
#> 2024-01-06     2024-01-06   212.7238    1       point    TRUE
#> 2024-01-07     2024-01-07   183.7663    1       point    TRUE

# Generate probabilistic nowcast with samples
baselinenowcast(
  example_reporting_triangle,
  output_type = "samples",
  draws = 100
)
#> Warning: 7 reference times available and 9 are specified.
#> ℹ All 7 reference times will be used.
#> ℹ 0.5 reference times were specified for delay estimation but 0.714 of reference times used for delay estimation.
#> ℹ This is due to the minumim requirement for the number of retrospective nowcasts for uncertainty estimation (2).
#>     pred_count reference_date draw output_type nowcast
#> 1          197     2024-01-01    1     samples   FALSE
#> 8          197     2024-01-01    2     samples   FALSE
#> 15         197     2024-01-01    3     samples   FALSE
#> 22         197     2024-01-01    4     samples   FALSE
#> 29         197     2024-01-01    5     samples   FALSE
#> 36         197     2024-01-01    6     samples   FALSE
#> 43         197     2024-01-01    7     samples   FALSE
#> 50         197     2024-01-01    8     samples   FALSE
#> 57         197     2024-01-01    9     samples   FALSE
#> 64         197     2024-01-01   10     samples   FALSE
#> 71         197     2024-01-01   11     samples   FALSE
#> 78         197     2024-01-01   12     samples   FALSE
#> 85         197     2024-01-01   13     samples   FALSE
#> 92         197     2024-01-01   14     samples   FALSE
#> 99         197     2024-01-01   15     samples   FALSE
#> 106        197     2024-01-01   16     samples   FALSE
#> 113        197     2024-01-01   17     samples   FALSE
#> 120        197     2024-01-01   18     samples   FALSE
#> 127        197     2024-01-01   19     samples   FALSE
#> 134        197     2024-01-01   20     samples   FALSE
#> 141        197     2024-01-01   21     samples   FALSE
#> 148        197     2024-01-01   22     samples   FALSE
#> 155        197     2024-01-01   23     samples   FALSE
#> 162        197     2024-01-01   24     samples   FALSE
#> 169        197     2024-01-01   25     samples   FALSE
#> 176        197     2024-01-01   26     samples   FALSE
#> 183        197     2024-01-01   27     samples   FALSE
#> 190        197     2024-01-01   28     samples   FALSE
#> 197        197     2024-01-01   29     samples   FALSE
#> 204        197     2024-01-01   30     samples   FALSE
#> 211        197     2024-01-01   31     samples   FALSE
#> 218        197     2024-01-01   32     samples   FALSE
#> 225        197     2024-01-01   33     samples   FALSE
#> 232        197     2024-01-01   34     samples   FALSE
#> 239        197     2024-01-01   35     samples   FALSE
#> 246        197     2024-01-01   36     samples   FALSE
#> 253        197     2024-01-01   37     samples   FALSE
#> 260        197     2024-01-01   38     samples   FALSE
#> 267        197     2024-01-01   39     samples   FALSE
#> 274        197     2024-01-01   40     samples   FALSE
#> 281        197     2024-01-01   41     samples   FALSE
#> 288        197     2024-01-01   42     samples   FALSE
#> 295        197     2024-01-01   43     samples   FALSE
#> 302        197     2024-01-01   44     samples   FALSE
#> 309        197     2024-01-01   45     samples   FALSE
#> 316        197     2024-01-01   46     samples   FALSE
#> 323        197     2024-01-01   47     samples   FALSE
#> 330        197     2024-01-01   48     samples   FALSE
#> 337        197     2024-01-01   49     samples   FALSE
#> 344        197     2024-01-01   50     samples   FALSE
#> 351        197     2024-01-01   51     samples   FALSE
#> 358        197     2024-01-01   52     samples   FALSE
#> 365        197     2024-01-01   53     samples   FALSE
#> 372        197     2024-01-01   54     samples   FALSE
#> 379        197     2024-01-01   55     samples   FALSE
#> 386        197     2024-01-01   56     samples   FALSE
#> 393        197     2024-01-01   57     samples   FALSE
#> 400        197     2024-01-01   58     samples   FALSE
#> 407        197     2024-01-01   59     samples   FALSE
#> 414        197     2024-01-01   60     samples   FALSE
#> 421        197     2024-01-01   61     samples   FALSE
#> 428        197     2024-01-01   62     samples   FALSE
#> 435        197     2024-01-01   63     samples   FALSE
#> 442        197     2024-01-01   64     samples   FALSE
#> 449        197     2024-01-01   65     samples   FALSE
#> 456        197     2024-01-01   66     samples   FALSE
#> 463        197     2024-01-01   67     samples   FALSE
#> 470        197     2024-01-01   68     samples   FALSE
#> 477        197     2024-01-01   69     samples   FALSE
#> 484        197     2024-01-01   70     samples   FALSE
#> 491        197     2024-01-01   71     samples   FALSE
#> 498        197     2024-01-01   72     samples   FALSE
#> 505        197     2024-01-01   73     samples   FALSE
#> 512        197     2024-01-01   74     samples   FALSE
#> 519        197     2024-01-01   75     samples   FALSE
#> 526        197     2024-01-01   76     samples   FALSE
#> 533        197     2024-01-01   77     samples   FALSE
#> 540        197     2024-01-01   78     samples   FALSE
#> 547        197     2024-01-01   79     samples   FALSE
#> 554        197     2024-01-01   80     samples   FALSE
#> 561        197     2024-01-01   81     samples   FALSE
#> 568        197     2024-01-01   82     samples   FALSE
#> 575        197     2024-01-01   83     samples   FALSE
#> 582        197     2024-01-01   84     samples   FALSE
#> 589        197     2024-01-01   85     samples   FALSE
#> 596        197     2024-01-01   86     samples   FALSE
#> 603        197     2024-01-01   87     samples   FALSE
#> 610        197     2024-01-01   88     samples   FALSE
#> 617        197     2024-01-01   89     samples   FALSE
#> 624        197     2024-01-01   90     samples   FALSE
#> 631        197     2024-01-01   91     samples   FALSE
#> 638        197     2024-01-01   92     samples   FALSE
#> 645        197     2024-01-01   93     samples   FALSE
#> 652        197     2024-01-01   94     samples   FALSE
#> 659        197     2024-01-01   95     samples   FALSE
#> 666        197     2024-01-01   96     samples   FALSE
#> 673        197     2024-01-01   97     samples   FALSE
#> 680        197     2024-01-01   98     samples   FALSE
#> 687        197     2024-01-01   99     samples   FALSE
#> 694        197     2024-01-01  100     samples   FALSE
#> 2          142     2024-01-02    1     samples   FALSE
#> 9          142     2024-01-02    2     samples   FALSE
#> 16         142     2024-01-02    3     samples   FALSE
#> 23         142     2024-01-02    4     samples   FALSE
#> 30         142     2024-01-02    5     samples   FALSE
#> 37         142     2024-01-02    6     samples   FALSE
#> 44         142     2024-01-02    7     samples   FALSE
#> 51         142     2024-01-02    8     samples   FALSE
#> 58         142     2024-01-02    9     samples   FALSE
#> 65         142     2024-01-02   10     samples   FALSE
#> 72         142     2024-01-02   11     samples   FALSE
#> 79         142     2024-01-02   12     samples   FALSE
#> 86         142     2024-01-02   13     samples   FALSE
#> 93         142     2024-01-02   14     samples   FALSE
#> 100        142     2024-01-02   15     samples   FALSE
#> 107        142     2024-01-02   16     samples   FALSE
#> 114        142     2024-01-02   17     samples   FALSE
#> 121        142     2024-01-02   18     samples   FALSE
#> 128        142     2024-01-02   19     samples   FALSE
#> 135        142     2024-01-02   20     samples   FALSE
#> 142        142     2024-01-02   21     samples   FALSE
#> 149        142     2024-01-02   22     samples   FALSE
#> 156        142     2024-01-02   23     samples   FALSE
#> 163        142     2024-01-02   24     samples   FALSE
#> 170        142     2024-01-02   25     samples   FALSE
#> 177        142     2024-01-02   26     samples   FALSE
#> 184        142     2024-01-02   27     samples   FALSE
#> 191        142     2024-01-02   28     samples   FALSE
#> 198        142     2024-01-02   29     samples   FALSE
#> 205        142     2024-01-02   30     samples   FALSE
#> 212        142     2024-01-02   31     samples   FALSE
#> 219        142     2024-01-02   32     samples   FALSE
#> 226        142     2024-01-02   33     samples   FALSE
#> 233        142     2024-01-02   34     samples   FALSE
#> 240        142     2024-01-02   35     samples   FALSE
#> 247        142     2024-01-02   36     samples   FALSE
#> 254        142     2024-01-02   37     samples   FALSE
#> 261        142     2024-01-02   38     samples   FALSE
#> 268        142     2024-01-02   39     samples   FALSE
#> 275        142     2024-01-02   40     samples   FALSE
#> 282        142     2024-01-02   41     samples   FALSE
#> 289        142     2024-01-02   42     samples   FALSE
#> 296        142     2024-01-02   43     samples   FALSE
#> 303        142     2024-01-02   44     samples   FALSE
#> 310        142     2024-01-02   45     samples   FALSE
#> 317        142     2024-01-02   46     samples   FALSE
#> 324        142     2024-01-02   47     samples   FALSE
#> 331        142     2024-01-02   48     samples   FALSE
#> 338        142     2024-01-02   49     samples   FALSE
#> 345        142     2024-01-02   50     samples   FALSE
#> 352        142     2024-01-02   51     samples   FALSE
#> 359        142     2024-01-02   52     samples   FALSE
#> 366        142     2024-01-02   53     samples   FALSE
#> 373        142     2024-01-02   54     samples   FALSE
#> 380        142     2024-01-02   55     samples   FALSE
#> 387        142     2024-01-02   56     samples   FALSE
#> 394        142     2024-01-02   57     samples   FALSE
#> 401        142     2024-01-02   58     samples   FALSE
#> 408        142     2024-01-02   59     samples   FALSE
#> 415        142     2024-01-02   60     samples   FALSE
#> 422        142     2024-01-02   61     samples   FALSE
#> 429        142     2024-01-02   62     samples   FALSE
#> 436        142     2024-01-02   63     samples   FALSE
#> 443        142     2024-01-02   64     samples   FALSE
#> 450        142     2024-01-02   65     samples   FALSE
#> 457        142     2024-01-02   66     samples   FALSE
#> 464        142     2024-01-02   67     samples   FALSE
#> 471        142     2024-01-02   68     samples   FALSE
#> 478        142     2024-01-02   69     samples   FALSE
#> 485        142     2024-01-02   70     samples   FALSE
#> 492        142     2024-01-02   71     samples   FALSE
#> 499        142     2024-01-02   72     samples   FALSE
#> 506        142     2024-01-02   73     samples   FALSE
#> 513        142     2024-01-02   74     samples   FALSE
#> 520        142     2024-01-02   75     samples   FALSE
#> 527        142     2024-01-02   76     samples   FALSE
#> 534        142     2024-01-02   77     samples   FALSE
#> 541        142     2024-01-02   78     samples   FALSE
#> 548        142     2024-01-02   79     samples   FALSE
#> 555        142     2024-01-02   80     samples   FALSE
#> 562        142     2024-01-02   81     samples   FALSE
#> 569        142     2024-01-02   82     samples   FALSE
#> 576        142     2024-01-02   83     samples   FALSE
#> 583        142     2024-01-02   84     samples   FALSE
#> 590        142     2024-01-02   85     samples   FALSE
#> 597        142     2024-01-02   86     samples   FALSE
#> 604        142     2024-01-02   87     samples   FALSE
#> 611        142     2024-01-02   88     samples   FALSE
#> 618        142     2024-01-02   89     samples   FALSE
#> 625        142     2024-01-02   90     samples   FALSE
#> 632        142     2024-01-02   91     samples   FALSE
#> 639        142     2024-01-02   92     samples   FALSE
#> 646        142     2024-01-02   93     samples   FALSE
#> 653        142     2024-01-02   94     samples   FALSE
#> 660        142     2024-01-02   95     samples   FALSE
#> 667        142     2024-01-02   96     samples   FALSE
#> 674        142     2024-01-02   97     samples   FALSE
#> 681        142     2024-01-02   98     samples   FALSE
#> 688        142     2024-01-02   99     samples   FALSE
#> 695        142     2024-01-02  100     samples   FALSE
#> 3          165     2024-01-03    1     samples   FALSE
#> 10         165     2024-01-03    2     samples   FALSE
#> 17         165     2024-01-03    3     samples   FALSE
#> 24         165     2024-01-03    4     samples   FALSE
#> 31         165     2024-01-03    5     samples   FALSE
#> 38         165     2024-01-03    6     samples   FALSE
#> 45         165     2024-01-03    7     samples   FALSE
#> 52         165     2024-01-03    8     samples   FALSE
#> 59         165     2024-01-03    9     samples   FALSE
#> 66         165     2024-01-03   10     samples   FALSE
#> 73         165     2024-01-03   11     samples   FALSE
#> 80         165     2024-01-03   12     samples   FALSE
#> 87         165     2024-01-03   13     samples   FALSE
#> 94         165     2024-01-03   14     samples   FALSE
#> 101        165     2024-01-03   15     samples   FALSE
#> 108        165     2024-01-03   16     samples   FALSE
#> 115        165     2024-01-03   17     samples   FALSE
#> 122        165     2024-01-03   18     samples   FALSE
#> 129        165     2024-01-03   19     samples   FALSE
#> 136        165     2024-01-03   20     samples   FALSE
#> 143        165     2024-01-03   21     samples   FALSE
#> 150        165     2024-01-03   22     samples   FALSE
#> 157        165     2024-01-03   23     samples   FALSE
#> 164        165     2024-01-03   24     samples   FALSE
#> 171        165     2024-01-03   25     samples   FALSE
#> 178        165     2024-01-03   26     samples   FALSE
#> 185        165     2024-01-03   27     samples   FALSE
#> 192        165     2024-01-03   28     samples   FALSE
#> 199        165     2024-01-03   29     samples   FALSE
#> 206        165     2024-01-03   30     samples   FALSE
#> 213        165     2024-01-03   31     samples   FALSE
#> 220        165     2024-01-03   32     samples   FALSE
#> 227        165     2024-01-03   33     samples   FALSE
#> 234        165     2024-01-03   34     samples   FALSE
#> 241        165     2024-01-03   35     samples   FALSE
#> 248        165     2024-01-03   36     samples   FALSE
#> 255        165     2024-01-03   37     samples   FALSE
#> 262        165     2024-01-03   38     samples   FALSE
#> 269        165     2024-01-03   39     samples   FALSE
#> 276        165     2024-01-03   40     samples   FALSE
#> 283        165     2024-01-03   41     samples   FALSE
#> 290        165     2024-01-03   42     samples   FALSE
#> 297        165     2024-01-03   43     samples   FALSE
#> 304        165     2024-01-03   44     samples   FALSE
#> 311        165     2024-01-03   45     samples   FALSE
#> 318        165     2024-01-03   46     samples   FALSE
#> 325        165     2024-01-03   47     samples   FALSE
#> 332        165     2024-01-03   48     samples   FALSE
#> 339        165     2024-01-03   49     samples   FALSE
#> 346        165     2024-01-03   50     samples   FALSE
#> 353        165     2024-01-03   51     samples   FALSE
#> 360        165     2024-01-03   52     samples   FALSE
#> 367        165     2024-01-03   53     samples   FALSE
#> 374        165     2024-01-03   54     samples   FALSE
#> 381        165     2024-01-03   55     samples   FALSE
#> 388        165     2024-01-03   56     samples   FALSE
#> 395        165     2024-01-03   57     samples   FALSE
#> 402        165     2024-01-03   58     samples   FALSE
#> 409        165     2024-01-03   59     samples   FALSE
#> 416        165     2024-01-03   60     samples   FALSE
#> 423        165     2024-01-03   61     samples   FALSE
#> 430        165     2024-01-03   62     samples   FALSE
#> 437        165     2024-01-03   63     samples   FALSE
#> 444        165     2024-01-03   64     samples   FALSE
#> 451        165     2024-01-03   65     samples   FALSE
#> 458        165     2024-01-03   66     samples   FALSE
#> 465        165     2024-01-03   67     samples   FALSE
#> 472        165     2024-01-03   68     samples   FALSE
#> 479        165     2024-01-03   69     samples   FALSE
#> 486        165     2024-01-03   70     samples   FALSE
#> 493        165     2024-01-03   71     samples   FALSE
#> 500        165     2024-01-03   72     samples   FALSE
#> 507        165     2024-01-03   73     samples   FALSE
#> 514        165     2024-01-03   74     samples   FALSE
#> 521        165     2024-01-03   75     samples   FALSE
#> 528        165     2024-01-03   76     samples   FALSE
#> 535        165     2024-01-03   77     samples   FALSE
#> 542        165     2024-01-03   78     samples   FALSE
#> 549        165     2024-01-03   79     samples   FALSE
#> 556        165     2024-01-03   80     samples   FALSE
#> 563        165     2024-01-03   81     samples   FALSE
#> 570        165     2024-01-03   82     samples   FALSE
#> 577        165     2024-01-03   83     samples   FALSE
#> 584        165     2024-01-03   84     samples   FALSE
#> 591        165     2024-01-03   85     samples   FALSE
#> 598        165     2024-01-03   86     samples   FALSE
#> 605        165     2024-01-03   87     samples   FALSE
#> 612        165     2024-01-03   88     samples   FALSE
#> 619        165     2024-01-03   89     samples   FALSE
#> 626        165     2024-01-03   90     samples   FALSE
#> 633        165     2024-01-03   91     samples   FALSE
#> 640        165     2024-01-03   92     samples   FALSE
#> 647        165     2024-01-03   93     samples   FALSE
#> 654        165     2024-01-03   94     samples   FALSE
#> 661        165     2024-01-03   95     samples   FALSE
#> 668        165     2024-01-03   96     samples   FALSE
#> 675        165     2024-01-03   97     samples   FALSE
#> 682        165     2024-01-03   98     samples   FALSE
#> 689        165     2024-01-03   99     samples   FALSE
#> 696        165     2024-01-03  100     samples   FALSE
#> 4          177     2024-01-04    1     samples    TRUE
#> 11         175     2024-01-04    2     samples    TRUE
#> 18         188     2024-01-04    3     samples    TRUE
#> 25         186     2024-01-04    4     samples    TRUE
#> 32         185     2024-01-04    5     samples    TRUE
#> 39         178     2024-01-04    6     samples    TRUE
#> 46         181     2024-01-04    7     samples    TRUE
#> 53         175     2024-01-04    8     samples    TRUE
#> 60         180     2024-01-04    9     samples    TRUE
#> 67         180     2024-01-04   10     samples    TRUE
#> 74         176     2024-01-04   11     samples    TRUE
#> 81         188     2024-01-04   12     samples    TRUE
#> 88         175     2024-01-04   13     samples    TRUE
#> 95         182     2024-01-04   14     samples    TRUE
#> 102        178     2024-01-04   15     samples    TRUE
#> 109        177     2024-01-04   16     samples    TRUE
#> 116        181     2024-01-04   17     samples    TRUE
#> 123        182     2024-01-04   18     samples    TRUE
#> 130        177     2024-01-04   19     samples    TRUE
#> 137        185     2024-01-04   20     samples    TRUE
#> 144        179     2024-01-04   21     samples    TRUE
#> 151        177     2024-01-04   22     samples    TRUE
#> 158        181     2024-01-04   23     samples    TRUE
#> 165        178     2024-01-04   24     samples    TRUE
#> 172        187     2024-01-04   25     samples    TRUE
#> 179        177     2024-01-04   26     samples    TRUE
#> 186        186     2024-01-04   27     samples    TRUE
#> 193        188     2024-01-04   28     samples    TRUE
#> 200        184     2024-01-04   29     samples    TRUE
#> 207        192     2024-01-04   30     samples    TRUE
#> 214        184     2024-01-04   31     samples    TRUE
#> 221        184     2024-01-04   32     samples    TRUE
#> 228        176     2024-01-04   33     samples    TRUE
#> 235        180     2024-01-04   34     samples    TRUE
#> 242        182     2024-01-04   35     samples    TRUE
#> 249        186     2024-01-04   36     samples    TRUE
#> 256        187     2024-01-04   37     samples    TRUE
#> 263        179     2024-01-04   38     samples    TRUE
#> 270        181     2024-01-04   39     samples    TRUE
#> 277        178     2024-01-04   40     samples    TRUE
#> 284        182     2024-01-04   41     samples    TRUE
#> 291        175     2024-01-04   42     samples    TRUE
#> 298        180     2024-01-04   43     samples    TRUE
#> 305        184     2024-01-04   44     samples    TRUE
#> 312        177     2024-01-04   45     samples    TRUE
#> 319        178     2024-01-04   46     samples    TRUE
#> 326        177     2024-01-04   47     samples    TRUE
#> 333        178     2024-01-04   48     samples    TRUE
#> 340        179     2024-01-04   49     samples    TRUE
#> 347        183     2024-01-04   50     samples    TRUE
#> 354        181     2024-01-04   51     samples    TRUE
#> 361        179     2024-01-04   52     samples    TRUE
#> 368        182     2024-01-04   53     samples    TRUE
#> 375        185     2024-01-04   54     samples    TRUE
#> 382        179     2024-01-04   55     samples    TRUE
#> 389        179     2024-01-04   56     samples    TRUE
#> 396        182     2024-01-04   57     samples    TRUE
#> 403        183     2024-01-04   58     samples    TRUE
#> 410        185     2024-01-04   59     samples    TRUE
#> 417        183     2024-01-04   60     samples    TRUE
#> 424        182     2024-01-04   61     samples    TRUE
#> 431        178     2024-01-04   62     samples    TRUE
#> 438        180     2024-01-04   63     samples    TRUE
#> 445        181     2024-01-04   64     samples    TRUE
#> 452        181     2024-01-04   65     samples    TRUE
#> 459        178     2024-01-04   66     samples    TRUE
#> 466        177     2024-01-04   67     samples    TRUE
#> 473        179     2024-01-04   68     samples    TRUE
#> 480        188     2024-01-04   69     samples    TRUE
#> 487        174     2024-01-04   70     samples    TRUE
#> 494        177     2024-01-04   71     samples    TRUE
#> 501        176     2024-01-04   72     samples    TRUE
#> 508        177     2024-01-04   73     samples    TRUE
#> 515        183     2024-01-04   74     samples    TRUE
#> 522        178     2024-01-04   75     samples    TRUE
#> 529        182     2024-01-04   76     samples    TRUE
#> 536        175     2024-01-04   77     samples    TRUE
#> 543        182     2024-01-04   78     samples    TRUE
#> 550        178     2024-01-04   79     samples    TRUE
#> 557        184     2024-01-04   80     samples    TRUE
#> 564        179     2024-01-04   81     samples    TRUE
#> 571        186     2024-01-04   82     samples    TRUE
#> 578        185     2024-01-04   83     samples    TRUE
#> 585        182     2024-01-04   84     samples    TRUE
#> 592        187     2024-01-04   85     samples    TRUE
#> 599        177     2024-01-04   86     samples    TRUE
#> 606        183     2024-01-04   87     samples    TRUE
#> 613        178     2024-01-04   88     samples    TRUE
#> 620        182     2024-01-04   89     samples    TRUE
#> 627        178     2024-01-04   90     samples    TRUE
#> 634        184     2024-01-04   91     samples    TRUE
#> 641        178     2024-01-04   92     samples    TRUE
#> 648        178     2024-01-04   93     samples    TRUE
#> 655        183     2024-01-04   94     samples    TRUE
#> 662        182     2024-01-04   95     samples    TRUE
#> 669        188     2024-01-04   96     samples    TRUE
#> 676        179     2024-01-04   97     samples    TRUE
#> 683        177     2024-01-04   98     samples    TRUE
#> 690        175     2024-01-04   99     samples    TRUE
#> 697        184     2024-01-04  100     samples    TRUE
#> 5          170     2024-01-05    1     samples    TRUE
#> 12         165     2024-01-05    2     samples    TRUE
#> 19         159     2024-01-05    3     samples    TRUE
#> 26         166     2024-01-05    4     samples    TRUE
#> 33         175     2024-01-05    5     samples    TRUE
#> 40         166     2024-01-05    6     samples    TRUE
#> 47         171     2024-01-05    7     samples    TRUE
#> 54         161     2024-01-05    8     samples    TRUE
#> 61         188     2024-01-05    9     samples    TRUE
#> 68         162     2024-01-05   10     samples    TRUE
#> 75         173     2024-01-05   11     samples    TRUE
#> 82         175     2024-01-05   12     samples    TRUE
#> 89         175     2024-01-05   13     samples    TRUE
#> 96         155     2024-01-05   14     samples    TRUE
#> 103        160     2024-01-05   15     samples    TRUE
#> 110        172     2024-01-05   16     samples    TRUE
#> 117        174     2024-01-05   17     samples    TRUE
#> 124        168     2024-01-05   18     samples    TRUE
#> 131        172     2024-01-05   19     samples    TRUE
#> 138        172     2024-01-05   20     samples    TRUE
#> 145        159     2024-01-05   21     samples    TRUE
#> 152        163     2024-01-05   22     samples    TRUE
#> 159        158     2024-01-05   23     samples    TRUE
#> 166        161     2024-01-05   24     samples    TRUE
#> 173        175     2024-01-05   25     samples    TRUE
#> 180        160     2024-01-05   26     samples    TRUE
#> 187        157     2024-01-05   27     samples    TRUE
#> 194        175     2024-01-05   28     samples    TRUE
#> 201        165     2024-01-05   29     samples    TRUE
#> 208        164     2024-01-05   30     samples    TRUE
#> 215        151     2024-01-05   31     samples    TRUE
#> 222        163     2024-01-05   32     samples    TRUE
#> 229        163     2024-01-05   33     samples    TRUE
#> 236        171     2024-01-05   34     samples    TRUE
#> 243        169     2024-01-05   35     samples    TRUE
#> 250        186     2024-01-05   36     samples    TRUE
#> 257        155     2024-01-05   37     samples    TRUE
#> 264        176     2024-01-05   38     samples    TRUE
#> 271        166     2024-01-05   39     samples    TRUE
#> 278        154     2024-01-05   40     samples    TRUE
#> 285        165     2024-01-05   41     samples    TRUE
#> 292        173     2024-01-05   42     samples    TRUE
#> 299        167     2024-01-05   43     samples    TRUE
#> 306        180     2024-01-05   44     samples    TRUE
#> 313        167     2024-01-05   45     samples    TRUE
#> 320        172     2024-01-05   46     samples    TRUE
#> 327        178     2024-01-05   47     samples    TRUE
#> 334        176     2024-01-05   48     samples    TRUE
#> 341        160     2024-01-05   49     samples    TRUE
#> 348        162     2024-01-05   50     samples    TRUE
#> 355        162     2024-01-05   51     samples    TRUE
#> 362        181     2024-01-05   52     samples    TRUE
#> 369        164     2024-01-05   53     samples    TRUE
#> 376        162     2024-01-05   54     samples    TRUE
#> 383        156     2024-01-05   55     samples    TRUE
#> 390        154     2024-01-05   56     samples    TRUE
#> 397        163     2024-01-05   57     samples    TRUE
#> 404        160     2024-01-05   58     samples    TRUE
#> 411        178     2024-01-05   59     samples    TRUE
#> 418        161     2024-01-05   60     samples    TRUE
#> 425        162     2024-01-05   61     samples    TRUE
#> 432        170     2024-01-05   62     samples    TRUE
#> 439        168     2024-01-05   63     samples    TRUE
#> 446        166     2024-01-05   64     samples    TRUE
#> 453        165     2024-01-05   65     samples    TRUE
#> 460        159     2024-01-05   66     samples    TRUE
#> 467        183     2024-01-05   67     samples    TRUE
#> 474        161     2024-01-05   68     samples    TRUE
#> 481        159     2024-01-05   69     samples    TRUE
#> 488        162     2024-01-05   70     samples    TRUE
#> 495        177     2024-01-05   71     samples    TRUE
#> 502        164     2024-01-05   72     samples    TRUE
#> 509        167     2024-01-05   73     samples    TRUE
#> 516        173     2024-01-05   74     samples    TRUE
#> 523        168     2024-01-05   75     samples    TRUE
#> 530        167     2024-01-05   76     samples    TRUE
#> 537        176     2024-01-05   77     samples    TRUE
#> 544        160     2024-01-05   78     samples    TRUE
#> 551        160     2024-01-05   79     samples    TRUE
#> 558        177     2024-01-05   80     samples    TRUE
#> 565        173     2024-01-05   81     samples    TRUE
#> 572        170     2024-01-05   82     samples    TRUE
#> 579        173     2024-01-05   83     samples    TRUE
#> 586        162     2024-01-05   84     samples    TRUE
#> 593        178     2024-01-05   85     samples    TRUE
#> 600        162     2024-01-05   86     samples    TRUE
#> 607        169     2024-01-05   87     samples    TRUE
#> 614        163     2024-01-05   88     samples    TRUE
#> 621        178     2024-01-05   89     samples    TRUE
#> 628        160     2024-01-05   90     samples    TRUE
#> 635        179     2024-01-05   91     samples    TRUE
#> 642        182     2024-01-05   92     samples    TRUE
#> 649        198     2024-01-05   93     samples    TRUE
#> 656        158     2024-01-05   94     samples    TRUE
#> 663        170     2024-01-05   95     samples    TRUE
#> 670        167     2024-01-05   96     samples    TRUE
#> 677        166     2024-01-05   97     samples    TRUE
#> 684        159     2024-01-05   98     samples    TRUE
#> 691        168     2024-01-05   99     samples    TRUE
#> 698        174     2024-01-05  100     samples    TRUE
#> 6          190     2024-01-06    1     samples    TRUE
#> 13         223     2024-01-06    2     samples    TRUE
#> 20         226     2024-01-06    3     samples    TRUE
#> 27         161     2024-01-06    4     samples    TRUE
#> 34         214     2024-01-06    5     samples    TRUE
#> 41         193     2024-01-06    6     samples    TRUE
#> 48         193     2024-01-06    7     samples    TRUE
#> 55         236     2024-01-06    8     samples    TRUE
#> 62         226     2024-01-06    9     samples    TRUE
#> 69         225     2024-01-06   10     samples    TRUE
#> 76         173     2024-01-06   11     samples    TRUE
#> 83         193     2024-01-06   12     samples    TRUE
#> 90         229     2024-01-06   13     samples    TRUE
#> 97         196     2024-01-06   14     samples    TRUE
#> 104        186     2024-01-06   15     samples    TRUE
#> 111        216     2024-01-06   16     samples    TRUE
#> 118        162     2024-01-06   17     samples    TRUE
#> 125        209     2024-01-06   18     samples    TRUE
#> 132        237     2024-01-06   19     samples    TRUE
#> 139        251     2024-01-06   20     samples    TRUE
#> 146        179     2024-01-06   21     samples    TRUE
#> 153        201     2024-01-06   22     samples    TRUE
#> 160        194     2024-01-06   23     samples    TRUE
#> 167        190     2024-01-06   24     samples    TRUE
#> 174        203     2024-01-06   25     samples    TRUE
#> 181        200     2024-01-06   26     samples    TRUE
#> 188        187     2024-01-06   27     samples    TRUE
#> 195        201     2024-01-06   28     samples    TRUE
#> 202        191     2024-01-06   29     samples    TRUE
#> 209        206     2024-01-06   30     samples    TRUE
#> 216        215     2024-01-06   31     samples    TRUE
#> 223        188     2024-01-06   32     samples    TRUE
#> 230        229     2024-01-06   33     samples    TRUE
#> 237        201     2024-01-06   34     samples    TRUE
#> 244        225     2024-01-06   35     samples    TRUE
#> 251        196     2024-01-06   36     samples    TRUE
#> 258        181     2024-01-06   37     samples    TRUE
#> 265        205     2024-01-06   38     samples    TRUE
#> 272        216     2024-01-06   39     samples    TRUE
#> 279        233     2024-01-06   40     samples    TRUE
#> 286        200     2024-01-06   41     samples    TRUE
#> 293        228     2024-01-06   42     samples    TRUE
#> 300        195     2024-01-06   43     samples    TRUE
#> 307        192     2024-01-06   44     samples    TRUE
#> 314        204     2024-01-06   45     samples    TRUE
#> 321        222     2024-01-06   46     samples    TRUE
#> 328        202     2024-01-06   47     samples    TRUE
#> 335        213     2024-01-06   48     samples    TRUE
#> 342        179     2024-01-06   49     samples    TRUE
#> 349        228     2024-01-06   50     samples    TRUE
#> 356        214     2024-01-06   51     samples    TRUE
#> 363        222     2024-01-06   52     samples    TRUE
#> 370        191     2024-01-06   53     samples    TRUE
#> 377        209     2024-01-06   54     samples    TRUE
#> 384        224     2024-01-06   55     samples    TRUE
#> 391        229     2024-01-06   56     samples    TRUE
#> 398        192     2024-01-06   57     samples    TRUE
#> 405        198     2024-01-06   58     samples    TRUE
#> 412        219     2024-01-06   59     samples    TRUE
#> 419        237     2024-01-06   60     samples    TRUE
#> 426        180     2024-01-06   61     samples    TRUE
#> 433        228     2024-01-06   62     samples    TRUE
#> 440        209     2024-01-06   63     samples    TRUE
#> 447        219     2024-01-06   64     samples    TRUE
#> 454        225     2024-01-06   65     samples    TRUE
#> 461        190     2024-01-06   66     samples    TRUE
#> 468        174     2024-01-06   67     samples    TRUE
#> 475        167     2024-01-06   68     samples    TRUE
#> 482        184     2024-01-06   69     samples    TRUE
#> 489        219     2024-01-06   70     samples    TRUE
#> 496        250     2024-01-06   71     samples    TRUE
#> 503        207     2024-01-06   72     samples    TRUE
#> 510        215     2024-01-06   73     samples    TRUE
#> 517        199     2024-01-06   74     samples    TRUE
#> 524        216     2024-01-06   75     samples    TRUE
#> 531        181     2024-01-06   76     samples    TRUE
#> 538        208     2024-01-06   77     samples    TRUE
#> 545        183     2024-01-06   78     samples    TRUE
#> 552        182     2024-01-06   79     samples    TRUE
#> 559        248     2024-01-06   80     samples    TRUE
#> 566        184     2024-01-06   81     samples    TRUE
#> 573        180     2024-01-06   82     samples    TRUE
#> 580        195     2024-01-06   83     samples    TRUE
#> 587        196     2024-01-06   84     samples    TRUE
#> 594        185     2024-01-06   85     samples    TRUE
#> 601        196     2024-01-06   86     samples    TRUE
#> 608        205     2024-01-06   87     samples    TRUE
#> 615        213     2024-01-06   88     samples    TRUE
#> 622        205     2024-01-06   89     samples    TRUE
#> 629        219     2024-01-06   90     samples    TRUE
#> 636        203     2024-01-06   91     samples    TRUE
#> 643        257     2024-01-06   92     samples    TRUE
#> 650        185     2024-01-06   93     samples    TRUE
#> 657        216     2024-01-06   94     samples    TRUE
#> 664        195     2024-01-06   95     samples    TRUE
#> 671        202     2024-01-06   96     samples    TRUE
#> 678        202     2024-01-06   97     samples    TRUE
#> 685        229     2024-01-06   98     samples    TRUE
#> 692        170     2024-01-06   99     samples    TRUE
#> 699        209     2024-01-06  100     samples    TRUE
#> 7          167     2024-01-07    1     samples    TRUE
#> 14         179     2024-01-07    2     samples    TRUE
#> 21         180     2024-01-07    3     samples    TRUE
#> 28         187     2024-01-07    4     samples    TRUE
#> 35         163     2024-01-07    5     samples    TRUE
#> 42         187     2024-01-07    6     samples    TRUE
#> 49         190     2024-01-07    7     samples    TRUE
#> 56         183     2024-01-07    8     samples    TRUE
#> 63         171     2024-01-07    9     samples    TRUE
#> 70         199     2024-01-07   10     samples    TRUE
#> 77         197     2024-01-07   11     samples    TRUE
#> 84         188     2024-01-07   12     samples    TRUE
#> 91         184     2024-01-07   13     samples    TRUE
#> 98         178     2024-01-07   14     samples    TRUE
#> 105        194     2024-01-07   15     samples    TRUE
#> 112        154     2024-01-07   16     samples    TRUE
#> 119        179     2024-01-07   17     samples    TRUE
#> 126        185     2024-01-07   18     samples    TRUE
#> 133        175     2024-01-07   19     samples    TRUE
#> 140        199     2024-01-07   20     samples    TRUE
#> 147        159     2024-01-07   21     samples    TRUE
#> 154        169     2024-01-07   22     samples    TRUE
#> 161        183     2024-01-07   23     samples    TRUE
#> 168        161     2024-01-07   24     samples    TRUE
#> 175        198     2024-01-07   25     samples    TRUE
#> 182        168     2024-01-07   26     samples    TRUE
#> 189        189     2024-01-07   27     samples    TRUE
#> 196        172     2024-01-07   28     samples    TRUE
#> 203        163     2024-01-07   29     samples    TRUE
#> 210        179     2024-01-07   30     samples    TRUE
#> 217        186     2024-01-07   31     samples    TRUE
#> 224        189     2024-01-07   32     samples    TRUE
#> 231        171     2024-01-07   33     samples    TRUE
#> 238        175     2024-01-07   34     samples    TRUE
#> 245        173     2024-01-07   35     samples    TRUE
#> 252        169     2024-01-07   36     samples    TRUE
#> 259        175     2024-01-07   37     samples    TRUE
#> 266        170     2024-01-07   38     samples    TRUE
#> 273        193     2024-01-07   39     samples    TRUE
#> 280        169     2024-01-07   40     samples    TRUE
#> 287        176     2024-01-07   41     samples    TRUE
#> 294        205     2024-01-07   42     samples    TRUE
#> 301        173     2024-01-07   43     samples    TRUE
#> 308        186     2024-01-07   44     samples    TRUE
#> 315        192     2024-01-07   45     samples    TRUE
#> 322        180     2024-01-07   46     samples    TRUE
#> 329        199     2024-01-07   47     samples    TRUE
#> 336        193     2024-01-07   48     samples    TRUE
#> 343        162     2024-01-07   49     samples    TRUE
#> 350        184     2024-01-07   50     samples    TRUE
#> 357        182     2024-01-07   51     samples    TRUE
#> 364        180     2024-01-07   52     samples    TRUE
#> 371        191     2024-01-07   53     samples    TRUE
#> 378        178     2024-01-07   54     samples    TRUE
#> 385        168     2024-01-07   55     samples    TRUE
#> 392        190     2024-01-07   56     samples    TRUE
#> 399        167     2024-01-07   57     samples    TRUE
#> 406        185     2024-01-07   58     samples    TRUE
#> 413        184     2024-01-07   59     samples    TRUE
#> 420        187     2024-01-07   60     samples    TRUE
#> 427        179     2024-01-07   61     samples    TRUE
#> 434        172     2024-01-07   62     samples    TRUE
#> 441        184     2024-01-07   63     samples    TRUE
#> 448        182     2024-01-07   64     samples    TRUE
#> 455        174     2024-01-07   65     samples    TRUE
#> 462        171     2024-01-07   66     samples    TRUE
#> 469        193     2024-01-07   67     samples    TRUE
#> 476        180     2024-01-07   68     samples    TRUE
#> 483        187     2024-01-07   69     samples    TRUE
#> 490        188     2024-01-07   70     samples    TRUE
#> 497        160     2024-01-07   71     samples    TRUE
#> 504        190     2024-01-07   72     samples    TRUE
#> 511        180     2024-01-07   73     samples    TRUE
#> 518        177     2024-01-07   74     samples    TRUE
#> 525        172     2024-01-07   75     samples    TRUE
#> 532        195     2024-01-07   76     samples    TRUE
#> 539        184     2024-01-07   77     samples    TRUE
#> 546        180     2024-01-07   78     samples    TRUE
#> 553        187     2024-01-07   79     samples    TRUE
#> 560        183     2024-01-07   80     samples    TRUE
#> 567        182     2024-01-07   81     samples    TRUE
#> 574        192     2024-01-07   82     samples    TRUE
#> 581        163     2024-01-07   83     samples    TRUE
#> 588        171     2024-01-07   84     samples    TRUE
#> 595        172     2024-01-07   85     samples    TRUE
#> 602        172     2024-01-07   86     samples    TRUE
#> 609        177     2024-01-07   87     samples    TRUE
#> 616        174     2024-01-07   88     samples    TRUE
#> 623        169     2024-01-07   89     samples    TRUE
#> 630        189     2024-01-07   90     samples    TRUE
#> 637        180     2024-01-07   91     samples    TRUE
#> 644        176     2024-01-07   92     samples    TRUE
#> 651        200     2024-01-07   93     samples    TRUE
#> 658        185     2024-01-07   94     samples    TRUE
#> 665        195     2024-01-07   95     samples    TRUE
#> 672        178     2024-01-07   96     samples    TRUE
#> 679        205     2024-01-07   97     samples    TRUE
#> 686        182     2024-01-07   98     samples    TRUE
#> 693        175     2024-01-07   99     samples    TRUE
#> 700        174     2024-01-07  100     samples    TRUE
```
