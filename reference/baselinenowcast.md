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
#>            reference_date pred_count draw output_type
#> 2024-01-01     2024-01-01   197.0000    1       point
#> 2024-01-02     2024-01-02   142.0000    1       point
#> 2024-01-03     2024-01-03   165.0000    1       point
#> 2024-01-04     2024-01-04   180.5919    1       point
#> 2024-01-05     2024-01-05   168.8218    1       point
#> 2024-01-06     2024-01-06   212.7238    1       point
#> 2024-01-07     2024-01-07   183.7663    1       point

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
#>     pred_count reference_date draw output_type
#> 1          197     2024-01-01    1     samples
#> 8          197     2024-01-01    2     samples
#> 15         197     2024-01-01    3     samples
#> 22         197     2024-01-01    4     samples
#> 29         197     2024-01-01    5     samples
#> 36         197     2024-01-01    6     samples
#> 43         197     2024-01-01    7     samples
#> 50         197     2024-01-01    8     samples
#> 57         197     2024-01-01    9     samples
#> 64         197     2024-01-01   10     samples
#> 71         197     2024-01-01   11     samples
#> 78         197     2024-01-01   12     samples
#> 85         197     2024-01-01   13     samples
#> 92         197     2024-01-01   14     samples
#> 99         197     2024-01-01   15     samples
#> 106        197     2024-01-01   16     samples
#> 113        197     2024-01-01   17     samples
#> 120        197     2024-01-01   18     samples
#> 127        197     2024-01-01   19     samples
#> 134        197     2024-01-01   20     samples
#> 141        197     2024-01-01   21     samples
#> 148        197     2024-01-01   22     samples
#> 155        197     2024-01-01   23     samples
#> 162        197     2024-01-01   24     samples
#> 169        197     2024-01-01   25     samples
#> 176        197     2024-01-01   26     samples
#> 183        197     2024-01-01   27     samples
#> 190        197     2024-01-01   28     samples
#> 197        197     2024-01-01   29     samples
#> 204        197     2024-01-01   30     samples
#> 211        197     2024-01-01   31     samples
#> 218        197     2024-01-01   32     samples
#> 225        197     2024-01-01   33     samples
#> 232        197     2024-01-01   34     samples
#> 239        197     2024-01-01   35     samples
#> 246        197     2024-01-01   36     samples
#> 253        197     2024-01-01   37     samples
#> 260        197     2024-01-01   38     samples
#> 267        197     2024-01-01   39     samples
#> 274        197     2024-01-01   40     samples
#> 281        197     2024-01-01   41     samples
#> 288        197     2024-01-01   42     samples
#> 295        197     2024-01-01   43     samples
#> 302        197     2024-01-01   44     samples
#> 309        197     2024-01-01   45     samples
#> 316        197     2024-01-01   46     samples
#> 323        197     2024-01-01   47     samples
#> 330        197     2024-01-01   48     samples
#> 337        197     2024-01-01   49     samples
#> 344        197     2024-01-01   50     samples
#> 351        197     2024-01-01   51     samples
#> 358        197     2024-01-01   52     samples
#> 365        197     2024-01-01   53     samples
#> 372        197     2024-01-01   54     samples
#> 379        197     2024-01-01   55     samples
#> 386        197     2024-01-01   56     samples
#> 393        197     2024-01-01   57     samples
#> 400        197     2024-01-01   58     samples
#> 407        197     2024-01-01   59     samples
#> 414        197     2024-01-01   60     samples
#> 421        197     2024-01-01   61     samples
#> 428        197     2024-01-01   62     samples
#> 435        197     2024-01-01   63     samples
#> 442        197     2024-01-01   64     samples
#> 449        197     2024-01-01   65     samples
#> 456        197     2024-01-01   66     samples
#> 463        197     2024-01-01   67     samples
#> 470        197     2024-01-01   68     samples
#> 477        197     2024-01-01   69     samples
#> 484        197     2024-01-01   70     samples
#> 491        197     2024-01-01   71     samples
#> 498        197     2024-01-01   72     samples
#> 505        197     2024-01-01   73     samples
#> 512        197     2024-01-01   74     samples
#> 519        197     2024-01-01   75     samples
#> 526        197     2024-01-01   76     samples
#> 533        197     2024-01-01   77     samples
#> 540        197     2024-01-01   78     samples
#> 547        197     2024-01-01   79     samples
#> 554        197     2024-01-01   80     samples
#> 561        197     2024-01-01   81     samples
#> 568        197     2024-01-01   82     samples
#> 575        197     2024-01-01   83     samples
#> 582        197     2024-01-01   84     samples
#> 589        197     2024-01-01   85     samples
#> 596        197     2024-01-01   86     samples
#> 603        197     2024-01-01   87     samples
#> 610        197     2024-01-01   88     samples
#> 617        197     2024-01-01   89     samples
#> 624        197     2024-01-01   90     samples
#> 631        197     2024-01-01   91     samples
#> 638        197     2024-01-01   92     samples
#> 645        197     2024-01-01   93     samples
#> 652        197     2024-01-01   94     samples
#> 659        197     2024-01-01   95     samples
#> 666        197     2024-01-01   96     samples
#> 673        197     2024-01-01   97     samples
#> 680        197     2024-01-01   98     samples
#> 687        197     2024-01-01   99     samples
#> 694        197     2024-01-01  100     samples
#> 2          142     2024-01-02    1     samples
#> 9          142     2024-01-02    2     samples
#> 16         142     2024-01-02    3     samples
#> 23         142     2024-01-02    4     samples
#> 30         142     2024-01-02    5     samples
#> 37         142     2024-01-02    6     samples
#> 44         142     2024-01-02    7     samples
#> 51         142     2024-01-02    8     samples
#> 58         142     2024-01-02    9     samples
#> 65         142     2024-01-02   10     samples
#> 72         142     2024-01-02   11     samples
#> 79         142     2024-01-02   12     samples
#> 86         142     2024-01-02   13     samples
#> 93         142     2024-01-02   14     samples
#> 100        142     2024-01-02   15     samples
#> 107        142     2024-01-02   16     samples
#> 114        142     2024-01-02   17     samples
#> 121        142     2024-01-02   18     samples
#> 128        142     2024-01-02   19     samples
#> 135        142     2024-01-02   20     samples
#> 142        142     2024-01-02   21     samples
#> 149        142     2024-01-02   22     samples
#> 156        142     2024-01-02   23     samples
#> 163        142     2024-01-02   24     samples
#> 170        142     2024-01-02   25     samples
#> 177        142     2024-01-02   26     samples
#> 184        142     2024-01-02   27     samples
#> 191        142     2024-01-02   28     samples
#> 198        142     2024-01-02   29     samples
#> 205        142     2024-01-02   30     samples
#> 212        142     2024-01-02   31     samples
#> 219        142     2024-01-02   32     samples
#> 226        142     2024-01-02   33     samples
#> 233        142     2024-01-02   34     samples
#> 240        142     2024-01-02   35     samples
#> 247        142     2024-01-02   36     samples
#> 254        142     2024-01-02   37     samples
#> 261        142     2024-01-02   38     samples
#> 268        142     2024-01-02   39     samples
#> 275        142     2024-01-02   40     samples
#> 282        142     2024-01-02   41     samples
#> 289        142     2024-01-02   42     samples
#> 296        142     2024-01-02   43     samples
#> 303        142     2024-01-02   44     samples
#> 310        142     2024-01-02   45     samples
#> 317        142     2024-01-02   46     samples
#> 324        142     2024-01-02   47     samples
#> 331        142     2024-01-02   48     samples
#> 338        142     2024-01-02   49     samples
#> 345        142     2024-01-02   50     samples
#> 352        142     2024-01-02   51     samples
#> 359        142     2024-01-02   52     samples
#> 366        142     2024-01-02   53     samples
#> 373        142     2024-01-02   54     samples
#> 380        142     2024-01-02   55     samples
#> 387        142     2024-01-02   56     samples
#> 394        142     2024-01-02   57     samples
#> 401        142     2024-01-02   58     samples
#> 408        142     2024-01-02   59     samples
#> 415        142     2024-01-02   60     samples
#> 422        142     2024-01-02   61     samples
#> 429        142     2024-01-02   62     samples
#> 436        142     2024-01-02   63     samples
#> 443        142     2024-01-02   64     samples
#> 450        142     2024-01-02   65     samples
#> 457        142     2024-01-02   66     samples
#> 464        142     2024-01-02   67     samples
#> 471        142     2024-01-02   68     samples
#> 478        142     2024-01-02   69     samples
#> 485        142     2024-01-02   70     samples
#> 492        142     2024-01-02   71     samples
#> 499        142     2024-01-02   72     samples
#> 506        142     2024-01-02   73     samples
#> 513        142     2024-01-02   74     samples
#> 520        142     2024-01-02   75     samples
#> 527        142     2024-01-02   76     samples
#> 534        142     2024-01-02   77     samples
#> 541        142     2024-01-02   78     samples
#> 548        142     2024-01-02   79     samples
#> 555        142     2024-01-02   80     samples
#> 562        142     2024-01-02   81     samples
#> 569        142     2024-01-02   82     samples
#> 576        142     2024-01-02   83     samples
#> 583        142     2024-01-02   84     samples
#> 590        142     2024-01-02   85     samples
#> 597        142     2024-01-02   86     samples
#> 604        142     2024-01-02   87     samples
#> 611        142     2024-01-02   88     samples
#> 618        142     2024-01-02   89     samples
#> 625        142     2024-01-02   90     samples
#> 632        142     2024-01-02   91     samples
#> 639        142     2024-01-02   92     samples
#> 646        142     2024-01-02   93     samples
#> 653        142     2024-01-02   94     samples
#> 660        142     2024-01-02   95     samples
#> 667        142     2024-01-02   96     samples
#> 674        142     2024-01-02   97     samples
#> 681        142     2024-01-02   98     samples
#> 688        142     2024-01-02   99     samples
#> 695        142     2024-01-02  100     samples
#> 3          165     2024-01-03    1     samples
#> 10         165     2024-01-03    2     samples
#> 17         165     2024-01-03    3     samples
#> 24         165     2024-01-03    4     samples
#> 31         165     2024-01-03    5     samples
#> 38         165     2024-01-03    6     samples
#> 45         165     2024-01-03    7     samples
#> 52         165     2024-01-03    8     samples
#> 59         165     2024-01-03    9     samples
#> 66         165     2024-01-03   10     samples
#> 73         165     2024-01-03   11     samples
#> 80         165     2024-01-03   12     samples
#> 87         165     2024-01-03   13     samples
#> 94         165     2024-01-03   14     samples
#> 101        165     2024-01-03   15     samples
#> 108        165     2024-01-03   16     samples
#> 115        165     2024-01-03   17     samples
#> 122        165     2024-01-03   18     samples
#> 129        165     2024-01-03   19     samples
#> 136        165     2024-01-03   20     samples
#> 143        165     2024-01-03   21     samples
#> 150        165     2024-01-03   22     samples
#> 157        165     2024-01-03   23     samples
#> 164        165     2024-01-03   24     samples
#> 171        165     2024-01-03   25     samples
#> 178        165     2024-01-03   26     samples
#> 185        165     2024-01-03   27     samples
#> 192        165     2024-01-03   28     samples
#> 199        165     2024-01-03   29     samples
#> 206        165     2024-01-03   30     samples
#> 213        165     2024-01-03   31     samples
#> 220        165     2024-01-03   32     samples
#> 227        165     2024-01-03   33     samples
#> 234        165     2024-01-03   34     samples
#> 241        165     2024-01-03   35     samples
#> 248        165     2024-01-03   36     samples
#> 255        165     2024-01-03   37     samples
#> 262        165     2024-01-03   38     samples
#> 269        165     2024-01-03   39     samples
#> 276        165     2024-01-03   40     samples
#> 283        165     2024-01-03   41     samples
#> 290        165     2024-01-03   42     samples
#> 297        165     2024-01-03   43     samples
#> 304        165     2024-01-03   44     samples
#> 311        165     2024-01-03   45     samples
#> 318        165     2024-01-03   46     samples
#> 325        165     2024-01-03   47     samples
#> 332        165     2024-01-03   48     samples
#> 339        165     2024-01-03   49     samples
#> 346        165     2024-01-03   50     samples
#> 353        165     2024-01-03   51     samples
#> 360        165     2024-01-03   52     samples
#> 367        165     2024-01-03   53     samples
#> 374        165     2024-01-03   54     samples
#> 381        165     2024-01-03   55     samples
#> 388        165     2024-01-03   56     samples
#> 395        165     2024-01-03   57     samples
#> 402        165     2024-01-03   58     samples
#> 409        165     2024-01-03   59     samples
#> 416        165     2024-01-03   60     samples
#> 423        165     2024-01-03   61     samples
#> 430        165     2024-01-03   62     samples
#> 437        165     2024-01-03   63     samples
#> 444        165     2024-01-03   64     samples
#> 451        165     2024-01-03   65     samples
#> 458        165     2024-01-03   66     samples
#> 465        165     2024-01-03   67     samples
#> 472        165     2024-01-03   68     samples
#> 479        165     2024-01-03   69     samples
#> 486        165     2024-01-03   70     samples
#> 493        165     2024-01-03   71     samples
#> 500        165     2024-01-03   72     samples
#> 507        165     2024-01-03   73     samples
#> 514        165     2024-01-03   74     samples
#> 521        165     2024-01-03   75     samples
#> 528        165     2024-01-03   76     samples
#> 535        165     2024-01-03   77     samples
#> 542        165     2024-01-03   78     samples
#> 549        165     2024-01-03   79     samples
#> 556        165     2024-01-03   80     samples
#> 563        165     2024-01-03   81     samples
#> 570        165     2024-01-03   82     samples
#> 577        165     2024-01-03   83     samples
#> 584        165     2024-01-03   84     samples
#> 591        165     2024-01-03   85     samples
#> 598        165     2024-01-03   86     samples
#> 605        165     2024-01-03   87     samples
#> 612        165     2024-01-03   88     samples
#> 619        165     2024-01-03   89     samples
#> 626        165     2024-01-03   90     samples
#> 633        165     2024-01-03   91     samples
#> 640        165     2024-01-03   92     samples
#> 647        165     2024-01-03   93     samples
#> 654        165     2024-01-03   94     samples
#> 661        165     2024-01-03   95     samples
#> 668        165     2024-01-03   96     samples
#> 675        165     2024-01-03   97     samples
#> 682        165     2024-01-03   98     samples
#> 689        165     2024-01-03   99     samples
#> 696        165     2024-01-03  100     samples
#> 4          186     2024-01-04    1     samples
#> 11         183     2024-01-04    2     samples
#> 18         180     2024-01-04    3     samples
#> 25         182     2024-01-04    4     samples
#> 32         182     2024-01-04    5     samples
#> 39         176     2024-01-04    6     samples
#> 46         185     2024-01-04    7     samples
#> 53         183     2024-01-04    8     samples
#> 60         181     2024-01-04    9     samples
#> 67         177     2024-01-04   10     samples
#> 74         181     2024-01-04   11     samples
#> 81         185     2024-01-04   12     samples
#> 88         182     2024-01-04   13     samples
#> 95         179     2024-01-04   14     samples
#> 102        182     2024-01-04   15     samples
#> 109        174     2024-01-04   16     samples
#> 116        182     2024-01-04   17     samples
#> 123        177     2024-01-04   18     samples
#> 130        185     2024-01-04   19     samples
#> 137        179     2024-01-04   20     samples
#> 144        177     2024-01-04   21     samples
#> 151        181     2024-01-04   22     samples
#> 158        178     2024-01-04   23     samples
#> 165        187     2024-01-04   24     samples
#> 172        177     2024-01-04   25     samples
#> 179        186     2024-01-04   26     samples
#> 186        188     2024-01-04   27     samples
#> 193        184     2024-01-04   28     samples
#> 200        192     2024-01-04   29     samples
#> 207        184     2024-01-04   30     samples
#> 214        184     2024-01-04   31     samples
#> 221        176     2024-01-04   32     samples
#> 228        180     2024-01-04   33     samples
#> 235        182     2024-01-04   34     samples
#> 242        186     2024-01-04   35     samples
#> 249        187     2024-01-04   36     samples
#> 256        179     2024-01-04   37     samples
#> 263        181     2024-01-04   38     samples
#> 270        178     2024-01-04   39     samples
#> 277        182     2024-01-04   40     samples
#> 284        175     2024-01-04   41     samples
#> 291        180     2024-01-04   42     samples
#> 298        184     2024-01-04   43     samples
#> 305        177     2024-01-04   44     samples
#> 312        178     2024-01-04   45     samples
#> 319        177     2024-01-04   46     samples
#> 326        178     2024-01-04   47     samples
#> 333        179     2024-01-04   48     samples
#> 340        183     2024-01-04   49     samples
#> 347        181     2024-01-04   50     samples
#> 354        179     2024-01-04   51     samples
#> 361        182     2024-01-04   52     samples
#> 368        185     2024-01-04   53     samples
#> 375        179     2024-01-04   54     samples
#> 382        179     2024-01-04   55     samples
#> 389        182     2024-01-04   56     samples
#> 396        183     2024-01-04   57     samples
#> 403        185     2024-01-04   58     samples
#> 410        183     2024-01-04   59     samples
#> 417        182     2024-01-04   60     samples
#> 424        178     2024-01-04   61     samples
#> 431        180     2024-01-04   62     samples
#> 438        181     2024-01-04   63     samples
#> 445        181     2024-01-04   64     samples
#> 452        178     2024-01-04   65     samples
#> 459        177     2024-01-04   66     samples
#> 466        179     2024-01-04   67     samples
#> 473        188     2024-01-04   68     samples
#> 480        174     2024-01-04   69     samples
#> 487        177     2024-01-04   70     samples
#> 494        176     2024-01-04   71     samples
#> 501        177     2024-01-04   72     samples
#> 508        183     2024-01-04   73     samples
#> 515        178     2024-01-04   74     samples
#> 522        182     2024-01-04   75     samples
#> 529        175     2024-01-04   76     samples
#> 536        182     2024-01-04   77     samples
#> 543        178     2024-01-04   78     samples
#> 550        184     2024-01-04   79     samples
#> 557        179     2024-01-04   80     samples
#> 564        186     2024-01-04   81     samples
#> 571        185     2024-01-04   82     samples
#> 578        182     2024-01-04   83     samples
#> 585        187     2024-01-04   84     samples
#> 592        177     2024-01-04   85     samples
#> 599        183     2024-01-04   86     samples
#> 606        178     2024-01-04   87     samples
#> 613        182     2024-01-04   88     samples
#> 620        178     2024-01-04   89     samples
#> 627        184     2024-01-04   90     samples
#> 634        178     2024-01-04   91     samples
#> 641        178     2024-01-04   92     samples
#> 648        183     2024-01-04   93     samples
#> 655        182     2024-01-04   94     samples
#> 662        188     2024-01-04   95     samples
#> 669        179     2024-01-04   96     samples
#> 676        177     2024-01-04   97     samples
#> 683        175     2024-01-04   98     samples
#> 690        184     2024-01-04   99     samples
#> 697        186     2024-01-04  100     samples
#> 5          160     2024-01-05    1     samples
#> 12         167     2024-01-05    2     samples
#> 19         158     2024-01-05    3     samples
#> 26         170     2024-01-05    4     samples
#> 33         153     2024-01-05    5     samples
#> 40         177     2024-01-05    6     samples
#> 47         173     2024-01-05    7     samples
#> 54         158     2024-01-05    8     samples
#> 61         179     2024-01-05    9     samples
#> 68         182     2024-01-05   10     samples
#> 75         169     2024-01-05   11     samples
#> 82         169     2024-01-05   12     samples
#> 89         165     2024-01-05   13     samples
#> 96         156     2024-01-05   14     samples
#> 103        160     2024-01-05   15     samples
#> 110        172     2024-01-05   16     samples
#> 117        168     2024-01-05   17     samples
#> 124        172     2024-01-05   18     samples
#> 131        172     2024-01-05   19     samples
#> 138        159     2024-01-05   20     samples
#> 145        163     2024-01-05   21     samples
#> 152        158     2024-01-05   22     samples
#> 159        161     2024-01-05   23     samples
#> 166        175     2024-01-05   24     samples
#> 173        160     2024-01-05   25     samples
#> 180        157     2024-01-05   26     samples
#> 187        175     2024-01-05   27     samples
#> 194        165     2024-01-05   28     samples
#> 201        164     2024-01-05   29     samples
#> 208        151     2024-01-05   30     samples
#> 215        163     2024-01-05   31     samples
#> 222        163     2024-01-05   32     samples
#> 229        171     2024-01-05   33     samples
#> 236        169     2024-01-05   34     samples
#> 243        186     2024-01-05   35     samples
#> 250        155     2024-01-05   36     samples
#> 257        176     2024-01-05   37     samples
#> 264        166     2024-01-05   38     samples
#> 271        154     2024-01-05   39     samples
#> 278        165     2024-01-05   40     samples
#> 285        173     2024-01-05   41     samples
#> 292        167     2024-01-05   42     samples
#> 299        180     2024-01-05   43     samples
#> 306        167     2024-01-05   44     samples
#> 313        172     2024-01-05   45     samples
#> 320        178     2024-01-05   46     samples
#> 327        176     2024-01-05   47     samples
#> 334        160     2024-01-05   48     samples
#> 341        162     2024-01-05   49     samples
#> 348        162     2024-01-05   50     samples
#> 355        181     2024-01-05   51     samples
#> 362        164     2024-01-05   52     samples
#> 369        162     2024-01-05   53     samples
#> 376        156     2024-01-05   54     samples
#> 383        154     2024-01-05   55     samples
#> 390        163     2024-01-05   56     samples
#> 397        160     2024-01-05   57     samples
#> 404        178     2024-01-05   58     samples
#> 411        161     2024-01-05   59     samples
#> 418        162     2024-01-05   60     samples
#> 425        170     2024-01-05   61     samples
#> 432        168     2024-01-05   62     samples
#> 439        166     2024-01-05   63     samples
#> 446        165     2024-01-05   64     samples
#> 453        159     2024-01-05   65     samples
#> 460        183     2024-01-05   66     samples
#> 467        161     2024-01-05   67     samples
#> 474        159     2024-01-05   68     samples
#> 481        162     2024-01-05   69     samples
#> 488        177     2024-01-05   70     samples
#> 495        164     2024-01-05   71     samples
#> 502        167     2024-01-05   72     samples
#> 509        173     2024-01-05   73     samples
#> 516        168     2024-01-05   74     samples
#> 523        167     2024-01-05   75     samples
#> 530        176     2024-01-05   76     samples
#> 537        160     2024-01-05   77     samples
#> 544        160     2024-01-05   78     samples
#> 551        177     2024-01-05   79     samples
#> 558        173     2024-01-05   80     samples
#> 565        170     2024-01-05   81     samples
#> 572        173     2024-01-05   82     samples
#> 579        162     2024-01-05   83     samples
#> 586        178     2024-01-05   84     samples
#> 593        162     2024-01-05   85     samples
#> 600        169     2024-01-05   86     samples
#> 607        163     2024-01-05   87     samples
#> 614        178     2024-01-05   88     samples
#> 621        160     2024-01-05   89     samples
#> 628        179     2024-01-05   90     samples
#> 635        182     2024-01-05   91     samples
#> 642        198     2024-01-05   92     samples
#> 649        158     2024-01-05   93     samples
#> 656        170     2024-01-05   94     samples
#> 663        167     2024-01-05   95     samples
#> 670        166     2024-01-05   96     samples
#> 677        159     2024-01-05   97     samples
#> 684        168     2024-01-05   98     samples
#> 691        174     2024-01-05   99     samples
#> 698        153     2024-01-05  100     samples
#> 6          194     2024-01-06    1     samples
#> 13         213     2024-01-06    2     samples
#> 20         202     2024-01-06    3     samples
#> 27         186     2024-01-06    4     samples
#> 34         224     2024-01-06    5     samples
#> 41         204     2024-01-06    6     samples
#> 48         199     2024-01-06    7     samples
#> 55         203     2024-01-06    8     samples
#> 62         189     2024-01-06    9     samples
#> 69         250     2024-01-06   10     samples
#> 76         179     2024-01-06   11     samples
#> 83         214     2024-01-06   12     samples
#> 90         232     2024-01-06   13     samples
#> 97         238     2024-01-06   14     samples
#> 104        180     2024-01-06   15     samples
#> 111        162     2024-01-06   16     samples
#> 118        209     2024-01-06   17     samples
#> 125        237     2024-01-06   18     samples
#> 132        251     2024-01-06   19     samples
#> 139        179     2024-01-06   20     samples
#> 146        201     2024-01-06   21     samples
#> 153        194     2024-01-06   22     samples
#> 160        190     2024-01-06   23     samples
#> 167        203     2024-01-06   24     samples
#> 174        200     2024-01-06   25     samples
#> 181        187     2024-01-06   26     samples
#> 188        201     2024-01-06   27     samples
#> 195        191     2024-01-06   28     samples
#> 202        206     2024-01-06   29     samples
#> 209        215     2024-01-06   30     samples
#> 216        188     2024-01-06   31     samples
#> 223        229     2024-01-06   32     samples
#> 230        201     2024-01-06   33     samples
#> 237        225     2024-01-06   34     samples
#> 244        196     2024-01-06   35     samples
#> 251        181     2024-01-06   36     samples
#> 258        205     2024-01-06   37     samples
#> 265        216     2024-01-06   38     samples
#> 272        233     2024-01-06   39     samples
#> 279        200     2024-01-06   40     samples
#> 286        228     2024-01-06   41     samples
#> 293        195     2024-01-06   42     samples
#> 300        192     2024-01-06   43     samples
#> 307        204     2024-01-06   44     samples
#> 314        222     2024-01-06   45     samples
#> 321        202     2024-01-06   46     samples
#> 328        213     2024-01-06   47     samples
#> 335        179     2024-01-06   48     samples
#> 342        228     2024-01-06   49     samples
#> 349        214     2024-01-06   50     samples
#> 356        222     2024-01-06   51     samples
#> 363        191     2024-01-06   52     samples
#> 370        209     2024-01-06   53     samples
#> 377        224     2024-01-06   54     samples
#> 384        229     2024-01-06   55     samples
#> 391        192     2024-01-06   56     samples
#> 398        198     2024-01-06   57     samples
#> 405        219     2024-01-06   58     samples
#> 412        237     2024-01-06   59     samples
#> 419        180     2024-01-06   60     samples
#> 426        228     2024-01-06   61     samples
#> 433        209     2024-01-06   62     samples
#> 440        219     2024-01-06   63     samples
#> 447        225     2024-01-06   64     samples
#> 454        190     2024-01-06   65     samples
#> 461        174     2024-01-06   66     samples
#> 468        167     2024-01-06   67     samples
#> 475        184     2024-01-06   68     samples
#> 482        219     2024-01-06   69     samples
#> 489        250     2024-01-06   70     samples
#> 496        207     2024-01-06   71     samples
#> 503        215     2024-01-06   72     samples
#> 510        199     2024-01-06   73     samples
#> 517        216     2024-01-06   74     samples
#> 524        181     2024-01-06   75     samples
#> 531        208     2024-01-06   76     samples
#> 538        183     2024-01-06   77     samples
#> 545        182     2024-01-06   78     samples
#> 552        248     2024-01-06   79     samples
#> 559        184     2024-01-06   80     samples
#> 566        180     2024-01-06   81     samples
#> 573        195     2024-01-06   82     samples
#> 580        196     2024-01-06   83     samples
#> 587        185     2024-01-06   84     samples
#> 594        196     2024-01-06   85     samples
#> 601        205     2024-01-06   86     samples
#> 608        213     2024-01-06   87     samples
#> 615        205     2024-01-06   88     samples
#> 622        219     2024-01-06   89     samples
#> 629        203     2024-01-06   90     samples
#> 636        257     2024-01-06   91     samples
#> 643        185     2024-01-06   92     samples
#> 650        216     2024-01-06   93     samples
#> 657        195     2024-01-06   94     samples
#> 664        202     2024-01-06   95     samples
#> 671        202     2024-01-06   96     samples
#> 678        229     2024-01-06   97     samples
#> 685        170     2024-01-06   98     samples
#> 692        209     2024-01-06   99     samples
#> 699        179     2024-01-06  100     samples
#> 7          191     2024-01-07    1     samples
#> 14         191     2024-01-07    2     samples
#> 21         181     2024-01-07    3     samples
#> 28         176     2024-01-07    4     samples
#> 35         194     2024-01-07    5     samples
#> 42         164     2024-01-07    6     samples
#> 49         204     2024-01-07    7     samples
#> 56         193     2024-01-07    8     samples
#> 63         193     2024-01-07    9     samples
#> 70         191     2024-01-07   10     samples
#> 77         197     2024-01-07   11     samples
#> 84         164     2024-01-07   12     samples
#> 91         195     2024-01-07   13     samples
#> 98         172     2024-01-07   14     samples
#> 105        177     2024-01-07   15     samples
#> 112        179     2024-01-07   16     samples
#> 119        185     2024-01-07   17     samples
#> 126        175     2024-01-07   18     samples
#> 133        199     2024-01-07   19     samples
#> 140        159     2024-01-07   20     samples
#> 147        169     2024-01-07   21     samples
#> 154        183     2024-01-07   22     samples
#> 161        161     2024-01-07   23     samples
#> 168        198     2024-01-07   24     samples
#> 175        168     2024-01-07   25     samples
#> 182        189     2024-01-07   26     samples
#> 189        172     2024-01-07   27     samples
#> 196        163     2024-01-07   28     samples
#> 203        179     2024-01-07   29     samples
#> 210        186     2024-01-07   30     samples
#> 217        189     2024-01-07   31     samples
#> 224        171     2024-01-07   32     samples
#> 231        175     2024-01-07   33     samples
#> 238        173     2024-01-07   34     samples
#> 245        169     2024-01-07   35     samples
#> 252        175     2024-01-07   36     samples
#> 259        170     2024-01-07   37     samples
#> 266        193     2024-01-07   38     samples
#> 273        169     2024-01-07   39     samples
#> 280        176     2024-01-07   40     samples
#> 287        205     2024-01-07   41     samples
#> 294        173     2024-01-07   42     samples
#> 301        186     2024-01-07   43     samples
#> 308        192     2024-01-07   44     samples
#> 315        180     2024-01-07   45     samples
#> 322        199     2024-01-07   46     samples
#> 329        193     2024-01-07   47     samples
#> 336        162     2024-01-07   48     samples
#> 343        184     2024-01-07   49     samples
#> 350        182     2024-01-07   50     samples
#> 357        180     2024-01-07   51     samples
#> 364        191     2024-01-07   52     samples
#> 371        178     2024-01-07   53     samples
#> 378        168     2024-01-07   54     samples
#> 385        190     2024-01-07   55     samples
#> 392        167     2024-01-07   56     samples
#> 399        185     2024-01-07   57     samples
#> 406        184     2024-01-07   58     samples
#> 413        187     2024-01-07   59     samples
#> 420        179     2024-01-07   60     samples
#> 427        172     2024-01-07   61     samples
#> 434        184     2024-01-07   62     samples
#> 441        182     2024-01-07   63     samples
#> 448        174     2024-01-07   64     samples
#> 455        171     2024-01-07   65     samples
#> 462        193     2024-01-07   66     samples
#> 469        180     2024-01-07   67     samples
#> 476        187     2024-01-07   68     samples
#> 483        188     2024-01-07   69     samples
#> 490        160     2024-01-07   70     samples
#> 497        190     2024-01-07   71     samples
#> 504        180     2024-01-07   72     samples
#> 511        177     2024-01-07   73     samples
#> 518        172     2024-01-07   74     samples
#> 525        195     2024-01-07   75     samples
#> 532        184     2024-01-07   76     samples
#> 539        180     2024-01-07   77     samples
#> 546        187     2024-01-07   78     samples
#> 553        183     2024-01-07   79     samples
#> 560        182     2024-01-07   80     samples
#> 567        192     2024-01-07   81     samples
#> 574        163     2024-01-07   82     samples
#> 581        171     2024-01-07   83     samples
#> 588        172     2024-01-07   84     samples
#> 595        172     2024-01-07   85     samples
#> 602        177     2024-01-07   86     samples
#> 609        174     2024-01-07   87     samples
#> 616        169     2024-01-07   88     samples
#> 623        189     2024-01-07   89     samples
#> 630        180     2024-01-07   90     samples
#> 637        176     2024-01-07   91     samples
#> 644        200     2024-01-07   92     samples
#> 651        185     2024-01-07   93     samples
#> 658        195     2024-01-07   94     samples
#> 665        178     2024-01-07   95     samples
#> 672        205     2024-01-07   96     samples
#> 679        182     2024-01-07   97     samples
#> 686        175     2024-01-07   98     samples
#> 693        174     2024-01-07   99     samples
#> 700        203     2024-01-07  100     samples
```
