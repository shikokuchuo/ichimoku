# Relative Numeric Representation

Produce a statistical summary of the latest numeric representation of
the ichimoku cloud chart relative to historical values. For determining
whether current trading falls within or outside of normal ranges.

## Usage

``` r
relative(x, order = FALSE, signif = 0.2, quietly)
```

## Arguments

- x:

  an ichimoku object.

- order:

  \[default FALSE\] set to TRUE to order the results by the absolute
  'z-score'.

- signif:

  \[default 0.2\] set a significance threshold for which if 'p' is equal
  or lower, the element will be starred with a '\*'.

- quietly:

  (optional) if set to TRUE, will suppress printing of additional output
  to the console and return quietly.

## Value

A data frame containing a statistical summary of the latest ichimoku
cloud chart representation in relation to historical values.

In addition, the time index of the latest observed values and total
number of datapoints are printed to the console.

## Details

'mean(X)' is the mean value for each element X, 'sd(X)' the standard
deviation, and 'X\[n\]' the nth or latest observed values.

'res' is the residual X\[n\] - mean(X) and represents a centred measure
of deviation for the latest observed value.

The 'z-score' (or standard score) is calculated as res / sd(X) and is a
centred and scaled measure of deviation for the latest observed value.

'p \>= \|z\|' represents the empirical probability of the latest
observed absolute 'z-score' or greater.

'p\*' will display a star if 'p \>= \|z\|' is less than or equal to the
value of the argument 'signif'.

'E(\|res\|)\|p' represents the mean or expected absolute value of 'res',
conditional upon the absolute 'z-score' being greater than equal to the
latest observed absolute 'z-score'.

## Further Details

Please refer to the strategies vignette by calling:
[`vignette("strategies", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/strategies.md)

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
statistics <- relative(cloud, quietly = TRUE)
relative(cloud, signif = 0.4)
#> Latest: 2020-12-24 00:00:00 | n: 155 
#>                 mean(X) sd(X)  X[n]   res z-score p >= |z| p* E(|res|)|p
#> chikou_close       1.51  6.07  7.00  5.49    0.91     0.39  *       8.53
#> chikou_high        0.79  6.09  6.60  5.81    0.95     0.36  *       8.91
#> chikou_low         2.31  6.12  7.80  5.49    0.90     0.41          8.40
#> chikou_tenkan      1.73  6.15  6.90  5.17    0.84     0.41          8.70
#> chikou_kijun       2.28  5.96  4.90  2.62    0.44     0.63          6.84
#> chikou_senkouA     3.44  6.46  4.75  1.31    0.20     0.89          6.26
#> chikou_senkouB     4.26  5.40  4.35  0.09    0.02     1.00          4.76
#> chikou_cloudT      2.67  6.44  4.35  1.68    0.26     0.84          6.59
#> chikou_cloudB      5.03  5.20  4.75 -0.28   -0.05     0.99          4.60
#> close_tenkan       0.52  1.77  1.70  1.18    0.67     0.52          2.19
#> close_kijun        1.27  3.25  3.85  2.58    0.79     0.48          4.26
#> close_senkouA      2.00  5.93  5.90  3.90    0.66     0.49          7.71
#> close_senkouB      3.02  5.34  5.75  2.73    0.51     0.73          5.79
#> close_cloudT       1.27  5.73  5.75  4.48    0.78     0.43          7.94
#> close_cloudB       3.75  5.31  5.90  2.15    0.40     0.70          5.90
#> high_tenkan        1.21  1.56  1.90  0.69    0.44     0.66          1.65
#> high_kijun         1.96  3.12  4.05  2.09    0.67     0.53          3.91
#> high_senkouA       2.69  5.84  6.10  3.41    0.58     0.56          7.16
#> high_senkouB       3.71  5.29  5.95  2.24    0.42     0.76          5.61
#> high_cloudT        1.96  5.64  5.95  3.99    0.71     0.48          7.46
#> high_cloudB        4.44  5.27  6.10  1.66    0.32     0.71          5.80
#> low_tenkan        -0.30  1.72  1.30  1.60    0.93     0.40  *       2.37
#> low_kijun          0.44  3.28  3.45  3.01    0.92     0.43          4.50
#> low_senkouA        1.18  5.97  5.50  4.32    0.72     0.45          8.12
#> low_senkouB        2.19  5.38  5.35  3.16    0.59     0.64          6.25
#> low_cloudT         0.45  5.76  5.35  4.90    0.85     0.43          8.02
#> low_cloudB         2.92  5.38  5.50  2.58    0.48     0.68          6.04
#> tenkan_kijun       0.75  2.42  2.15  1.40    0.58     0.60          2.79
#> tenkan_senkouA     1.48  5.44  4.20  2.72    0.50     0.57          6.61
#> tenkan_senkouB     2.49  5.12  4.05  1.56    0.30     0.79          5.26
#> tenkan_cloudT      0.75  5.20  4.05  3.30    0.63     0.50          6.82
#> tenkan_cloudB      3.23  5.12  4.20  0.97    0.19     0.79          5.17
#> kijun_senkouA      0.74  3.64  2.05  1.31    0.36     0.69          4.05
#> kijun_senkouB      1.75  4.16  1.90  0.15    0.04     0.97          3.50
#> kijun_cloudT       0.00  3.62  1.90  1.90    0.52     0.64          4.18
#> kijun_cloudB       2.48  3.86  2.05 -0.43   -0.11     0.98          3.31
#> senkouA_senkouB    1.01  2.98 -0.15 -1.16   -0.39     0.70          3.27
relative(cloud, order = TRUE, signif = 0.4)
#> Latest: 2020-12-24 00:00:00 | n: 155 
#>                 mean(X) sd(X)  X[n]   res z-score p >= |z| p* E(|res|)|p
#> chikou_high        0.79  6.09  6.60  5.81    0.95     0.36  *       8.91
#> low_tenkan        -0.30  1.72  1.30  1.60    0.93     0.40  *       2.37
#> low_kijun          0.44  3.28  3.45  3.01    0.92     0.43          4.50
#> chikou_close       1.51  6.07  7.00  5.49    0.91     0.39  *       8.53
#> chikou_low         2.31  6.12  7.80  5.49    0.90     0.41          8.40
#> low_cloudT         0.45  5.76  5.35  4.90    0.85     0.43          8.02
#> chikou_tenkan      1.73  6.15  6.90  5.17    0.84     0.41          8.70
#> close_kijun        1.27  3.25  3.85  2.58    0.79     0.48          4.26
#> close_cloudT       1.27  5.73  5.75  4.48    0.78     0.43          7.94
#> low_senkouA        1.18  5.97  5.50  4.32    0.72     0.45          8.12
#> high_cloudT        1.96  5.64  5.95  3.99    0.71     0.48          7.46
#> high_kijun         1.96  3.12  4.05  2.09    0.67     0.53          3.91
#> close_tenkan       0.52  1.77  1.70  1.18    0.67     0.52          2.19
#> close_senkouA      2.00  5.93  5.90  3.90    0.66     0.49          7.71
#> tenkan_cloudT      0.75  5.20  4.05  3.30    0.63     0.50          6.82
#> low_senkouB        2.19  5.38  5.35  3.16    0.59     0.64          6.25
#> high_senkouA       2.69  5.84  6.10  3.41    0.58     0.56          7.16
#> tenkan_kijun       0.75  2.42  2.15  1.40    0.58     0.60          2.79
#> kijun_cloudT       0.00  3.62  1.90  1.90    0.52     0.64          4.18
#> close_senkouB      3.02  5.34  5.75  2.73    0.51     0.73          5.79
#> tenkan_senkouA     1.48  5.44  4.20  2.72    0.50     0.57          6.61
#> low_cloudB         2.92  5.38  5.50  2.58    0.48     0.68          6.04
#> high_tenkan        1.21  1.56  1.90  0.69    0.44     0.66          1.65
#> chikou_kijun       2.28  5.96  4.90  2.62    0.44     0.63          6.84
#> high_senkouB       3.71  5.29  5.95  2.24    0.42     0.76          5.61
#> close_cloudB       3.75  5.31  5.90  2.15    0.40     0.70          5.90
#> senkouA_senkouB    1.01  2.98 -0.15 -1.16   -0.39     0.70          3.27
#> kijun_senkouA      0.74  3.64  2.05  1.31    0.36     0.69          4.05
#> high_cloudB        4.44  5.27  6.10  1.66    0.32     0.71          5.80
#> tenkan_senkouB     2.49  5.12  4.05  1.56    0.30     0.79          5.26
#> chikou_cloudT      2.67  6.44  4.35  1.68    0.26     0.84          6.59
#> chikou_senkouA     3.44  6.46  4.75  1.31    0.20     0.89          6.26
#> tenkan_cloudB      3.23  5.12  4.20  0.97    0.19     0.79          5.17
#> kijun_cloudB       2.48  3.86  2.05 -0.43   -0.11     0.98          3.31
#> chikou_cloudB      5.03  5.20  4.75 -0.28   -0.05     0.99          4.60
#> kijun_senkouB      1.75  4.16  1.90  0.15    0.04     0.97          3.50
#> chikou_senkouB     4.26  5.40  4.35  0.09    0.02     1.00          4.76
```
