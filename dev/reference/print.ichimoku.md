# Print Ichimoku Objects

Default print method for ichimoku objects to enable automatic plotting
of the ichimoku cloud chart.

## Usage

``` r
# S3 method for class 'ichimoku'
print(x, plot = TRUE, rows = 26L, ...)
```

## Arguments

- x:

  an object of class ‘ichimoku’.

- plot:

  \[default TRUE\] set to FALSE to prevent automatic plotting of the
  ichimoku cloud chart.

- rows:

  \[default 26L\] integer number of rows to print.

- ...:

  additional arguments passed along to the xts print and
  [`plot.ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/plot.ichimoku.md)
  methods.

## Value

The ichimoku object supplied (invisibly). The data is printed to the
console. The ichimoku cloud chart is also output to the graphical device
depending on the parameters set.

## Details

This function is an S3 method for the generic function print() for class
‘ichimoku’. It can be invoked by calling print(x) on an object ‘x’ of
class ‘ichimoku’.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")

print(cloud)
#>  ichimoku   [ more() to display more rows | look() to inspect attributes ]
#>   object
#>             open  high   low close cd tenkan  kijun senkouA senkouB chikou
#> 2020-01-02 123.0 123.1 122.5 122.7 -1     NA     NA      NA      NA  122.8
#> 2020-01-03 122.7 122.8 122.6 122.8  1     NA     NA      NA      NA  122.9
#> 2020-01-06 122.8 123.4 122.4 123.3  1     NA     NA      NA      NA  123.0
#> 2020-01-07 123.3 124.3 123.3 124.1  1     NA     NA      NA      NA  123.9
#> 2020-01-08 124.1 124.8 124.0 124.8  1     NA     NA      NA      NA  123.6
#> 2020-01-09 124.8 125.4 124.5 125.3  1     NA     NA      NA      NA  122.5
#> 2020-01-10 125.3 125.3 124.8 125.2 -1     NA     NA      NA      NA  122.6
#> 2020-01-13 125.2 125.3 125.1 125.2  0     NA     NA      NA      NA  123.0
#> 2020-01-14 125.2 125.2 124.3 124.4 -1 123.90     NA      NA      NA  123.1
#> 2020-01-15 124.4 124.5 123.7 123.9 -1 123.90     NA      NA      NA  122.1
#> 2020-01-16 123.9 124.4 123.8 124.2  1 123.90     NA      NA      NA  121.9
#> 2020-01-17 124.2 124.3 123.0 123.5 -1 124.20     NA      NA      NA  121.1
#> 2020-01-20 123.5 123.8 123.1 123.2 -1 124.20     NA      NA      NA  121.4
#> 2020-01-21 123.6 123.6 123.4 123.6  0 124.20     NA      NA      NA  121.5
#> 2020-01-22 123.5 124.0 123.3 123.5  0 124.15     NA      NA      NA  121.9
#> 2020-01-23 123.5 124.5 123.3 124.3  1 124.15     NA      NA      NA  120.9
#> 2020-01-24 124.3 124.4 124.0 124.2 -1 124.10     NA      NA      NA  121.0
#> 2020-01-27 124.2 124.2 122.7 123.0 -1 123.60     NA      NA      NA  120.8
#> 2020-01-28 123.0 123.6 122.7 123.1  1 123.60     NA      NA      NA  121.2
#> 2020-01-29 123.1 123.3 123.1 123.2  1 123.60     NA      NA      NA  121.6
#> 2020-01-30 123.2 123.2 122.8 123.1 -1 123.60     NA      NA      NA  121.1
#> 2020-01-31 123.1 123.5 123.0 123.2  1 123.60     NA      NA      NA  122.1
#> 2020-02-03 123.2 123.3 122.6 123.0 -1 123.55     NA      NA      NA  122.2
#> 2020-02-04 123.0 123.1 122.6 122.9 -1 123.55     NA      NA      NA  122.2
#> 2020-02-05 122.9 123.2 122.6 122.8 -1 123.50     NA      NA      NA  122.4
#> 2020-02-06 122.8 122.9 122.8 122.8  0 123.40 123.90      NA      NA  122.4
#>             cloudT  cloudB
#> 2020-01-02      NA      NA
#> 2020-01-03      NA      NA
#> 2020-01-06      NA      NA
#> 2020-01-07      NA      NA
#> 2020-01-08      NA      NA
#> 2020-01-09      NA      NA
#> 2020-01-10      NA      NA
#> 2020-01-13      NA      NA
#> 2020-01-14      NA      NA
#> 2020-01-15      NA      NA
#> 2020-01-16      NA      NA
#> 2020-01-17      NA      NA
#> 2020-01-20      NA      NA
#> 2020-01-21      NA      NA
#> 2020-01-22      NA      NA
#> 2020-01-23      NA      NA
#> 2020-01-24      NA      NA
#> 2020-01-27      NA      NA
#> 2020-01-28      NA      NA
#> 2020-01-29      NA      NA
#> 2020-01-30      NA      NA
#> 2020-01-31      NA      NA
#> 2020-02-03      NA      NA
#> 2020-02-04      NA      NA
#> 2020-02-05      NA      NA
#> 2020-02-06      NA      NA
#>  [ reached 'max' / getOption("max.print") -- omitted 27 rows ]

print(cloud, plot = FALSE, rows = 20L)
#>  ichimoku   [ more() to display more rows | look() to inspect attributes ]
#>   object
#>             open  high   low close cd tenkan kijun senkouA senkouB chikou
#> 2020-01-02 123.0 123.1 122.5 122.7 -1     NA    NA      NA      NA  122.8
#> 2020-01-03 122.7 122.8 122.6 122.8  1     NA    NA      NA      NA  122.9
#> 2020-01-06 122.8 123.4 122.4 123.3  1     NA    NA      NA      NA  123.0
#> 2020-01-07 123.3 124.3 123.3 124.1  1     NA    NA      NA      NA  123.9
#> 2020-01-08 124.1 124.8 124.0 124.8  1     NA    NA      NA      NA  123.6
#> 2020-01-09 124.8 125.4 124.5 125.3  1     NA    NA      NA      NA  122.5
#> 2020-01-10 125.3 125.3 124.8 125.2 -1     NA    NA      NA      NA  122.6
#> 2020-01-13 125.2 125.3 125.1 125.2  0     NA    NA      NA      NA  123.0
#> 2020-01-14 125.2 125.2 124.3 124.4 -1 123.90    NA      NA      NA  123.1
#> 2020-01-15 124.4 124.5 123.7 123.9 -1 123.90    NA      NA      NA  122.1
#> 2020-01-16 123.9 124.4 123.8 124.2  1 123.90    NA      NA      NA  121.9
#> 2020-01-17 124.2 124.3 123.0 123.5 -1 124.20    NA      NA      NA  121.1
#> 2020-01-20 123.5 123.8 123.1 123.2 -1 124.20    NA      NA      NA  121.4
#> 2020-01-21 123.6 123.6 123.4 123.6  0 124.20    NA      NA      NA  121.5
#> 2020-01-22 123.5 124.0 123.3 123.5  0 124.15    NA      NA      NA  121.9
#> 2020-01-23 123.5 124.5 123.3 124.3  1 124.15    NA      NA      NA  120.9
#> 2020-01-24 124.3 124.4 124.0 124.2 -1 124.10    NA      NA      NA  121.0
#> 2020-01-27 124.2 124.2 122.7 123.0 -1 123.60    NA      NA      NA  120.8
#> 2020-01-28 123.0 123.6 122.7 123.1  1 123.60    NA      NA      NA  121.2
#> 2020-01-29 123.1 123.3 123.1 123.2  1 123.60    NA      NA      NA  121.6
#>             cloudT cloudB
#> 2020-01-02      NA     NA
#> 2020-01-03      NA     NA
#> 2020-01-06      NA     NA
#> 2020-01-07      NA     NA
#> 2020-01-08      NA     NA
#> 2020-01-09      NA     NA
#> 2020-01-10      NA     NA
#> 2020-01-13      NA     NA
#> 2020-01-14      NA     NA
#> 2020-01-15      NA     NA
#> 2020-01-16      NA     NA
#> 2020-01-17      NA     NA
#> 2020-01-20      NA     NA
#> 2020-01-21      NA     NA
#> 2020-01-22      NA     NA
#> 2020-01-23      NA     NA
#> 2020-01-24      NA     NA
#> 2020-01-27      NA     NA
#> 2020-01-28      NA     NA
#> 2020-01-29      NA     NA
#>  [ reached 'max' / getOption("max.print") -- omitted 21 rows ]
```
