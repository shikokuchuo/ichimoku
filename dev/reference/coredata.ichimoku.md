# Extract the Core Data of Ichimoku Objects

Method for extracting the core data matrix of ichimoku objects.

## Usage

``` r
# S3 method for class 'ichimoku'
coredata(x, fmt, ...)
```

## Arguments

- x:

  an object of class ‘ichimoku’.

- fmt:

  (optional) set to TRUE to retain the index as row names of the
  returned matrix, or a character string passed on to the ‘format’
  argument of [`format.POSIXct()`](https://rdrr.io/r/base/strptime.html)
  to format these values in a specific way.

- ...:

  arguments passed to or from other methods.

## Value

A numeric matrix containing the ichimoku object data, stripped of the
index unless ‘fmt’ is specified in which case the index will be retained
as character values in the matrix row names.

## Details

This function is an S3 method for the generic function coredata() for
class ‘ichimoku’. It can be invoked by calling coredata(x) on an object
‘x’ of class ‘ichimoku’.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data)
coredata(cloud)[101:120, ]
#>        open  high   low close cd tenkan  kijun senkouA senkouB chikou  cloudT
#>  [1,] 122.4 122.6 121.1 121.9 -1 121.05 123.90 125.675  124.85  135.6 125.675
#>  [2,] 121.9 123.7 121.7 123.3  1 121.40 123.80 125.675  124.85  134.5 125.675
#>  [3,] 123.3 124.0 123.0 124.0  1 121.55 123.80 125.675  124.85  134.3 125.675
#>  [4,] 124.2 124.3 124.0 124.1 -1 121.75 123.65 125.975  124.85  135.9 125.975
#>  [5,] 124.1 124.1 123.4 123.9 -1 122.25 123.65 126.475  124.85  135.2 126.475
#>  [6,] 123.9 124.7 123.7 124.5  1 122.90 123.65 126.575  124.85  135.7 126.575
#>  [7,] 124.5 124.7 123.9 124.2 -1 122.90 123.60 126.250  124.85  135.4 126.250
#>  [8,] 124.2 125.2 124.0 124.7  1 123.15 123.60 126.150  124.85  135.7 126.150
#>  [9,] 124.7 129.8 124.7 129.6  1 125.45 124.45 125.675  124.85  135.6 125.675
#> [10,] 129.9 130.6 129.9 130.2  1 126.15 124.85 125.625  124.85  136.2 125.625
#> [11,] 130.2 132.5 129.7 131.6  1 127.75 125.80 125.625  124.85  136.5 125.625
#> [12,] 131.6 132.0 131.1 131.7  1 127.95 125.80 125.550  124.85  138.1 125.550
#> [13,] 131.7 133.4 131.6 133.0  1 128.40 126.25 125.425  124.85  137.8 125.425
#> [14,] 133.0 134.3 131.9 132.5 -1 129.00 126.70 125.550  124.85  139.3 125.550
#> [15,] 132.4 132.8 131.2 131.9 -1 129.10 126.70 125.650  124.85  139.3 125.650
#> [16,] 131.9 132.0 131.6 131.9  0 129.15 126.70 125.875  124.85  138.8 125.875
#> [17,] 131.9 132.3 131.1 132.0  1 129.50 126.70 125.400  124.85  137.2 125.400
#> [18,] 132.0 134.7 131.8 134.3  1 132.20 126.90 124.825  124.85  135.2 124.850
#> [19,] 134.3 134.4 132.0 132.3 -1 132.20 126.90 123.800  124.85  137.0 124.850
#> [20,] 132.3 132.8 131.5 131.8 -1 132.90 126.90 123.375  124.55  136.6 124.550
#>        cloudB
#>  [1,] 124.850
#>  [2,] 124.850
#>  [3,] 124.850
#>  [4,] 124.850
#>  [5,] 124.850
#>  [6,] 124.850
#>  [7,] 124.850
#>  [8,] 124.850
#>  [9,] 124.850
#> [10,] 124.850
#> [11,] 124.850
#> [12,] 124.850
#> [13,] 124.850
#> [14,] 124.850
#> [15,] 124.850
#> [16,] 124.850
#> [17,] 124.850
#> [18,] 124.825
#> [19,] 123.800
#> [20,] 123.375
```
