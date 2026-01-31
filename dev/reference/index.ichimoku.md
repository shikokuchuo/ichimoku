# Extract the Index of Ichimoku Objects

Method for extracting the date-time index of ichimoku objects.

## Usage

``` r
# S3 method for class 'ichimoku'
index(x, subset, ...)
```

## Arguments

- x:

  an object of class ‘ichimoku’.

- subset:

  an integer or logical value or vector by which to subset the index.

- ...:

  arguments passed to or from other methods.

## Value

The date-time index of the ichimoku object as a vector of POSIXct
values.

## Details

This function is an S3 method for the generic function index() for class
‘ichimoku’. It can be invoked by calling index(x) on an object ‘x’ of
class ‘ichimoku’.

Subsetting by specifying the ‘subset’ parameter subsets using the
numerical values underlying the POSIXct times and results in a faster
operation than usual subset operators such as ‘\\’.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data)
index(cloud)[101:110]
#>  [1] "2020-05-20 23:00:00 UTC" "2020-05-21 23:00:00 UTC"
#>  [3] "2020-05-24 23:00:00 UTC" "2020-05-25 23:00:00 UTC"
#>  [5] "2020-05-26 23:00:00 UTC" "2020-05-27 23:00:00 UTC"
#>  [7] "2020-05-28 23:00:00 UTC" "2020-05-31 23:00:00 UTC"
#>  [9] "2020-06-01 23:00:00 UTC" "2020-06-02 23:00:00 UTC"
index(cloud, 101:110)
#>  [1] "2020-05-20 23:00:00 UTC" "2020-05-21 23:00:00 UTC"
#>  [3] "2020-05-24 23:00:00 UTC" "2020-05-25 23:00:00 UTC"
#>  [5] "2020-05-26 23:00:00 UTC" "2020-05-27 23:00:00 UTC"
#>  [7] "2020-05-28 23:00:00 UTC" "2020-05-31 23:00:00 UTC"
#>  [9] "2020-06-01 23:00:00 UTC" "2020-06-02 23:00:00 UTC"
```
