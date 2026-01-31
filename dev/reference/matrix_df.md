# Convert matrix to data.frame

An optimised 'matrix' to 'data.frame' constructor.

## Usage

``` r
matrix_df(x, keep.attrs = FALSE)
```

## Arguments

- x:

  a matrix.

- keep.attrs:

  \[default FALSE\] if set to TRUE, will preserve any custom attributes
  set on the original object.

## Value

A ‘data.frame’ object. If the matrix has row names, these are retained
by the dataframe.

## Details

The optimised data.frame constructors are used internally within the
package and made available as utilities. Please note that no data
validation or checking is performed.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data)
mcloud <- as.matrix(cloud)
df <- matrix_df(mcloud)
str(df)
#> 'data.frame':    281 obs. of  12 variables:
#>  $ open   : num  123 123 123 123 124 ...
#>  $ high   : num  123 123 123 124 125 ...
#>  $ low    : num  122 123 122 123 124 ...
#>  $ close  : num  123 123 123 124 125 ...
#>  $ cd     : num  -1 1 1 1 1 1 -1 0 -1 -1 ...
#>  $ tenkan : num  NA NA NA NA NA ...
#>  $ kijun  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouA: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ senkouB: num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ chikou : num  123 123 123 124 124 ...
#>  $ cloudT : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ cloudB : num  NA NA NA NA NA NA NA NA NA NA ...
str(rownames(df))
#>  chr [1:281] "2020-01-02" "2020-01-03" "2020-01-06" "2020-01-07" ...
```
