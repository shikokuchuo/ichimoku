# is.ichimoku

A function for checking if an object is an ichimoku object.

## Usage

``` r
is.ichimoku(x)
```

## Arguments

- x:

  an object.

## Value

A logical value of TRUE if ‘x’ is of class ‘ichimoku’, FALSE otherwise.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data)

# TRUE:
is.ichimoku(cloud)
#> [1] TRUE
# FALSE:
is.ichimoku(sample_ohlc_data)
#> [1] FALSE
```
