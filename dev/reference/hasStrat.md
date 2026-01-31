# hasStrat

A function for checking if an object contains a strategy.

## Usage

``` r
hasStrat(x)
```

## Arguments

- x:

  an object.

## Value

A logical value of TRUE if the 'strat' attribute of 'x' is set,
otherwise FALSE.

## Details

Designed to be used by ichimoku functions that are either S3 methods for
class 'ichimoku' or after validation that 'x' is an ichimoku object,
hence there is no check on the class of 'x' within this function.

## Examples

``` r
cloud <- ichimoku(sample_ohlc_data)
strat <- strat(cloud)

# TRUE:
hasStrat(strat)
#> [1] TRUE
# FALSE:
hasStrat(cloud)
#> [1] FALSE
```
