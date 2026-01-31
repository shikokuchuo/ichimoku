# Sample OHLC Price Data

Simulated prices for a hypothetical financial asset. Created for the
purpose of demonstrating package functions in examples and vignettes
only.

## Usage

``` r
sample_ohlc_data
```

## Format

A data frame with 256 observations of 6 variables:

- time - timestamp of observation \[POSIXct\]

- open - opening price \[numeric\]

- low - low price \[numeric\]

- high - high price \[numeric\]

- close - closing price \[numeric\]

- volume - volume \[integer\]

## Source

Not applicable: simulated data

## Examples

``` r
head(sample_ohlc_data)
#>         time  open  high   low close volume
#> 1 2020-01-02 123.0 123.1 122.5 122.7   1875
#> 2 2020-01-03 122.7 122.8 122.6 122.8   1479
#> 3 2020-01-06 122.8 123.4 122.4 123.3   1792
#> 4 2020-01-07 123.3 124.3 123.3 124.1   1977
#> 5 2020-01-08 124.1 124.8 124.0 124.8   2239
#> 6 2020-01-09 124.8 125.4 124.5 125.3   1842
```
