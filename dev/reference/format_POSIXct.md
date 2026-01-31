# Format POSIXct

Converts a POSIXct double timestamp into a character string much faster
than [`format.POSIXct`](https://rdrr.io/r/base/strptime.html), with the
same output as the method default.

## Usage

``` r
format_POSIXct(x)
```

## Arguments

- x:

  an object of class ‘POSIXct’.

## Value

A character string

## Examples

``` r
time <- Sys.time()
format(time)
#> [1] "2026-01-31 11:51:30"
format_POSIXct(time)
#> [1] "2026-01-31 11:51:30"
```
