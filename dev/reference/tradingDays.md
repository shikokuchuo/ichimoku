# Select Trading Days

Used by
[`ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md)
to subset a vector of dates to trading days.

## Usage

``` r
tradingDays(x, holidays, ...)
```

## Arguments

- x:

  a vector of POSIXct date objects.

- holidays:

  (optional) a vector, or function which outputs a vector, of dates
  defined as holidays. Set to NULL for a continuously-traded market.

- ...:

  other arguments not used by this function.

## Value

A vector of logical values: TRUE if the corresponding element of ‘x’ is
a weekday and not a holiday, FALSE otherwise.

Or, if the parameter ‘holidays’ is set to NULL, a vector of TRUE values
of the same length as ‘x’.

## Details

New Year's Day (01-01) and Christmas Day (12-25) are defined as holidays
by default if ‘holidays’ is not specified.

## Examples

``` r
dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
dates
#> [1] "2020-01-01 UTC" "2020-01-02 UTC" "2020-01-03 UTC" "2020-01-04 UTC"
#> [5] "2020-01-05 UTC" "2020-01-06 UTC" "2020-01-07 UTC"
tradingDays(dates)
#> [1] FALSE  TRUE  TRUE FALSE FALSE  TRUE  TRUE
tradingDays(dates, holidays = c("2020-01-02", "2020-01-03"))
#> [1]  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
tradingDays(dates, holidays = NULL)
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```
