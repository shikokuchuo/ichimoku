# OANDA Order Book

Provides a summary of the aggregate orders posted by OANDA fxTrade
clients at each price level.

## Usage

``` r
oanda_orders(instrument, time, server, apikey)
```

## Arguments

- instrument:

  string containing the base currency and quote currency delimited by
  '\_' or '-' (e.g. "USD_JPY" or "usd-jpy"). Use the
  [`oanda_instruments`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_instruments.md)
  function to return a list of all valid instruments.

- time:

  (optional) the time for which to retrieve the order book, in a format
  convertible to POSIXct by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html). If not
  specified, the most recent order book will be retrieved.

- server:

  (optional) specify the "practice" or "live" server according to the
  account type held. If not specified, will default to "practice",
  unless this has been changed by
  [`oanda_switch`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_switch.md).

- apikey:

  (optional) string containing the OANDA fxTrade API key (personal
  access token), or function that returns this string. Does not need to
  be specified if already stored as the environment variable
  `OANDA_API_KEY` or by
  [`oanda_set_key`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_set_key.md).
  Can also be entered interactively if not specified.

## Value

Invisibly, a data frame of the order book with parameters saved as
attributes. A chart showing the percentage long and short orders at each
price level is output to the graphical device.

## Details

This feature has been implemented by OANDA only for certain major
currency pairs and should be considered experimental.

Note: as certain orders are placed far from the market price, only the
interquartile range of order levels is shown on the chart. The returned
data frame does however contain the entire order book.

For further details please refer to the OANDA fxTrade API vignette by
calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run this example
oanda_orders("USD_JPY")
} # }
```
