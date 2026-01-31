# OANDA Quote Latest Price

Provides a single line price quote for an instrument.

## Usage

``` r
oanda_quote(instrument, price = c("M", "B", "A"), server, apikey)
```

## Arguments

- instrument:

  string containing the base currency and quote currency delimited by
  '\_' or '-' (e.g. "USD_JPY" or "usd-jpy"). Use the
  [`oanda_instruments`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_instruments.md)
  function to return a list of all valid instruments.

- price:

  \[default "M"\] pricing component, one of "M" (midpoint), "B" (bid) or
  "A" (ask).

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

Invisible NULL. The instrument, timestamp, daily open, high, low and
last prices, percentage change from the open, and the pricing component
(M for mid, B for bid, A for ask) is output to the console.

## Details

This function is designed for interactive use.

For further details please refer to the OANDA fxTrade API vignette by
calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run this example
oanda_quote("USD_JPY")
} # }
```
