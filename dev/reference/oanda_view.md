# OANDA View Market Performance

Provides a snapshot overview of markets on an intraday basis, showing
the relative performance of individual constituents.

## Usage

``` r
oanda_view(
  market = c("allfx", "bonds", "commodities", "fx", "metals", "stocks"),
  price = c("M", "B", "A"),
  server,
  apikey
)
```

## Arguments

- market:

  string specifying the market: 'allfx' for all available currencies,
  'bonds' for government bonds, 'commodities' for commodities, 'fx' for
  major currency pairs, 'metals' for metals and 'stocks' for global
  stock markets.

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

A data.frame containing the daily open, high, low and last prices, along
with the percentage price change from the open, ordered by the
percentage change. The instrument names are set as row names.

The first timestamp retrieved and the pricing component are printed to
the console as well as saved as attributes to the dataframe. The
dataframe is also printed to the console.

## Details

This function is designed for interactive use.

For further details please refer to the OANDA fxTrade API vignette by
calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run this example
oanda_view("fx")
} # }
```
