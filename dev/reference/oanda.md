# OANDA Price Data

Retrieve price data for major currencies, metals, commodities,
government bonds and stock indices from the OANDA fxTrade API.

## Usage

``` r
oanda(
  instrument,
  granularity = c("D", "W", "M", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30", "M15",
    "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5"),
  count = NULL,
  from = NULL,
  to = NULL,
  price = c("M", "B", "A"),
  server,
  apikey,
  quietly
)
```

## Arguments

- instrument:

  string containing the base currency and quote currency delimited by
  '\_' or '-' (e.g. "USD_JPY" or "usd-jpy"). Use the
  [`oanda_instruments`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_instruments.md)
  function to return a list of all valid instruments.

- granularity:

  \[default "D"\] the granularity of the price data to fetch, one of
  "M", "W", "D", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30",
  "M15", "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5".

- count:

  (optional) the number of periods to return. The API supports a maximum
  of 5000 for each individual request, and defaults to 500 if not
  specified. If both 'from' and 'to' are specified, 'count' is ignored,
  as the time range combined with 'granularity' will determine the
  number of periods to return.

- from:

  (optional) the start of the time range for which to fetch price data,
  in a format convertible to POSIXct by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), for example
  "2020-02-01".

- to:

  (optional) the end of the time range for which to fetch price data, in
  a format convertible to POSIXct by
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), for example
  "2020-06-30".

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

- quietly:

  (optional) if set to TRUE, will suppress printing of auxiliary output
  to the console and return quietly.

## Value

A data.frame containing the price data requested.

## Details

This function queries the OANDA fxTrade API.

Requires an fxTrade account with OANDA
<https://www.oanda.com/forex-trading/>. If you do not already hold a
live account, you may register for an OANDA fxTrade practice / demo
account. There is a link on your OANDA fxTrade account profile page
'Manage API Access' (My Account -\> My Services -\> Manage API Access).
From there, a personal access token to use with the OANDA API can be
generated, as well as revoked.

The
[`oanda_set_key`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_set_key.md)
function can be used to save the API key in the system credential store
so that it is automatically recognised in future (requires the 'keyring'
package to be installed).

## Further Details

Please refer to the OANDA fxTrade API vignette by calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

'OANDA' and 'fxTrade' are trademarks owned by OANDA Corporation, an
entity unaffiliated with the ichimoku package.

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run these examples
prices <- oanda("USD_JPY")
ichimoku(prices)

oanda("EUR_JPY", granularity = "H1", count = 250, from = "2020-02-01", price = "B")
} # }
```
