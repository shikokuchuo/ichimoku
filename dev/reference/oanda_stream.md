# OANDA Streaming Data

Stream live price and liquidity data for major currencies, metals,
commodities, government bonds and stock indices from the OANDA fxTrade
Streaming API.

## Usage

``` r
oanda_stream(instrument, display = 8L, limit, server, apikey)
```

## Arguments

- instrument:

  string containing the base currency and quote currency delimited by
  '\_' or '-' (e.g. "USD_JPY" or "usd-jpy"). Use the
  [`oanda_instruments`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_instruments.md)
  function to return a list of all valid instruments.

- display:

  \[default 8L\] integer rows of data to display in the console at any
  one time.

- limit:

  (optional) specify a time in seconds by which to limit the streaming
  session. The session will end with data returned automatically after
  the specified time has elapsed.

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

Returned invisibly, a dataframe containing the data for the streaming
session on function exit. The latest rows of the dataframe are printed
to the console, as governed by the 'display' argument.

## Details

This function connects to the OANDA fxTrade Streaming API. Send an
interrupt using the 'Esc' key or 'Ctrl+c' to stop the stream and return
the session data.

Note: only messages of type 'PRICE' are processed. Messages of type
'HEARTBEAT' consisting of only a timestamp are discarded.

## Streaming Data

Summarised from the streaming API documentation:

- Pricing stream does not include every single price created for the
  Account

- At most 4 prices are sent per second (every 250 milliseconds) for each
  instrument

- If more than one price is created during the 250 millisecond window,
  only the price in effect at the end of the window is sent

- This means that during periods of rapid price movement, not every
  price is sent

- Pricing windows for different connections to the stream are not all
  aligned in the same way (e.g. to the top of the second)

- This means that during periods of rapid price movement, different
  prices may be observed depending on the alignment for the connection

## Further Details

Please refer to the OANDA fxTrade API vignette by calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run this example
data <- oanda_stream("USD_JPY", display = 8L)
} # }
```
