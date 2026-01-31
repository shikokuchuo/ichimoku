# Available OANDA Instruments

Return list of instruments including major currencies, metals,
commodities, government bonds and stock indices for which pricing data
is available from the OANDA fxTrade API.

## Usage

``` r
oanda_instruments(server, apikey)
```

## Arguments

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

A data.frame containing the instrument name, full display name, and
type.

## Details

This function returns a data.frame listing the instrument names
available for an account associated with the supplied OANDA fxTrade API
key.

For further details please refer to the OANDA fxTrade API vignette by
calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run this example
oanda_instruments()
} # }
```
