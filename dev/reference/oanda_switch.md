# Switch Default OANDA Server

Switch the default OANDA fxTrade server from ‘practice’ to ‘live’ or
vice versa. Settings persist for the current session only.

## Usage

``` r
oanda_switch()
```

## Value

Invisible NULL. A message informs the resulting default server setting.

## Details

The default server at the start of a new session is the practice server.
Call this function to switch to the live server.

This function can be used to toggle between the practice and live
servers. Any cached variables for API key, account or instruments list
are cleared each time this function is called.

For further details please refer to the OANDA fxTrade API vignette by
calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
oanda_switch()
#> Default OANDA server switched to 'live'
oanda_switch()
#> Default OANDA server switched to 'practice'
```
