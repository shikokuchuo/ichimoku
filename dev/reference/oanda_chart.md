# OANDA Real-time Cloud Charts

Plot real-time Ichimoku Kinko Hyo cloud charts for major currencies,
metals, commodities, government bonds and stock indices using OANDA
fxTrade API data.

## Usage

``` r
oanda_chart(
  instrument,
  granularity = c("D", "W", "M", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30", "M15",
    "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5"),
  refresh = 5,
  count = 250,
  price = c("M", "B", "A"),
  theme = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
  type = c("none", "r", "s"),
  limit,
  server,
  apikey,
  ...,
  periods = c(9L, 26L, 52L)
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

- refresh:

  \[default 5\] data refresh interval in seconds, with a minimum of 1.

- count:

  \[default 250\] the number of periods to return. The API supports a
  maximum of 5000. Note that fewer periods are actually shown on the
  chart to ensure a full cloud is always displayed.

- price:

  \[default "M"\] pricing component, one of "M" (midpoint), "B" (bid) or
  "A" (ask).

- theme:

  \[default 'classic'\] with further choices of ‘dark’, ‘mono’,
  ‘noguchi’, ‘okabe-ito’ or ‘solarized’. Alternatively, supply a vector
  of 12 colour values (hex codes or names) as a user-defined theme.

- type:

  \[default 'none'\] type of sub-plot to display beneath the ichimoku
  cloud chart, with a choice of 'none', 'r' or 's' for the corresponding
  oscillator type.

- limit:

  (optional) specify a time in seconds by which to limit the session.
  The session will end with data returned automatically after the
  specified time has elapsed.

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

- ...:

  additional arguments passed along to
  [`ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md)
  for calculating the ichimoku cloud or
  [`autoplot`](https://shikokuchuo.net/ichimoku/dev/reference/autoplot.ichimoku.md)
  to set chart parameters.

- periods:

  \[default c(9L, 26L, 52L)\] a vector defining the length of periods
  used for the cloud. This parameter shoud not normally be modified as
  using other values would be invalid in the context of traditional
  ichimoku analysis.

## Value

The ichimoku object underlying the chart (invisibly) on function exit. A
plot of the ichimoku chart for the price data requested is output to the
graphical device at each refresh interval.

## Details

This function polls the OANDA fxTrade API for the latest live prices and
updates the plot in the graphical device at each refresh interval. Use
'ctrl+c' or 'Esc' to interrupt and stop updating.

To access the underlying data, assign the function to an object, for
example: `cloud <- oanda_chart("USD_JPY")`.

## Further Details

Please refer to the OANDA fxTrade API vignette by calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run these examples
oanda_chart("USD_JPY")
oanda_chart("EUR_JPY", granularity = "H1", refresh = 3, count = 300, price = "B", theme = "mono")

# Save data underlying chart at time of function exit
cloud <- oanda_chart("USD_JPY")
} # }
```
