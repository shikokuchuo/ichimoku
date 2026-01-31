# OANDA Studio Interactive Live Analysis

Interactive and fully-customisable R Shiny environment providing
real-time Ichimoku Kinko Hyo cloud charts for major currencies, metals,
commodities, government bonds and stock indices using OANDA fxTrade API
data. Intuitive cursor infotip provides ready access to the data
directly from the chart.

## Usage

``` r
oanda_studio(
  instrument = "USD_JPY",
  granularity = c("D", "W", "M", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30", "M15",
    "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5"),
  refresh = 5,
  count = 300,
  price = c("M", "B", "A"),
  theme = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
  type = c("none", "r", "s"),
  server,
  apikey,
  new.process = FALSE,
  multi.session = FALSE,
  ...,
  launch.browser = TRUE,
  periods = c(9L, 26L, 52L)
)
```

## Arguments

- instrument:

  \[default 'USD_JPY'\] string containing the base currency and quote
  currency delimited by a '\_'. Use the
  [`oanda_instruments`](https://shikokuchuo.net/ichimoku/dev/reference/oanda_instruments.md)
  function to return a list of all valid instruments.

- granularity:

  \[default "D"\] the granularity of the price data to fetch, one of
  "M", "W", "D", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30",
  "M15", "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5".

- refresh:

  \[default 5\] data refresh interval in seconds, with a minimum of 1.

- count:

  \[default 300\] the number of periods to return, from 100 to 800. Note
  that fewer periods are actually shown on the chart to ensure a full
  cloud is always displayed.

- price:

  \[default "M"\] pricing component, one of "M" (midpoint), "B" (bid) or
  "A" (ask).

- theme:

  \[default 'original'\] with alternative choices of 'conceptual',
  'dark', 'fresh', 'mono', or 'solarized'.

- type:

  \[default 'none'\] type of sub-plot to display beneath the ichimoku
  cloud chart, with a choice of 'none', 'r' or 's' for the corresponding
  oscillator type.

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

- new.process:

  \[default FALSE\] if TRUE, will start the shiny session in a new R
  process, unblocking the current process and allowing continued use of
  the R console.

- multi.session:

  \[default FALSE\] if TRUE, does not automatically close the Shiny app
  when an individual session (web browser page) disconnects. Use with
  caution in conjunction with ‘new.process’ as the Shiny app continues
  to run in the background process.

- ...:

  additional arguments passed along to
  [`ichimoku`](https://shikokuchuo.net/ichimoku/dev/reference/ichimoku.md)
  for calculating the ichimoku cloud,
  [`autoplot`](https://shikokuchuo.net/ichimoku/dev/reference/autoplot.ichimoku.md)
  to set chart parameters, or the 'options' argument of
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- launch.browser:

  \[default TRUE\] If TRUE, the system's default web browser will be
  launched automatically after the app is started. The value of this
  argument can also be a function to call with the application's URL. To
  use the default Shiny viewer in RStudio, please specify
  `getOption("shiny.launch.browser")`.

- periods:

  \[default c(9L, 26L, 52L)\] a vector defining the length of periods
  used for the cloud. This parameter shoud not normally be modified as
  using other values would be invalid in the context of traditional
  ichimoku analysis.

## Value

Invisible NULL, or a 'mirai' if 'new.process' is specified as TRUE. With
default arguments, a Shiny app is launched in the default browser.

## Details

This function polls the OANDA fxTrade API for the latest prices and
updates a customisable reactive Shiny app at each refresh interval.

## Further Details

Please refer to the OANDA fxTrade API vignette by calling:
[`vignette("xoanda", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/xoanda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# OANDA fxTrade API key required to run these examples
oanda_studio()

# To open in RStudio viewer instead of default browser
oanda_studio(launch.browser = getOption("shiny.launch.browser"))
} # }
```
