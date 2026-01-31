# Interactive Ichimoku Cloud Plot

Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects in R Shiny,
allowing full customisation of chart elements in an interactive
environment. Intuitive cursor infotip provides ready access to the data
directly from the chart.

## Usage

``` r
iplot(
  x,
  ticker,
  subtitle,
  theme = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
  strat = TRUE,
  type = c("none", "r", "s", "bar", "line"),
  custom,
  ...,
  launch.browser = TRUE
)
```

## Arguments

- x:

  an object of class ‘ichimoku’.

- ticker:

  (optional) specify a ticker (or other text) to include in the chart
  heading. If not set, the ticker saved within the ichimoku object will
  be used.

- subtitle:

  (optional) specify a subtitle to display under the chart title.

- theme:

  \[default 'classic'\] with further choices of ‘dark’, ‘mono’,
  ‘noguchi’, ‘okabe-ito’ or ‘solarized’.

- strat:

  \[default TRUE\] if the ichimoku object contains a strategy, the
  periods for which the strategy results in a position will be shaded,
  and the strategy printed as the chart subtitle (if not otherwise
  specified). Set to FALSE to turn off this behaviour.

- type:

  \[default 'none'\] type of sub-plot to display beneath the ichimoku
  cloud chart, with a choice of ‘none’, ‘r’ or ‘s’ for the corresponding
  oscillator type, and ‘bar’ or ‘line’ for custom plots.

- custom:

  (optional) character string (containing a regular expression) matching
  the column name of the variable to be displayed as sub-plot. Specify
  `type = 'bar'` or `type = 'line'`, otherwise other type settings will
  take precedence.

- ...:

  additional parameters passed along to the ‘options’ argument of
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- launch.browser:

  \[default TRUE\] If TRUE, the system's default web browser will be
  launched automatically after the app is started. The value of this
  argument can also be a function to call with the application's URL. To
  use the default Shiny viewer in RStudio, please specify
  `getOption("shiny.launch.browser")`.

## Value

A Shiny app object with class ‘shiny.appobj’. With default arguments,
the Shiny app is launched in the default browser.

## Details

For further details please refer to the reference vignette by calling:
[`vignette("reference", package = "ichimoku")`](https://shikokuchuo.net/ichimoku/dev/articles/reference.md)

## Examples

``` r
if (interactive()) {
# Only run examples in interactive R sessions
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
iplot(cloud)

# To open in RStudio viewer instead of default browser
iplot(cloud, launch.browser = getOption("shiny.launch.browser"))
}
```
