
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ichimoku <a href="https://shikokuchuo.net/ichimoku/" alt="ichimoku"><img src="man/figures/logo.png" alt="ichimoku logo" align="right" /></a>

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/ichimoku?color=00008b)](https://CRAN.R-project.org/package=ichimoku)
[![ichimoku status
badge](https://shikokuchuo.r-universe.dev/badges/ichimoku)](https://shikokuchuo.r-universe.dev/ichimoku)
[![R-CMD-check](https://github.com/shikokuchuo/ichimoku/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shikokuchuo/ichimoku/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/shikokuchuo/ichimoku/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/ichimoku)
[![DOI](https://zenodo.org/badge/367928545.svg)](https://zenodo.org/badge/latestdoi/367928545)
<!-- badges: end -->

Visualization and Tools for Ichimoku Kinko Hyo Strategies

An implementation of ‘Ichimoku Kinko Hyo’, also commonly known as ‘cloud
charts’. Static and interactive visualizations with tools for creating,
backtesting and development of quantitative ‘ichimoku’ strategies. As
described in Sasaki (1996, ISBN:4925152009), the technique is a
refinement on candlestick charting, originating from Japan and now in
widespread use in technical analysis worldwide. Translating as
‘one-glance equilibrium chart’, it allows the price action and market
structure of financial securities to be determined ‘at-a-glance’.
Incorporates an interface with the OANDA fxTrade API
<https://developer.oanda.com/> for retrieving historical and live
streaming price data for major currencies, metals, commodities,
government bonds and stock indices.

## Installation

Install ichimoku from CRAN:

``` r
install.packages("ichimoku")
```

## Quick Start

``` r
library(ichimoku)
```

Simply `ichimoku()` and `plot()`:

``` r
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
plot(cloud, window = "2020-05/")
```

<img src="man/figures/README-ichimoku-1.png" alt="ichimoku kinko hyo" />
<!-- alt="ichimoku kinko hyo" -->

`autostrat()` to automatically evaluate and rank top-performing
strategies:

``` r
autostrat(cloud, n = 3)
#>                        [,1]               [,2]              [,3]             
#> Strategy               "senkouB > tenkan" "cloudB > tenkan" "senkouB > kijun"
#> ---------------------  "----------"       "----------"      "----------"     
#> Strategy cuml return % 17.49              16.08             14.1             
#> Per period mean ret %  0.0906             0.0838            0.0741           
#> Periods in market      63                 51                64               
#> Total trades           3                  3                 3                
#> Average trade length   21                 17                21.33            
#> Trade success %        100                100               100              
#> Worst trade ret %      3.64               3.16              3.49             
#> ---------------------  "----------"       "----------"      "----------"     
#> Benchmark cuml ret %   5.53               5.53              5.53             
#> Per period mean ret %  0.0302             0.0302            0.0302           
#> Periods in market      178                178               178              
#> ---------------------  "----------"       "----------"      "----------"     
#> Direction              "long"             "long"            "long"           
#> Start                  2020-04-20         2020-04-20        2020-04-20       
#> End                    2020-12-23         2020-12-23        2020-12-23       
#> Ticker                 "TKR"              "TKR"             "TKR"
```

## Principal ichimoku functions

#### Data & Visualization

- [`ichimoku()`](https://shikokuchuo.net/ichimoku/reference/ichimoku.html) -
  to create an ichimoku object from price data.

- [`plot()`](https://shikokuchuo.net/ichimoku/reference/plot.ichimoku.html)
  / [`iplot()`](https://shikokuchuo.net/ichimoku/reference/iplot.html) -
  to plot (interactive) cloud charts from ichimoku objects.

- [`archive()`](https://shikokuchuo.net/ichimoku/reference/archive.html) -
  for reading/writing objects to/from archive files with data
  verification.

- [`oanda()`](https://shikokuchuo.net/ichimoku/reference/oanda.html) -
  to retrieve price data from the OANDA fxTrade API.

#### Strategies & ML

- [`strat()`](https://shikokuchuo.net/ichimoku/reference/strat.html) -
  to augment an ichimoku object with a strategy, including combined and
  asymmetric strategies.

- [`autostrat()`](https://shikokuchuo.net/ichimoku/reference/autostrat.html) -
  to automatically evaluate and rank top-performing strategies.

- [`mlgrid()`](https://shikokuchuo.net/ichimoku/reference/mlgrid.html) -
  to generate a numeric representation of the ichimoku cloud chart.

- [`relative()`](https://shikokuchuo.net/ichimoku/reference/relative.html) -
  to produce a statistical summary of the latest ichimoku numeric
  representation relative to historical values.

#### Real-time

- [`oanda_chart()`](https://shikokuchuo.net/ichimoku/reference/oanda_chart.html) -
  to plot real-time ichimoku cloud charts using OANDA data.

- [`oanda_studio()`](https://shikokuchuo.net/ichimoku/reference/oanda_studio.html) -
  a complete live analysis environment using OANDA data implemented in R
  Shiny.

- [`oanda_stream()`](https://shikokuchuo.net/ichimoku/reference/oanda_stream.html)
  /
  [`oanda_quote()`](https://shikokuchuo.net/ichimoku/reference/oanda_quote.html) -
  to obtain the latest live data stream / quote from the OANDA fxTrade
  API.

- [`oanda_view()`](https://shikokuchuo.net/ichimoku/reference/oanda_view.html) -
  for a market overview showing the relative performance of
  constituents.

- [`oanda_orders()`](https://shikokuchuo.net/ichimoku/reference/oanda_orders.html)
  /
  [`oanda_positions()`](https://shikokuchuo.net/ichimoku/reference/oanda_positions.html) -
  to retrieve the aggregate OANDA fxTrade order / position book.

## Vignettes

Long-form documentation links:

{ 1 } [Cloud Charts - The Reference
Manual](https://shikokuchuo.net/ichimoku/articles/reference.html)

{ 2 } [Beyond Visualization - Quantitative
Strategies](https://shikokuchuo.net/ichimoku/articles/strategies.html)

{ 3 } [Auxiliary
Functions](https://shikokuchuo.net/ichimoku/articles/utilities.html)

{ 4 } [The OANDA fxTrade
API](https://shikokuchuo.net/ichimoku/articles/xoanda.html)

## References

Sasaki, H. 佐々木 英信 (1996), *一目均衡表の研究 [ichimoku kinkouhyou
no kenkyuu]*. Tokyo, Japan: Toushi Radar.

‘OANDA’ and ‘fxTrade’ are trademarks owned by OANDA Corporation, an
entity unaffiliated with the ichimoku package.

–

◈ ichimoku R package: <https://shikokuchuo.net/ichimoku/>.

Listed CRAN Finance Task View:
<https://cran.r-project.org/view=Finance>.

–

Please note that this project is released with a [Contributor Code of
Conduct](https://shikokuchuo.net/ichimoku/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by its terms.
