
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ichimoku <img src='man/figures/logo.jpg' align="right" height="79" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/shikokuchuo/ichimoku/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shikokuchuo/ichimoku/actions/workflows/R-CMD-check.yaml)
![CRAN/METACRAN](https://img.shields.io/cran/v/ichimoku)
![CRAN/METACRAN](https://img.shields.io/cran/l/ichimoku)
<!-- badges: end -->

An implementation of the ‘Ichimoku Kinko Hyo’ charting system, also
commonly known as ‘cloud charts’, providing both publication-ready and
fully-interactive charts for analysis. As described in Sasaki (1996,
<ISBN:4925152009>), the technique is a refinement on candlestick
charting originating from Japan, now in widespread use in technical
analysis worldwide. Translating to ‘one-glance equilibrium chart’, it
allows the price action and market structure of financial securities to
be determined ‘at-a-glance’.

## Installation

Install the released version of ichimoku from CRAN:

``` r
install.packages("ichimoku")
```

Or install the development version of ichimoku from GitHub with:

``` r
devtools::install_github("shikokuchuo/ichimoku")
```

## Example

Simply `ichimoku()` and `plot()`.

``` r
library(ichimoku)
TKR <- sample_ohlc_data
# Create ichimoku object:
cloud <- ichimoku(TKR)
# Plot ichimoku object:
plot(cloud, from = "2020-05-01", to = "2020-12-03")
```

<img src="man/figures/README-plot-1.png" width="672" width="480" />

## Reference

R package site: <https://shikokuchuo.net/ichimoku/>

CRAN page: <https://CRAN.R-project.org/package=ichimoku>
