
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ichimoku <img src='man/figures/logo.jpg' align="right" height="79" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/shikokuchuo/ichimoku/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/ichimoku/actions)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

An implementation of the ‘Ichimoku Kinko Hyo’ charting system, also
commonly known as ‘cloud charts’, providing both publication-ready and
fully-interactive charts for analysis. Originating from and popularised
in Japan, the technique is a refinement on candlestick charting, in
widespread use on trading floors worldwide. Translating to ‘one-glance
equilibrium chart’, it allows the price action and market structure of
financial securities to be determined ‘at-a-glance’.

## Installation

Install the development version of ichimoku from Github with:

``` r
# install.packages("devtools")
devtools::install_github("shikokuchuo/ichimoku")
```

## Example

Simply `ichimoku()` and `plot()`.

``` r
library(ichimoku)
# TKR is a data frame of OHLC pricing data
cloud <- ichimoku(TKR)
# Plot ichimoku object
plot(cloud, from = "2020-05-01", to = "2020-12-03")
```

<img src="man/figures/README-plot-1.png" width="672" width="480" />

## Reference

R package site: <https://shikokuchuo.net/ichimoku/>
