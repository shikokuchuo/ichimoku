
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ichimoku <img src='man/figures/logo.jpg' align="right" height="79" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/shikokuchuo/ichimoku/workflows/R-CMD-check/badge.svg)](https://github.com/shikokuchuo/ichimoku/actions)
<!-- badges: end -->

An implementation of the Ichimoku Kinko Hyo charting technique, also
commonly known as ‘cloud charts’ in R. Translating to ‘one-glance
equilibrium chart’, it allows the price action and market structure of
financial securities to be determined ‘at-a-glance’. This R package
contains functions to compute and plot both static and interactive
ichimoku cloud charts.

## Installation

You can install the released version of ichimoku from
[Github](https://github.com) with:

``` r
devtools::install_github("shikokuchuo/ichimoku")
```

## Example

Example based on synthetic data:

``` r
library(ichimoku)
cloud <- ichimoku(data, ticker = "TKR", periods = c(2, 4, 8))
plot(cloud)
```

<img src="man/figures/README-plot-1.png" width="672" height="480" />
