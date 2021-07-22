# ichimoku 0.3.0

* **New features:**

* OANDA fxTrade API interface for retrieving price data: new `oanda()`, `oanda_stream()` and `oanda_chart()` functions.
* `oanda_studio()` is a complete live analysis envrionment using OANDA data in an R Shiny app.
* `iplot()` is re-launched using R Shiny. Introduces an intuitive cursor tooltip that allows data to be easily read from the chart.
* `look()` function for viewing informational attributes of objects created by the package, and for extracting ichimoku objects from lists returned by `autostrat()`.
* `strat()` and `autostrat()` gain a new type/level 3 for asymmetric strategies using different indicators for position entry and exit (experimental). 

* **Updates:**

* New dependencies on the 'httr' and 'jsonlite' packages, required for the OANDA fxTrade API interface.
* `strat()` now takes optional 'c3' and 'c4' arguments to provide parameters for complex strategies. This allows a combined strategy to be specified directly using one `strat()` call rather than 2 separate `strat()` calls and a call to `stratcombine()`.
* Plotly charts have been retired as they are simply not scalable to large datasets. `iplot()` now uses a Shiny backend providing similar/enhanced features. To contiune working with plotly, please use `plotly::ggplotly()` on plot objects created by `plot.ichimoku()`.
* Argument 'gaps' for the plot functions is deprecated but remains available through the `gplot()` function until at least version 0.4.
* Chikou span now the top layer in plots so visible over the candlesticks.
* Performance enhancements for principal functions, including optimised data validation and error handling code.
* Certain helper functions renamed for consistency.
* Documentation and README refresh.

# ichimoku 0.2.0

* **New features:**

* ichimoku now has the following capabilities:
  - Visualization layer: compute and plot ichimoku cloud charts.
  - Strategy layer: tools for creating and backtesting ichimoku strategies.
  - ML layer: tools for further developing quantitative ichimoku solutions.
* Create and backtest ichimoku strategies.
* New all-greyscale 'mono' theme.
* `tradingDays()` helper function to allow customisation of holidays when calculating the future cloud.

* **Updates:**

* Ichimoku object specification updated - this is breaking but can be fixed by re-running `ichimoku()` on previous objects.
* Ichimoku objects now inherit 'xts' and 'zoo' classes for better integration with other econometrics and analytics packages.
* Package now depends on 'Rcpp' and requires compilation - added more performant rolling window functions `minOver()` and `maxOver()` using customised C++ algorithm.
* 'RcppRoll' and 'timeDate' package dependencies retired.
* `sample_ohlc_data` updated to better demonstrate new features.
* Updated with CRAN release status.

# ichimoku 0.1.2

* Added sample synthetic pricing dataset for examples and vignettes.
* Documentation changes for CRAN release.

# ichimoku 0.1.0

* This is the first pre-release of ichimoku.
