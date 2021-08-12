# ichimoku 0.3.51.4

#### New features:

* `archive()` replaces `ichimoku_write()` and `ichimoku_read()` by merging their functionality.
* Re-export functions for working with ichimoku objects: `index()`, `coredata()` from 'zoo', and `xts()` from the 'xts' package.

#### Updates:

* Charts of daily or lower frequency now have prettier and more usefully-aligned breaks using custom algorithm.
* Improved handling of timezones. OANDA data and charts will now show correctly in the user timezone.
* Fixed cases of the timezone of restored objects from Arrow archives not matching the original.
* Fixed bug which caused `oanda()` not to return weekly data in certain cases.
* Fixed bug in trade success statistics for short strategies returned by `strat()`.
* Linking to 'cpp11' package, removed vendored code.
* Added 'stats' R core package dependency for improved performance in strat functions.

# ichimoku 0.3.51

#### *ichimoku object specification release (version 1)*

* Please upgrade to the latest version of ichimoku.
* ichimoku objects created in versions prior to 0.3.51 will no longer work correctly with newer versions of the package.
* `ichimoku()` can be called on previously-created objects to re-create the objects according to the v1 specification.

# ichimoku 0.3.5

#### New features:

* New `ichimoku_write()` and `ichimoku_read()` functions allow for archiving of ichimoku objects to files stored in the Apache Arrow IPC file format.
* `ichimoku()` gains a new S3 method for the `ArrowTabular` class for working with Arrow Tables.
* `oanda()` gains the capability to download over 5000 data periods in multiple (rate-limited) requests when both the 'from' and 'to' arguments are specified.

#### Updates:

* Implemented caching of certain OANDA variables so they are retrieved once and then used throughout a session.
* `iplot()` and `oanda_studio()` now use 'bslib' (a Shiny dependency) to enable theming of the entire UI rather than just the chart. Infotip candle direction symbols updated for greater clarity.
* `oanda()` arguments 'from' and 'to' can now take any date-time format convertible to POSIXct.
* `oanda_studio()` subsets the plot window so as to always show a full cloud, consistent with the behaviour of `oanda_chart()`.
* `oanda_chart()` and `oanda_studio()` add explicit support for the 'periods' argument passed to `ichimoku()`.
* `oanda_chart()` now passes on additional parameters to `autoplot()`.
* `ichimoku()` now enforces data types on the price data for higher certainty of success, and has more robust handling of matrices and 'data.frame' compatible formats such as 'tibble'.
* Package dependency 'httr' switched to 'curl' providing performance gains in the OANDA fxTrade API interface.
* ichimoku now employs a vendored version of 'cpp11' 0.3.1 headers which allows for enhanced stability and faster package compilation. Removed dependency on the 'Rcpp' package.
* The following functions are no longer exported to keep the package tidy: `maxOver()`, `minOver()`, `oanda_accounts()`.
* `sample_ohlc_data` slightly lengthened to better demonstrate strat features.
* Miscellaneous performance optimisations.

# ichimoku 0.3.0

#### New features:

* OANDA fxTrade API interface for retrieving price data: new `oanda()`, `oanda_stream()` and `oanda_chart()` functions.
* `oanda_studio()` is a complete live analysis envrionment using OANDA data in an R Shiny app.
* `iplot()` is re-launched using R Shiny. Introduces an intuitive cursor infotip that allows data to be read directly from the chart.
* `look()` function for viewing informational attributes of objects created by the package, and for extracting ichimoku objects from lists returned by `autostrat()`.
* `strat()` and `autostrat()` gain a new type/level 3 for asymmetric strategies using different indicators for position entry and exit. 

#### Updates:

* New dependencies on the 'httr' and 'jsonlite' packages, required for the OANDA fxTrade API interface.
* `strat()` now takes optional 'c3' and 'c4' arguments to provide parameters for complex strategies. This allows a combined strategy to be specified directly using one `strat()` call rather than 2 separate `strat()` calls and a call to `stratcombine()`.
* Plotly charts have been retired as they are simply not scalable to large datasets. `iplot()` now uses a Shiny backend providing similar/enhanced features. To contiune working with plotly, please use `plotly::ggplotly()` on plot objects created by `plot.ichimoku()`.
* Argument 'gaps' for the plot functions is deprecated but remains available through the `gplot()` function until at least version 0.4.
* Chikou span now the top layer in plots so visible over the candlesticks.
* Performance enhancements for principal functions, including optimised data validation and error handling code.
* Certain helper functions renamed for consistency.
* Documentation and README refresh.

# ichimoku 0.2.0

#### New features:

* ichimoku now has the following capabilities:
  - Visualization layer: compute and plot ichimoku cloud charts.
  - Strategy layer: tools for creating and backtesting ichimoku strategies.
  - ML layer: tools for further developing quantitative ichimoku solutions.
* Create and backtest ichimoku strategies.
* New all-greyscale 'mono' theme.
* `tradingDays()` helper function to allow customisation of holidays when calculating the future cloud.

#### Updates:

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
