# ichimoku 1.1.5

#### New features:

* `ichimoku()` adds ability to create *pseudo* cloud charts from single series price data where OHLC data is not available.
* New 'keep.data' argument for `ichimoku()` to preserve additional data present in the input object.
* New 'type' argument for `plot.ichimoku()` and `iplot()` enables a choice of oscillator (R-type and S-type) to be shown as a sub-plot.
* New 'custom' argument for `plot.ichimoku()` and `iplot()` to enable custom data variable to be shown beneath the ichimoku cloud chart as either a line or bar chart.
* `oanda_studio()` adds a button for saving the underlying data of the live ichimoku cloud chart using `archive()`.

#### Updates:

* Fixes certain cases where calculation of the future cloud could fail for data frequency lower than daily.
* Updates to `sample_ohlc_data` to add volume column, adhere to working days etc.
* Argument 'message' renamed to 'subtitle' across all plot functions for clarity.
* New sub-plot functionality adds 'gridExtra' package dependency.
* Minor performance improvements for `ichimoku()`, plot functions and various utilities.
* Vignettes updates

# ichimoku 1.1.0

#### New features:

* `archive()` moves to using the native RData format, enabling any R object to be stored perfectly with sha256 verification.

#### Updates:

* 'ArrowTabular' method for `ichimoku()` removed and 'arrow' optional dependency retired.
* Fixes data types issue affecting dataframes returned by `oanda()` in v1.0.0.
* Fixes critical issues affecting the Solaris platform in v1.0.0.

# ichimoku 1.0.0

#### *ichimoku object specification release:*

* ichimoku objects created in versions prior to 0.3.51 will no longer work correctly with newer versions of the package. Upgrade to the latest package version and run `ichimoku()` on previously-created objects to re-create them according to the new specification (data is preserved).

#### New features:

* New `archive()` function allows for archiving of ichimoku objects to files stored in the Apache Arrow IPC file format. Also uses sha256 hashing to ensure data integrity of archives - adds optional dependency on `openssl` package.
* `oanda()` gains the capability to download over 5000 data periods in multiple (rate-limited) requests when both the 'from' and 'to' arguments are specified.
* `look()` can now inspect any R object.
* `ichimoku()` gains a new S3 method for the `ArrowTabular` class for working with Arrow Tables.
* `tradingDays()` gets a 'noholidays' argument for use in markets that trade 24/7 with no non-trading days.
* `iplot()` and `oanda_studio()` now use 'bslib' (a Shiny dependency) to enable theming of the entire UI rather than just the chart. Infotip candle direction symbols updated for greater clarity.
* `xts_df()` and `matrix_df()` dataframe constructors gain a 'keep.attrs' argument. If set to TRUE, the returned dataframe will retain the custom attributes of the original object.
* Re-export functions for working with ichimoku objects: `index()`, `coredata()` from 'zoo', and `xts()` from the 'xts' package.

#### Updates:

* Implements caching of certain OANDA variables so that they are retrieved once and then used throughout a session.
* Charts of daily or lower frequency now have prettier and more usefully-aligned breaks.
* Improved handling of timezones: OANDA data and charts will now show correctly in the user timezone.
* `oanda()` arguments 'from' and 'to' can now take any date-time format convertible to POSIXct.
* `oanda_studio()` subsets the plot window so as to always show a full cloud, consistent with the behaviour of `oanda_chart()`.
* `oanda_chart()` and `oanda_studio()` add explicit support for the 'periods' argument passed to `ichimoku()`.
* `oanda_chart()` now passes on additional parameters to `autoplot()`.
* `ichimoku()` now enforces data types on the price data for higher certainty of success, and has more robust handling of matrices and 'data.frame' compatible formats such as 'tibble'.
* Fixes issue which caused `oanda()` not to return weekly data in certain cases.
* Corrects trade success statistics for short strategies returned by `strat()`.
* The following functions are no longer exported to keep the package tidy: `grid_dup()`, `maxOver()`, `minOver()`, `oanda_accounts()`.
* `sample_ohlc_data` slightly lengthened to better demonstrate strat features.
* Adds 'stats' R core package dependency.
* Package now links to 'cpp11' headers, retiring 'Rcpp' package dependency.
* Package dependency 'httr' switched to 'curl'.
* Miscellaneous performance optimisations.
* Documentation refresh.

# ichimoku 0.3.0

#### New features:

* OANDA fxTrade API interface for retrieving price data: new `oanda()`, `oanda_stream()` and `oanda_chart()` functions.
* `oanda_studio()` is a complete live analysis envrionment using OANDA data in an R Shiny app.
* `iplot()` is re-launched using R Shiny. Introduces an intuitive cursor infotip that allows data to be read directly from the chart.
* `look()` function for viewing informational attributes of objects created by the package, and for extracting ichimoku objects from lists returned by `autostrat()`.
* `strat()` and `autostrat()` gain a new type/level 3 for asymmetric strategies using different indicators for position entry and exit. 

#### Updates:

* New dependencies on 'httr' and 'jsonlite' packages, required for the OANDA fxTrade API interface.
* `strat()` now takes optional 'c3' and 'c4' arguments to provide parameters for complex strategies. This allows a combined strategy to be specified using a single `strat()` call.
* `iplot()` now uses a Shiny backend. Plotly charts have been retired.
* Argument 'gaps' for the plot functions is deprecated but remains available through `gplot()` for the time being.
* Chikou span is now the top layer in plots so visible over the candlesticks.
* Performance enhancements for principal functions, including optimised data validation and error handling code.
* Certain helper functions renamed for consistency.
* Documentation refresh.

# ichimoku 0.2.0

#### New features:

* ichimoku now has the following capabilities:
  - Visualization layer: compute and plot ichimoku cloud charts.
  - Strategy layer: tools for creating and backtesting ichimoku strategies.
  - ML layer: tools for further developing quantitative ichimoku solutions.
* New all-greyscale 'mono' theme.
* `tradingDays()` helper function to allow customisation of holidays when calculating the future cloud.

#### Updates:

* Ichimoku object specification updated.
* Ichimoku objects now inherit 'xts' and 'zoo' classes for better integration with other econometrics and analytics packages.
* Package now requires compilation and adds 'Rcpp' dependency due to rolling window functions `minOver()` and `maxOver()` using custom C++ algorithm.
* 'RcppRoll' and 'timeDate' package dependencies retired.
* `sample_ohlc_data` updated to better demonstrate new features.
* Updated with CRAN release status.

# ichimoku 0.1.2

* Added sample synthetic pricing dataset for examples and vignettes.
* Documentation changes for CRAN release.

# ichimoku 0.1.0

* This is the first pre-release of ichimoku.
