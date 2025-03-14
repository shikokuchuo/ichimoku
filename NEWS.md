# ichimoku (development version)

# ichimoku 1.5.6

#### New features:

* `format_POSIXct()` is a much faster equivalent of the base R `format.POSIXct()`, used internally and exported as a utility.

# ichimoku 1.5.5

#### New features:

* `oanda_studio()` adds option 'multi.session' to facilitate multiple sessions to be used with the Shiny app (for example more than one browser page).

#### Updates:

* Optimizes package load efficiency with `mirai` and `RcppSimdJson` only loaded the first time they are used.

# ichimoku 1.5.4

#### Updates:

* Fixes cases of asymmetric (type 3) strategies not generating the final entry signal if the corresponding exit condition has not yet been met (thanks @bbac63 #9).
* Internal performance enhancements.

# ichimoku 1.5.3

#### Updates:

* Internal performance enhancements.
* Requires secretbase >= 1.0.0.

# ichimoku 1.5.2

#### Updates:

* `archive()` reverts to using SHA256.
* Requires nanonext >= 1.0.0 and mirai >= 1.0.0.

# ichimoku 1.5.1

#### Updates:

* Fixes `oanda_studio()` so that the live chart no longer greys out when updating (with recent Shiny versions).

# ichimoku 1.5.0

#### New features:

* `archive()` updated to use the fast and memory-efficient implementation of SHA3-256 from {secretbase} for data verification.
  + Note: archive files created using earlier package versions can no longer be verified using `archive()` but may nevertheless be loaded using `readRDS()`.
 
#### Updates:

* Requires secretbase.

# ichimoku 1.4.13

#### Updates:

* `oanda_quote()` now correctly writes a new line after each quote.
* Internal performance enhancements.
* Requires nanonext >= 0.12.0 and mirai >= 0.12.0.

# ichimoku 1.4.12

#### Updates:

* Uses 'mirai' to run `oanda_studio(new.process = TRUE)`
* Fixes `oanda_switch()` after a regression in 1.4.11.
* Updates internal OANDA instruments list.
* Internal performance enhancements.

# ichimoku 1.4.11

#### New features:
 
* Allows using the environment variable 'OANDA_API_KEY' as an alternative to the 'keyring' package.

#### Updates:

* Arguments 'keep.data' and 'keep.attrs' across the package now have an explicit default value of FALSE (no resultant change in behaviour).
* Internal performance enhancements.
* Requires nanonext >= 0.11.0.

# ichimoku 1.4.10

#### Updates:

* Improves handling of OANDA API errors.

# ichimoku 1.4.9

#### Updates:

* Internal performance enhancements.
* Requires nanonext >= 0.10.0 and R >= 3.5.

# ichimoku 1.4.8

#### Updates:

* Ensures compatibility with upcoming {nanonext} releases.
* Internal performance enhancements.

# ichimoku 1.4.7

#### Updates:

* Internal performance enhancements.

# ichimoku 1.4.6

#### Updates:

* Switches to the C interface provided by 'RcppSimdJson' for JSON parsing.
* Internal performance enhancements.

# ichimoku 1.4.5

#### Updates:

* `mlgrid()` argument 'func' becomes 'expr' and takes a named list of quoted language objects or expressions rather than functions for more versatility in custom calculations (for advanced use).
* Internal performance enhancements.

# ichimoku 1.4.4

#### Updates:

* `oanda_instruments()` retries once if encountering a non-success server status code (eliminating a potential error when calling an OANDA function for the first time in a session).
* Updates internal OANDA instruments list.
* Internal performance enhancements.

# ichimoku 1.4.3

#### New features:
 
* `mlgrid()` gains the argument 'func' for supplying a named list of functions for custom calculations (for advanced use). Also, when choosing type z-score, now returns the 'means' and 'sdevs' used to standardise the grid as attributes.

#### Updates:

* Improved performance and reliability of `oanda_stream()`. Returned dataframe no longer contains the column 'type', which was always 'PRICE'.
* Fixes to `ichimoku()` validation code for POSIXct handling changes in upcoming R 4.3.0.
* No longer attempts to plot a single row (subset) ichimoku object.
* API changes in 'ggplot2' require version >= 3.4.0; R version requirement consequently raised to 3.3.
* Improved stability of OANDA functions requiring nanonext >= 0.7.0.
* Switch from 'jsonlite' to 'RcppSimdJson' for JSON parsing.
* Internal performance enhancements.

# ichimoku 1.4.2

#### Updates:

* Implements fix for upcoming R 4.3.0 (thanks @kalibera).
* Internal changes requiring nanonext >= 0.6.0.

# ichimoku 1.4.1

#### Updates:

* Internal performance enhancements.

# ichimoku 1.4.0

#### New features:

* New print method for ichimoku objects.

#### Updates:

* In conjunction with the new print method, `more()` now defaults to printing all rows by default unless 'rows' is specified.
* SHA-256 authentication in `archive()` is faster after an upgrade to use the 'MbedTLS' library via 'nanonext'.
* `archive()` now returns the filename when saving to archive rather than NULL.
* `as_tibble` method for ichimoku objects removed.
* OANDA functions now use 'nanonext' to retrieve data instead of 'curl'.
* 'curl', 'tibble' and 'openssl' dependencies retired.

# ichimoku 1.3.4

#### Updates:

* Moves 'shiny' to 'imports' from 'suggests'.
* Updates internal OANDA instruments list.

# ichimoku 1.3.3

#### Updates:

* Fixed regression in 1.3.2 which caused `iplot()` to fail.

# ichimoku 1.3.2

#### Updates:

* Theme re-organisation with the following changes:
  + default 'original' theme renamed 'classic'
  + 'fresh' replaced with 'okabe-ito' theme (using colours from this colourblind-friendly palette)
  + 'conceptual' theme renamed 'noguchi' (palette inspired by the works of Isamu Noguchi)

# ichimoku 1.3.1

#### New features:

* The `theme` argument of `plot()` and `oanda_chart()` now accepts a user-defined vector of 12 colour values (as hex values or R colour names).

#### Updates:

* Minor UI tweaks and internal performance optimisations.

# ichimoku 1.3.0

#### New features:

* Sub-plots now feature on the same chart and values can be read via the infotip in `iplot()` or `oanda_studio()`.
* R/S-type indicators now an option for `oanda_chart()` and `oanda_studio()`.
* `.ichimoku()` introduced as a faster technical utility version of `ichimoku()` for use when data is already in the prescribed format. 

#### Updates:

* Dependency on C++ compiler, 'cpp11'  and 'gtable' packages retired.
* Internal rolling min/max/mean functions re-implemented directly in C; various ichimoku functions now call C code internally.
* Added `LinkingTo: xts` for use of C function exported from 'xts'.
* `df_trim()` and `extraplot()` removed as no longer required.
* The 'limit' argument for `oanda_chart()` and `oanda_stream()` changed to accept a time in seconds rather than minutes.
* `new.process = TRUE` for `oanda_studio()` now more portable and works under Windows.
* Fixes `oanda_stream()` for when multiple bid/ask liquidity levels are returned in the stream. Returned dataframe now in a revised tidier format with correct data types for ease of further processing.
* Fixes `archive()` to work correctly on R-oldrel (4.0).
* Further performance enhancements to `ichimoku()`, `mlgrid()` and plot functions.
* Documentation refresh.

# ichimoku 1.2.5

#### New features:

* Enhancement to the printing of ichimoku objects by utilising the 'tibble' print method (adds dependency on 'tibble' package), paired with `more()` for quick printing of further rows.
* Optimised `as_tibble()` method implemented for ichimoku objects.
* `mlgrid()` now appends attributes 'k' and 'type' to returned objects, and gains the following arguments:
  - `k` to specify the k-period time horizon over which to calculate target variable 'y'.
  - `type = 'z-score'` to produce the standard score of a 'numeric' type grid.
  - `format` to choose between returning a dataframe or matrix.
* New `oanda_orders()` function provides the OANDA fxTrade order book for certain major currency pairs.
* Major revamp of `oanda_stream()` which has a much improved interface and now renders the stream as a structured dataframe.
* `oanda_chart()` and `oanda_stream()` gain the argument 'limit' which imposes a time limit for the session after which data is returned automatically.
* `oanda_positions()` gains the parameter 'time' for retrieving the position book at a particular time.
* `archive()` gains the ability to save files interactively using a system dialog - call the function with an object, leaving the second argument empty e.g. `archive(object, )`.
* `index()` gains the parameter 'subset' for fast subsetting of ichimoku cloud indexes.
* New plot themes 'conceptual' and 'fresh'.

#### Updates:

* Simplification of `look()` by removing the 'which' argument. To extract ichimoku objects in autostrat lists, subset the list directly. The object argument is now optional, accessing .Last.value otherwise.
* Minor enhancements to the `str()` method.
* `df_append()` argument order for 'old' and 'new' swapped to allow for chaining with the pipe operator.
* `archive()` now safe for use in non-interactive settings, where it no longer prompts to confirm overwriting of existing files.
* `oanda_view()` updated for new instruments and now returns correct data types in the dataframe.
* Fixes cases of `relative()` showing the incorrect date for 'latest', along with other cosmetic changes.
* General performance improvements to date handling and dataframe conversions.
* Documentation refresh.

# ichimoku 1.2.4

#### New features:

* New `relative()` function produces a statistical summary of the latest ichimoku cloud chart numeric representation relative to historical values, for determining whether trading falls within or outside of normal ranges.
* `oanda_studio()` gains the argument 'new.process', which when set to TRUE, starts the shiny session in a new R process, unblocking the current process and allowing continued use of the console.
* `mlgrid()` gains the argument `y = 'none'` for a grid with the latest cloud representation and without 'y'.
* `autostrat()`, `relative()` and `oanda()` gain the argument 'quietly' which suppresses additional console output if set to TRUE.
* Improved time index handling for `ichimoku()`: where conversion by `as.POSIXct()` fails, will convert numeric values as POSIX times (with an appropriate warning).
* Optimised 'ichimoku' methods for `coredata()` and `index()` generic functions.
* 'ichimoku' method for `as.data.frame()` implemented as a marginally faster version of `xts_df()` for ichimoku objects.
* More informative custom `str()` and `summary()` methods implemented for ichimoku objects.

#### Updates:

* For OANDA functions, where the 'server' parameter is specified, the corresponding API key will now be retrieved rather than the default, allowing for example `oanda_studio(server = "live", new.process = TRUE)`
* Improved appearance of progress indicators for `oanda()` and `oanda_view()`.
* `oanda()` now safe to use non-interactively - it will no longer prompt in such cases.
* Improvements to the visual appearance of oscillator plots.
* Update to the interactive interface for `oanda_set_key()`.
* Fixes sign of %chg for `oanda_quote()`.
* `xts()` is no longer re-exported from the 'xts' package as `ichimoku()` can now fully re-construct an ichimoku object from its components (see 'Working with ichimoku objects' in the Reference vignette).
* Further performance improvements to `ichimoku()` and other functions.
* Documentation refresh.

# ichimoku 1.2.2

#### New features:

* New `oanda_quote()` function outputs the latest quote for an instrument along with intraday trading statistics to the console.
* New `oanda_view()` function provides the latest overview of an entire market - showing the relative performance of constituents.
* New `oanda_positions()` function provides the OANDA fxTrade position book (% longs and shorts at each price level) for certain major currency pairs.
* `archive()` now allows files to be chosen interactively using a system dialog - call the function with no arguments.
* `df_append()` utility is now faster and gains the arguments 'key' and 'keep.attr'.

#### Updates:

* Accessibility improvements: default 'original' theme adjusted to accommodate colour vision deficiency.
* All OANDA functions now prompt for missing required arguments instead of returning errors.
* For ease of use, the 'instrument' argument in all OANDA functions is now case-insensitive and the delimiter may be supplied as either '_' or '-', so both `oanda("usd-jpy")` and `oanda("USD_JPY")` are acceptable.
* `tradingDays()` argument 'noholidays' removed in favour of 'holidays = NULL'. Logic changed slightly so that default holidays are applied only if 'holidays' is not specified.
* Minor performance improvements to OANDA and ML layer functions.
* Documentation refresh

# ichimoku 1.2.1

#### Updates:

* Fix issue with 'bslib' theming after release of Shiny 1.7.0. For improved reliability, 'bslib' optional dependency removed.
* More neutral formatting of `oanda_stream()` output to accommodate different console colour schemes.
* Fix cases of mis-alignment of main and sub-plots, axis label formatting of custom plots.
* Fix issue which caused `iplot()` to re-calculate the indicators when adjusting the data window.
* Package dependency switched from 'gridExtra' to 'gtable'.
* Minor performance improvements for `ichimoku()` and other functions.

# ichimoku 1.2.0

#### New features:

* `ichimoku()` adds ability to create *pseudo* cloud charts from single series price data where OHLC data is not available.
* `ichimoku()` gains a 'keep.data' argument for retaining additional data present in the input object.
* Plot functions gain a 'type' argument to enable a choice of oscillator (R-type and S-type) to be shown as a sub-plot.
* Plot functions gain a 'custom' argument to enable a custom data variable to be plot as a line or bar chart in the sub-plot area.
* `oanda_studio()` adds a button for saving the underlying data of the live ichimoku cloud chart using `archive()`.
* `oanda_chart()` now returns the underlying ichimoku object (invisibly) on function exit, providing easy access to the chart data.
* `oanda_set_key()` adds support for storing both practice and live account API keys.
* New `oanda_switch()` function allows the default server to be switched from 'practice' to 'live' for the session.

#### Updates:

* Adds fallback for `oanda_instruments()` using an internal instruments table when the API call fails.
* `plot.ichimoku()` now returns the original object invisibly, use autoplot() and extraplot() to return plot objects.
* For all plot functions, the argument 'message' is renamed to 'subtitle'.
* Plots now show Tenkan-sen over Kijun-sen.
* Slight adjustments to original theme: cloud edges now plum-tinted for Senkou A, cyan-tinted for Senkou B.
* Fixes certain cases where calculation of the future cloud could fail for data frequency lower than daily.
* Updates to `sample_ohlc_data` to add volume column, adhere to working days etc.
* OANDA internal functions re-implemented as encapsulated closure list.
* `oanda_get_key()` is removed as functionality incorporated elsewhere.
* New sub-plot functionality adds 'gridExtra' package dependency.
* 'rlang' package dependency is retired as no longer required.
* Internal C++ code: now includes only required cpp11 headers, adds rolling mean function, miscellaneous improvements.
* Minor performance improvements for `ichimoku()`, plot functions, OANDA functions and various utilities.
* Documentation refresh.

# ichimoku 1.1.0

#### New features:

* `archive()` moves to using the native RData format, enabling any R object to be stored perfectly with sha256 verification.

#### Updates:

* 'ArrowTabular' method for `ichimoku()` removed and 'arrow' optional dependency retired.
* Fixes data types issue affecting dataframes returned by `oanda()` in v1.0.0.
* Fixes critical issues affecting the Solaris platform in v1.0.0.

# ichimoku 1.0.0

#### *ichimoku object specification v1 release:*

* ichimoku objects created in v0.x will no longer work correctly with newer versions of the package. Upgrade to the latest package version and run `ichimoku()` on previously-created objects to re-create them according to the new specification (data is preserved).

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
