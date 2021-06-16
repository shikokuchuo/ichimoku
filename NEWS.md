# ichimoku 0.1.10

ichimoku now has the following capabilities:
* visualization layer: compute and plot ichimoku cloud charts.
* strategy layer: tools for creating and backtesting ichimoku strategies.
* ML layer: tools for further developing quantitative ichimoku solutions.

New features:
* Create and backtest ichimoku strategies.
* New all-greyscale 'mono' theme.
* tradingDays() function to allow customisation of holidays when calculating the future cloud.

Updates:
* Ichimoku object specification updated - this is breaking, but can be fixed by re-running 'ichimoku()' on previous ichimoku objects.
* Ichimoku objects now inherit 'xts' and 'zoo' classes for better integration with other econometrics and analytics packages.
* Package now depends on Rcpp and requires compilation - added more performant rolling window functions minOver() and maxOver() using customised C++ algorithm.
* 'RcppRoll' and 'timeDate' package dependencies retired.
* 'sample_ohlc_data' updated to better demonstrate new features.

Other:
* Updated with CRAN release status.

# ichimoku 0.1.2

* Added sample synthetic pricing dataset for examples and vignettes.
* Documentation changes for CRAN release.

# ichimoku 0.1.0

* This is the first pre-release of ichimoku.
