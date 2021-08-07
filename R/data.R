#' Sample OHLC Price Data
#'
#' Simulated prices for a hypothetical financial asset. Created for the purpose
#'     of demonstrating package functions in examples and vignettes only.
#'
#' @format A data frame with 256 observations of 5 variables:
#' \itemize{
#'   \item{time}{ - timestamp of observation [POSIXct]}
#'   \item{open}{ - opening price [numeric]}
#'   \item{low}{ - low price [numeric]}
#'   \item{high}{ - high price [numeric]}
#'   \item{close}{ - closing price [numeric]}
#' }
#'
#' @usage sample_ohlc_data
#' @source Not applicable: simulated data
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
"sample_ohlc_data"
