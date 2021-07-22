#' Sample OHLC Price Data
#'
#' Simulated prices for a hypothetical financial asset. Created for the purpose
#'     of demonstrating package functions in examples and vignettes only.
#'
#' @format A data frame with 234 observations of 5 variables:
#' \itemize{
#'   \item{time}{ - timestamp of observation}
#'   \item{open}{ - opening price}
#'   \item{low}{ - low price}
#'   \item{high}{ - high price}
#'   \item{close}{ - closing price}
#' }
#'
#' @usage sample_ohlc_data
#' @source Not applicable: simulated data
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
"sample_ohlc_data"
