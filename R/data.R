#' Sample OHLC Pricing Data
#'
#' Synthetic prices for a hypothetical financial asset. Created for the purpose
#'     of demonstrating package functions in examples and vignettes only.
#'
#' @format A data frame with 234 observations of 5 variables:
#' \itemize{
#'   \item{Date}{ - date of observation}
#'   \item{Open}{ - daily opening price}
#'   \item{Low}{ - daily low price}
#'   \item{High}{ - daily high price}
#'   \item{Close}{ - daily closing price}
#' }
#'
#' @usage sample_ohlc_data
#' @source Not applicable: synthetic data
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
"sample_ohlc_data"
