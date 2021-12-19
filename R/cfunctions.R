# ichimoku - C Functions -------------------------------------------------------

#' Mean Over a Rolling Window
#'
#' Window funtion used to calculate the rolling mean of a numeric vector.
#'
#' @param x a numeric vector of type 'double'.
#' @param window integer size of the rolling window.
#'
#' @return A numeric vector of the same length as 'x'. The first (window - 1)
#'     elements will be NAs.
#'
#' @details This is an implementation designed for performance. If NAs are
#'     present in the input vector 'x', the corresponding element and all
#'     subsequent elements will be NAs in the output vector.
#'
#' @noRd
#'
meanOver <- function(x, window) {
  .Call(`_ichimoku_meanOver`, x, window)
}

#' Look at Informational Attributes
#'
#' Inspect the informational attributes of objects.
#'
#' @param x an object (optional). If 'x' is not supplied, \code{\link{.Last.value}}
#'     will be used instead.
#'
#' @return For objects created by the ichimoku package, a pairlist of attributes
#'     specific to that data type.
#'
#'     For other objects, a pairlist of non-standard attributes for matrix /
#'     data.frame / xts classes, or else invisible NULL if none are present.
#'
#' @details Note: autostrat list attributes may be accessed directly using
#'     \code{look(x)$logret} and \code{look(x)$summary}.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' look(cloud)
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#'
#' strat <- stratlist[[1]]
#' look(strat)
#'
#' grid <- mlgrid(cloud)
#' look(grid)
#'
#' \dontrun{
#' # OANDA API key required to run this example
#' prices <- oanda("USD_JPY")
#' look(prices)
#' }
#'
#' @export
#'
look <- function(x) {
  if (missing(x)) x <- .Last.value
  lk <- .Call(`_ichimoku_look`, x)
  if (length(lk)) lk else invisible()
}

#' Make POSIXct
#'
#' Utility function to convert a UNIX time integer into a POSIXct variable by
#'     appending the appropriate class attribute.
#'
#' @param x an integer vector representing the UNIX/POSIX time since the epoch.
#'
#' @return A vector of class 'POSIXct' and 'POSIXt'.
#'
#' @details This is the equivalent of .POSIXct available in the base namespace,
#'     re-implemented in C code for performance.
#'
#' @noRd
#'
psxct <- function(x) {
  .Call(`_ichimoku_psxct`, x)
}

