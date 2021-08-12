# Ichimoku - Look Utility ------------------------------------------------------

#' Look at Ichimoku Objects
#'
#' Inspect the informational attributes of objects created by the 'ichimoku'
#'     package. Can also be used to extract ichimoku objects from lists returned
#'     by \code{\link{autostrat}}.
#'
#' @param x an ichimoku object or an object returned by \code{\link{autostrat}},
#'     \code{\link{mlgrid}} or \code{\link{oanda}}.
#' @param which (optional) integer value of strategy to return from an autostrat
#'     list.
#'
#' @return List of attribute values, or if 'which' is specified on an autostrat
#'     list, an ichimoku object containing a strategy.
#'
#' @details Note: for a level 2 autostrat object, if the object fails to print
#'     correctly due to its length, please access the list items directly using
#'     \code{look(x)$summary} and \code{look(x)$logret}, possibly in conjunction
#'     with head() or by setting the 'max' argument in print().
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' look(cloud)
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#'
#' strat <- look(stratlist, which = 1)
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
look <- function(x, which) {

  name <- deparse(substitute(x))

  if (is.ichimoku(x)) {
    if (hasStrat(x)) list(periods = attr(x, "periods"), periodicity = attr(x, "periodicity"),
                          ticker = attr(x, "ticker"), strat = attr(x, "strat"))
    else list(periods = attr(x, "periods"), periodicity = attr(x, "periodicity"),
              ticker = attr(x, "ticker"))

  } else if (isTRUE(attr(x, "autostrat"))) {
    if (missing(which)) list(logret = attr(x, "logret"), summary = attr(x, "summary"))
    else tryCatch(x[[which]], error = function(e) {
      stop("Value '", which, "' for 'which' is invalid\n'which' must be an integer ",
           "specifying one of the strategies contained in '", name, "'", call. = FALSE)
      })

  } else if (isTRUE(attr(x, "mlgrid"))) {
    list(y = attr(x, "y"), direction = attr(x, "direction"), ticker = attr(x, "ticker"))

  } else if (isTRUE(attr(x, "oanda"))) {
    list(instrument = attr(x, "instrument"), price = attr(x, "price"),
         timestamp = attr(x, "timestamp"))

  } else stop("look() only works with certain object types created with the ichimoku package",
              call. = FALSE)

}

