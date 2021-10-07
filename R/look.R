# Ichimoku - Look Utility ------------------------------------------------------

#' Look at Informational Attributes
#'
#' Inspect the informational attributes of objects, or extract ichimoku objects
#'     from lists returned by \code{\link{autostrat}}.
#'
#' @param x an object.
#' @param which (optional) integer value of strategy to return from an autostrat
#'     list.
#'
#' @return For objects created by the ichimoku package, a list of attributes
#'     specific to that data type, or if 'which' is specified on an autostrat
#'     list, an ichimoku object containing a strategy.
#'
#'     For other objects, a list of non-standard attributes for matrix /
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

  if (is.null(attr(x, "autostrat"))) {
    lk <- attributes(x)
    lk <- lk[!names(lk) %in% c("dim", "dimnames", "names", "row.names", "index", "class", "mlgrid", "oanda")]
    if (length(lk)) lk else invisible()
  } else {
    if (missing(which)) {
      lk <- attributes(x)
      lk$autostrat <- NULL
      lk
    } else if (is.numeric(which) && which %in% seq_len(length(x))) {
      x[[which]]
    } else {
      stop("'", which, "' is not a valid value for 'which'\n",
           "'which' should be an integer value specifying one of the strategies 1 to ",
           length(x), call. = FALSE)
    }
  }

}

