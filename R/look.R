# Ichimoku - Look Utility ------------------------------------------------------

#' Look at Informational Attributes
#'
#' Inspect the informational attributes of objects, or extract ichimoku objects
#'     from lists returned by \code{\link{autostrat}}.
#'
#' @param x an object (created by the ichimoku package).
#' @param which (optional) integer value of strategy to return from an autostrat
#'     list.
#'
#' @return For objects created by the ichimoku package, a list of attribute values
#'     specific to that data type, or if 'which' is specified on an autostrat
#'     list, an ichimoku object containing a strategy.
#'
#'     For other objects, a list of attributes that are non-standard for 'matrix',
#'     'data.frame', or 'xts' objects, or else invisible NULL if none are present.
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

  if (isTRUE(attr(x, "autostrat"))) {
    if (missing(which)) {
      lk <- attributes(x)
      lk$autostrat <- NULL
      lk
    } else tryCatch(x[[which]], error = function(e) {
      stop("'", which, "' is not a valid value for 'which'\n'which' should be an integer ",
           "value specifying one of the strategies 1 to ", length(x), call. = FALSE)
    })

  } else {
    lk <- attributes(x)
    lk$dim <- lk$dimnames <- lk$names <- lk$row.names <- lk$index <- lk$class <- lk$mlgrid <- lk$oanda <- NULL
    if (length(lk)) lk else invisible()
  }

}

