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
#'     For other objects, a list of attributes that are non-standard for matrix /
#'     data.frame / xts objects, or else invisible NULL if none are present.
#'
#' @details Note: for a level 2 autostrat object, if the attributes fail to print
#'     correctly due to their length, please access the them directly using
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

  if (is.null(attr(x, "autostrat"))) {
    lk <- attributes(x)
    lk$dim <- lk$dimnames <- lk$names <- lk$row.names <- lk$index <- lk$class <- lk$mlgrid <- lk$oanda <- NULL
    if (length(lk)) lk else invisible()
  } else {
    if (missing(which)) {
      lk <- attributes(x)
      lk$autostrat <- NULL
      lk
    } else if (!is.numeric(which)) {
      stop("'", which, "' is not a valid value for 'which'\n",
           "'which' should be an integer value specifying one of the strategies 1 to ",
           length(x), call. = FALSE)
    } else {
      tryCatch(x[[which]], error = function(e) {
        stop(which, " is not a valid value for 'which'\n",
             "'which' should be an integer value specifying one of the strategies 1 to ",
             length(x), call. = FALSE)
      })
    }
  }

}

