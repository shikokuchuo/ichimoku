# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' Switch Default OANDA Server
#'
#' Switch the default OANDA fxTrade server from 'practice' to 'live' or vice versa.
#'
#' @return Invisible NULL. A message informs the resulting default server setting.
#'
#' @details This function is designed to be run once at the start of each session
#'     if using a live account.
#'
#'     Re-load the package using \code{library(ichimoku)} before calling
#'     \code{oanda_switch()} to clear cached account variables if necessary.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' oanda_switch()
#' oanda_switch()
#'
#' @export
#'
oanda_switch <- function() {
  if (Sys.getenv("OANDA_ACTYPE") == "practice") {
    Sys.setenv(OANDA_ACTYPE = "live")
    message("Default OANDA server switched to 'live'")
  } else {
    Sys.setenv(OANDA_ACTYPE = "practice")
    message("Default OANDA server switched to 'practice'")
  }
}

