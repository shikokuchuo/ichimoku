# Ichimoku ---------------------------------------------------------------------

#' Check if dates are trading days
#'
#' A function for checking if specified dates are trading days.
#'
#' @param x a POSIXct date object, or vector of POSIXct date objects.
#' @param holidays (optional) a vector, or function which outputs a
#'     vector, of dates defined as holidays. If not specified, New Year's and
#'     Christmas day are defined as holidays by default.
#' @param ... other arguments to be passed along.
#'
#' @return A vector of logical values: TRUE if the corresponding element of 'x'
#'     is a weekday and not a holiday, otherwise FALSE.
#'
#' @examples
#' tradingDays(sample_ohlc_data$Date[1:7])
#'
#' @export
#'
tradingDays <- function(x, holidays, ...) {
  baseyear <- as.POSIXlt(x[length(x)])$year + 1900
  if(missing(holidays)) {
    holidays <- c(as.POSIXct(paste0(baseyear, "-12-25")),
                  as.POSIXct(paste0(baseyear + 1, "-01-01")))
  } else {
    holidays <- tryCatch(as.POSIXct(holidays),
                         error = function(e) {
                           warning("ichimoku: specified holidays are invalid - disregarding",
                                   call. = FALSE)
                           c(as.POSIXct(paste0(baseyear, "-12-25")),
                             as.POSIXct(paste0(baseyear + 1, "-01-01")))
                         })
  }
  vapply(x, function(y, i = holidays) {
    if(!as.POSIXlt(y)$wday %in% 1:5) FALSE
    else if(y %in% i) FALSE
    else TRUE
  }, logical(1), USE.NAMES = FALSE)
}
