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

