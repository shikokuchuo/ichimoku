# Copyright (C) 2021-2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of ichimoku.
#
# ichimoku is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ichimoku is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# ichimoku. If not, see <https://www.gnu.org/licenses/>.

# Ichimoku - Core Functions ----------------------------------------------------

#' ichimoku
#'
#' Create an ichimoku object containing values for all components of the
#'     Ichimoku Kinko Hyo cloud chart. The object encapsulates a date-time
#'     index, OHLC pricing data, candle direction, the cloud lines Tenkan-sen,
#'     Kijun-sen, Senkou span A, Senkou span B and Chikou span, as well as
#'     values for the cloud top and cloud base.
#'
#' @param x a data.frame or other compatible object, which includes xts,
#'     data.table, tibble, and matrix.
#' @param ticker (optional) specify a ticker to identify the instrument,
#'     otherwise this is set to the name of the input object.
#' @param periods [default c(9L, 26L, 52L)] a vector defining the length of
#'     periods used for the cloud. This parameter shoud not normally be modified
#'     as using other values would be invalid in the context of traditional
#'     ichimoku analysis.
#' @param keep.data (optional) set to TRUE to retain additional data present
#'     in the input object as additional columns and/or attributes.
#' @param ... additional arguments, for instance 'holidays', passed along to
#'     \code{\link{tradingDays}} for calculating the future cloud on daily data.

#'
#' @return An ichimoku object with S3 classes of 'ichimoku', 'xts' and 'zoo'.
#'
#' @details Calling an ichimoku object automatically invokes its print method,
#'     which by default produces a printout of the data to the console as well
#'     as a plot of the cloud chart to the graphical device.
#'
#'     For further options, use \code{plot()} on the returned ichimoku object to
#'     pass further arguments for customising the chart. Use \code{iplot()} for
#'     interactive charting.
#'
#'     Where an ichimoku object is passed to \code{ichimoku()}, the ichimoku
#'     object is re-calculated using the OHLC pricing data contained within.
#'
#' @section Ichimoku Object Specification:
#'
#'     Index:
#'     \itemize{
#'         \item{\code{index(object)}:} {date-time index [POSIXct]}
#'      }
#'     Columns [numeric]:
#'     \itemize{
#'         \item{\code{object$open}:} {opening price}
#'         \item{\code{$high}:} {high price}
#'         \item{\code{$low}:} {low price}
#'         \item{\code{$close}:} {closing price}
#'         \item{\code{$cd}:} {candle direction (-1 = down, 0 = flat, 1 = up)}
#'         \item{\code{$tenkan}:} {Tenkan-sen}
#'         \item{\code{$kijun}:} {Kijun-sen}
#'         \item{\code{$senkouA}:} {Senkou span A}
#'         \item{\code{$senkouB}:} {Senkou span B}
#'         \item{\code{$chikou}:} {Chikou span}
#'         \item{\code{$cloudT}:} {cloud Top (max of senkouA, senkouB)}
#'         \item{\code{$cloudB}:} {cloud Base (min of senkouA, senkouB)}
#'      }
#'     Attributes:
#'     \itemize{
#'         \item{\code{attributes(object)$periods}:} { parameters used to
#'         calculate the cloud [integer vector of length 3]}
#'         \item{\code{$periodicity}:} { periodicity of the
#'         data in seconds [numeric]}
#'         \item{\code{$ticker}:} { instrument identifier [character]}
#'      }
#'
#' @section Further Details:
#'
#'     \code{ichimoku()} requires OHLC (or else HLC) price data as input to
#'     calculate the cloud chart values.
#'
#'     If only single series price data is supplied, a \emph{pseudo} OHLC series
#'     is generated and a \emph{pseudo} cloud chart is returned.
#'
#'     A faster technical utility version of this function is available in
#'     \code{\link{.ichimoku}} for use when the data is already in the required
#'     format.
#'
#'     Please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' TKR <- sample_ohlc_data
#'
#' cloud <- ichimoku(TKR)
#' cloud
#'
#' kumo <- ichimoku(TKR, ticker = "TKR Co.", periods = c(9, 26, 52), keep.data = TRUE)
#' summary(kumo)
#' print(kumo, plot = FALSE, rows = 10)
#' plot(kumo, theme = "solarized", type = "bar", custom = "volume")
#'
#' @rdname ichimoku
#' @export
#'
ichimoku <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) UseMethod("ichimoku")

#' @rdname ichimoku
#' @method ichimoku ichimoku
#' @export
#'
ichimoku.ichimoku <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  if (missing(ticker)) ticker <- attr(x, "ticker")
  x <- x[!is.na(coredata.ichimoku(x)[, "close"]), ]

  if (!missing(keep.data) && isTRUE(keep.data)) {
    x$cd <- x$tenkan <- x$kijun <- x$senkouA <- x$senkouB <- x$chikou <- x$cloudT <- x$cloudB <- NULL
    x <- as.data.frame.ichimoku(x, keep.attrs = TRUE)
  } else {
    x <- as.data.frame.ichimoku(x)
  }

  ichimoku.data.frame(x, ticker = ticker, periods = periods, keep.data = keep.data, ...)

}

#' @rdname ichimoku
#' @method ichimoku xts
#' @export
#'
ichimoku.xts <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  x <- xts_df(x, keep.attrs = !missing(keep.data) && isTRUE(keep.data))

  ichimoku.data.frame(x, ticker = ticker, periods = periods, keep.data = keep.data, ...)

}

#' @rdname ichimoku
#' @method ichimoku data.frame
#' @export
#'
ichimoku.data.frame <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  xlen <- dim(x)[1L]
  cnames <- attr(x, "names")

  coli <- grep("index|date|time", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (is.na(coli)) {
    index <- if (is.integer(rnames <- attr(x, "row.names"))) {
      rnames[1L] == 1L && stop("valid date-time index not found. Perhaps check column names?",
                               call. = FALSE)
      warning("Converted numeric row names as POSIX times - please check validity", call. = FALSE)
      rnames
    } else {
      tryCatch(unclass(as.POSIXct(rnames <- attr(x, "row.names"))),
               error = function(e) stop("valid date-time index not found. Perhaps check column names?",
                                        call. = FALSE))
    }

  } else {
    index <- if (is.numeric(idxcol <- .subset2(x, coli))) {
      warning("Converted numeric values in column '", cnames[coli],
              "' as POSIX times - please check validity", call. = FALSE)
      idxcol
    } else {
      tryCatch(unclass(as.POSIXct(idxcol <- .subset2(x, coli))),
               error = function(e) stop("column '", cnames[coli],
                                        "' is not convertible to a POSIXct date-time format",
                                        call. = FALSE))
    }
  }

  colh <- grep("high", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  coll <- grep("low", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  colc <- grep("close", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (anyNA(c(colh, coll, colc))) {
    colp <- grep("price|value|close", cnames, ignore.case = TRUE, perl = TRUE)[1L]
    is.na(colp) && stop("price data not found. Perhaps check column names?", call. = FALSE)
    close <- as.numeric(.subset2(x, colp))
    open <- c(NA, `length<-`(close, xlen - 1L))
    high <- pmax.int(open, close)
    low <- pmin.int(open, close)
    warning("OHLC data not found - using pseudo-OHLC data constructed from '", cnames[colp],
            "'\nResulting ichimoku cloud chart is an approximation only", call. = FALSE)

  } else {
    high <- as.numeric(.subset2(x, colh))
    low <- as.numeric(.subset2(x, coll))
    close <- as.numeric(.subset2(x, colc))
    colo <- grep("open", cnames, ignore.case = TRUE, perl = TRUE)[1L]
    if (is.na(colo)) {
      warning("Opening prices not found - using previous closing prices as substitute",
              "\nThis affects the candles but not the calculation of the cloud chart", call. = FALSE)
      open <- c(NA, `length<-`(close, xlen - 1L))
    } else {
      open <- as.numeric(.subset2(x, colo))
    }
  }

  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1L)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - reverting to defaults c(9L, 26L, 52L)", call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p1 <- periods[1L]
  p2 <- periods[2L]
  p3 <- periods[3L]
  xlen > p2 || stop("dataset must be longer than the medium cloud period '", p2, "'", call. = FALSE)
  p21 <- p2 - 1L
  clen <- xlen + p21

  cd <- numeric(xlen)
  cd[open < close] <- 1
  cd[open > close] <- -1
  tenkan <- (.Call(ichimoku_wmax, high, p1) + .Call(ichimoku_wmin, low, p1)) / 2
  kijun <- (.Call(ichimoku_wmax, high, p2) + .Call(ichimoku_wmin, low, p2)) / 2
  senkouA <- (tenkan + kijun) / 2
  senkouB <- (.Call(ichimoku_wmax, high, p3) + .Call(ichimoku_wmin, low, p3)) / 2
  cloudT <- pmax.int(senkouA, senkouB)
  cloudB <- pmin.int(senkouA, senkouB)

  periodicity <- min(index[2:4] - `length<-`(index, 3L))
  if (periodicity == 86400) {
    future <- seq.int(from = index[xlen] + periodicity, by = periodicity, length.out = p2 + p2)
    future <- `length<-`(future[tradingDays(future, ...)], p21)
  } else {
    future <- seq.int(from = index[xlen] + periodicity, by = periodicity, length.out = p21)
  }
  xtsindex <- c(index, future)

  if (missing(keep.data) || !isTRUE(keep.data)) {
    x <- kmatrix <- NULL
  } else {
    used <- unlist(lapply(c("coli", "colo", "colh", "coll", "colc", "colp"),
                          function(x) get0(x, envir = parent.frame(2L), inherits = FALSE)))
    used <- used[!is.na(used)]
    keep <- if (!is.null(used)) cnames[-used]
    kmatrix <- do.call(cbind, lapply(.subset(x, keep),
                                     function(x) `length<-`(as.numeric(x), clen)))
  }

  kumo <- cbind(open = `length<-`(open, clen),
                high = `length<-`(high, clen),
                low = `length<-`(low, clen),
                close = `length<-`(close, clen),
                cd = `length<-`(cd, clen),
                tenkan = `length<-`(tenkan, clen),
                kijun = `length<-`(kijun, clen),
                senkouA = c(rep(NA, p21), senkouA),
                senkouB = c(rep(NA, p21), senkouB),
                chikou = `length<-`(close[p2:xlen], clen),
                cloudT = c(rep(NA, p21), cloudT),
                cloudB = c(rep(NA, p21), cloudB),
                kmatrix)

  .Call(ichimoku_create, kumo, xtsindex, periods, periodicity, ticker, x)

}

#' @rdname ichimoku
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  x <- matrix_df(x, keep.attrs = !missing(keep.data) && isTRUE(keep.data))

  ichimoku.data.frame(x, ticker = ticker, periods = periods, keep.data = keep.data, ...)

}

#' @rdname ichimoku
#' @method ichimoku default
#' @export
#'
ichimoku.default <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  is.character(x) || stop("cannot create an ichimoku object from a '", class(x)[1L], "' object", call. = FALSE)
  exists(x) || stop("object '", x, "' not found", call. = FALSE)
  identical(x, object <- get(x)) && stop("cannot create an ichimoku object from a 'character' object", call. = FALSE)

  ichimoku(object, ticker = if (missing(ticker)) x else ticker, periods = periods, keep.data = keep.data, ...)

}

#' ichimoku Technical Utility Version
#'
#' Create an ichimoku object containing values for all components of the
#'     Ichimoku Kinko Hyo cloud chart. The object encapsulates a date-time
#'     index, OHLC pricing data, candle direction, the cloud lines Tenkan-sen,
#'     Kijun-sen, Senkou span A, Senkou span B and Chikou span, as well as
#'     values for the cloud top and cloud base.
#'
#' @param x a data.frame object with a POSIXct date-time index as the first
#'     column and numeric OHLC pricing data as the second through fifth columns.
#' @inheritParams ichimoku
#'
#' @details A faster version of \code{\link{ichimoku}} which can be used when
#'     the data is a dataframe in the prescribed format. Does not support the
#'     argument 'keep.data'.
#'
#' @return An ichimoku object with S3 classes of 'ichimoku', 'xts' and 'zoo'.
#'
#' @keywords internal
#' @export
#'
.ichimoku <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  xlen <- dim(x)[1L]
  cnames <- attr(x, "names")
  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1L)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - reverting to defaults c(9L, 26L, 52L)", call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p1 <- periods[1L]
  p2 <- periods[2L]
  p3 <- periods[3L]
  xlen > p2 || stop("dataset must be longer than the medium cloud period '", p2, "'", call. = FALSE)
  p21 <- p2 -1L
  clen <- xlen + p21

  index <- unclass(.subset2(x, 1L))
  open <- .subset2(x, 2L)
  high <- .subset2(x, 3L)
  low <- .subset2(x, 4L)
  close <- .subset2(x, 5L)
  cd <- numeric(xlen)
  cd[open < close] <- 1
  cd[open > close] <- -1
  tenkan <- (.Call(ichimoku_wmax, high, p1) + .Call(ichimoku_wmin, low, p1)) / 2
  kijun <- (.Call(ichimoku_wmax, high, p2) + .Call(ichimoku_wmin, low, p2)) / 2
  senkouA <- (tenkan + kijun) / 2
  senkouB <- (.Call(ichimoku_wmax, high, p3) + .Call(ichimoku_wmin, low, p3)) / 2
  cloudT <- pmax.int(senkouA, senkouB)
  cloudB <- pmin.int(senkouA, senkouB)

  periodicity <- min(index[2:4] - `length<-`(index, 3L))
  if (periodicity == 86400) {
    future <- seq.int(from = index[xlen] + periodicity, by = periodicity, length.out = p2 + p2)
    future <- `length<-`(future[tradingDays(future, ...)], p21)
  } else {
    future <- seq.int(from = index[xlen] + periodicity, by = periodicity, length.out = p21)
  }
  xtsindex <- c(index, future)
  x <- NULL

  kumo <- cbind(open = `length<-`(open, clen),
                high = `length<-`(high, clen),
                low = `length<-`(low, clen),
                close = `length<-`(close, clen),
                cd = `length<-`(cd, clen),
                tenkan = `length<-`(tenkan, clen),
                kijun = `length<-`(kijun, clen),
                senkouA = c(rep(NA, p21), senkouA),
                senkouB = c(rep(NA, p21), senkouB),
                chikou = `length<-`(close[p2:xlen], clen),
                cloudT = c(rep(NA, p21), cloudT),
                cloudB = c(rep(NA, p21), cloudB))

  .Call(ichimoku_create, kumo, xtsindex, periods, periodicity, ticker, x)

}

