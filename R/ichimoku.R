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
#'     data.table, tibble, tsibble, and matrix.
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
#' print(kumo, plot = FALSE, n = 20)
#' plot(kumo, theme = "solarized", type = "bar", custom = "volume")
#'
#' @rdname ichimoku
#' @export
#'
ichimoku <- function(x, ...) UseMethod("ichimoku")

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

  if (!missing(keep.data) && isTRUE(keep.data)) {
    x <- xts_df(x, keep.attrs = TRUE)
  } else {
    x <- xts_df(x)
  }

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
    index <- tryCatch(unclass(as.POSIXct(attr(x, "row.names"))), error = function(e) {
      if (is.integer(rnames <- attr(x, "row.names")) && rnames[1L] != 1L) {
        warning("Converted numeric row names as POSIX times - please check validity", call. = FALSE)
        rnames
      } else {
        stop("valid date-time index not found. Perhaps check column names?", call. = FALSE)
      }
    })
  } else {
    index <- tryCatch(unclass(as.POSIXct(.subset2(x, coli))), error = function(e) {
      if (is.numeric(idxcol <- .subset2(x, coli))) {
        warning("Converted numeric values in column '", cnames[coli], "' as POSIX times - please check validity", call. = FALSE)
        idxcol
      } else {
        stop("column '", cnames[coli], "' is not convertible to a POSIXct date-time format", call. = FALSE)
      }
    })
  }

  colh <- grep("high", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  coll <- grep("low", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  colc <- grep("close", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (anyNA(c(colh, coll, colc))) {
    colp <- grep("price|value|close", cnames, ignore.case = TRUE, perl = TRUE)[1L]
    is.na(colp) && stop("price data not found. Perhaps check column names?", call. = FALSE)
    close <- as.numeric(.subset2(x, colp))
    open <- c(NA, close[1:(xlen - 1L)])
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
      open <- c(NA, close[1:(xlen - 1L)])
    } else {
      open <- as.numeric(.subset2(x, colo))
    }
  }

  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - reverting to defaults c(9L, 26L, 52L)", call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p1 <- periods[1L]
  p2 <- periods[2L]
  p3 <- periods[3L]
  xlen > p2 || stop("dataset must be longer than the medium cloud period '", p2, "'", call. = FALSE)

  cd <- numeric(xlen)
  cd[open < close] <- 1
  cd[open > close] <- -1
  tenkan <- (maxOver(high, p1) + minOver(low, p1)) / 2
  kijun <- (maxOver(high, p2) + minOver(low, p2)) / 2
  senkouA <- (tenkan + kijun) / 2
  senkouB <- (maxOver(high, p3) + minOver(low, p3)) / 2
  chikou <- c(close[p2:xlen], rep(NA, p2 - 1L))
  cloudT <- pmax.int(senkouA, senkouB)
  cloudB <- pmin.int(senkouA, senkouB)

  periodicity <- min(index[2:4] - index[1:3])
  if (periodicity == 86400) {
    future <- (future <- seq.int(from = index[xlen], by = periodicity, length.out = p2 + p2
                                 )[-1L])[tradingDays(.POSIXct(future), ...)][1:(p2 - 1L)]
  } else {
    future <- seq.int(from = index[xlen], by = periodicity, length.out = p2)[-1L]
  }
  xtsindex <- c(index, future)
  attributes(xtsindex) <- list(tzone = "", tclass = c("POSIXct", "POSIXt"))

  lk <- kmatrix <- NULL
  if (!missing(keep.data) && isTRUE(keep.data)) {
    used <- (used <- unlist(lapply(c("coli", "colo", "colh", "coll", "colc", "colp"), function(x) {
      if (exists(x, where = parent.frame(2L), inherits = FALSE)) get(x, pos = parent.frame(2L), inherits = FALSE)
    })))[!is.na(used)]
    keep <- if (!is.null(used)) cnames[-used]
    kmatrix <- do.call(cbind, lapply(.subset(x, keep), function(x) c(as.numeric(x), rep(NA, p2 - 1L))))
    lk <- look(x)
    lk$periods <- lk$periodicity <- lk$ticker <- NULL
  }

  kumo <- cbind(open = c(open, rep(NA, p2 - 1L)),
                high = c(high, rep(NA, p2 - 1L)),
                low = c(low, rep(NA, p2 - 1L)),
                close = c(close, rep(NA, p2 - 1L)),
                cd = c(cd, rep(NA, p2 - 1L)),
                tenkan = c(tenkan, rep(NA, p2 - 1L)),
                kijun = c(kijun, rep(NA, p2 - 1L)),
                senkouA = c(rep(NA, p2 - 1L), senkouA),
                senkouB = c(rep(NA, p2 - 1L), senkouB),
                chikou = c(chikou, rep(NA, p2 - 1L)),
                cloudT = c(rep(NA, p2 - 1L), cloudT),
                cloudB = c(rep(NA, p2 - 1L), cloudB),
                kmatrix)
  attributes(kumo) <- c(attributes(kumo),
                        list(index = xtsindex,
                             class = c("ichimoku", "xts", "zoo"),
                             periods = periods,
                             periodicity = periodicity,
                             ticker = ticker),
                        lk)
  kumo

}

#' @rdname ichimoku
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9L, 26L, 52L), keep.data, ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))

  if (!missing(keep.data) && isTRUE(keep.data)) {
    x <- matrix_df(x, keep.attrs = TRUE)
  } else {
    x <- matrix_df(x)
  }

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

  if (missing(ticker)) {
    ichimoku(object, ticker = x, periods = periods, keep.data = keep.data, ...)
  } else {
    ichimoku(object, ticker = ticker, periods = periods, keep.data = keep.data, ...)
  }

}

