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
#' plot(cloud)
#' print(cloud[101:120, ], plot = FALSE)
#'
#' kumo <- ichimoku(TKR, ticker = "TKR Co.", periods = c(9, 26, 52), keep.data = TRUE)
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
    index <- tryCatch(as.POSIXct(attr(x, "row.names")), error = function(e) {
      if (is.integer(rnames <- attr(x, "row.names")) && rnames[1L] != 1L) {
        warning("Converted numeric row names as POSIX times - please check validity", call. = FALSE)
        .POSIXct(rnames)
      } else {
        stop("valid date-time index not found. Perhaps check column names?", call. = FALSE)
      }
    })
  } else {
    index <- tryCatch(as.POSIXct(.subset2(x, coli)), error = function(e) {
      if (is.numeric(idxcol <- .subset2(x, coli))) {
        warning("Converted numeric values in column '", cnames[coli], "' as POSIX times - please check validity", call. = FALSE)
        .POSIXct(idxcol)
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

  periodicity <- min(unclass(index[2:4]) - unclass(index[1:3]))
  if (periodicity == 86400) {
    seq <- seq.POSIXt(from = index[xlen], by = periodicity, length.out = p2 + p2)[-1L]
    future <- seq[tradingDays(seq, ...)][1:(p2 - 1L)]
  } else {
    future <- seq.POSIXt(from = index[xlen], by = periodicity, length.out = p2)[-1L]
  }
  xtsindex <- unclass(c(index, future))
  attributes(xtsindex) <- list(tzone = Sys.getenv("TZ"), tclass = class(future))

  lk <- kmatrix <- NULL
  if (!missing(keep.data) && isTRUE(keep.data)) {
    cols <- c("coli", "colo", "colh", "coll", "colc", "colp")
    used <- do.call(c, lapply(cols, function(x) {
      if (exists(x, where = parent.frame(2L), inherits = FALSE)) get(x, pos = parent.frame(2L), inherits = FALSE)
    }))
    used <- used[!is.na(used)]
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

#' Print Ichimoku Objects
#'
#' Default print method for ichimoku objects to enable automatic plotting of the
#'     ichimoku cloud chart.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot [default TRUE] set to FALSE to prevent automatic plotting of
#'     the ichimoku cloud chart.
#' @param ... additional arguments passed along to print() and plot() functions.
#'
#' @return The ichimoku object supplied (invisibly). The data is printed to the
#'     console. The ichimoku cloud chart is also output to the graphical device
#'     depending on the parameters set.
#'
#' @details This function is an S3 method for the generic function print() for
#'     class 'ichimoku'. It can be invoked by calling print(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' print(cloud, max = 110, digits = 4)
#' print(cloud[100:110,], plot = FALSE, digits = 4)
#'
#' @method print ichimoku
#' @export
#'
print.ichimoku <- function(x, plot = TRUE, ...) {

  if (missing(plot) || isTRUE(plot)) tryCatch(plot.ichimoku(x, ...),
                                              error = function(e) invisible(),
                                              warning = function(w) invisible())
  NextMethod()
  invisible(x)

}

#' Display the Structure of Ichimoku Objects
#'
#' Compactly display the internal structure of ichimoku objects.
#'
#' @param object an object of class 'ichimoku'.
#' @param ... arguments passed to or from other methods.
#'
#' @return Invisible NULL. A compact display of the structure of the object is
#'     output to the console.
#'
#' @details This function is an S3 method for the generic function str()
#'     for class 'ichimoku'. It can be invoked by calling str(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' str(cloud)
#'
#' strat <- strat(cloud)
#' str(strat)
#'
#' @rdname str.ichimoku
#' @method str ichimoku
#' @export
#'
str.ichimoku <- function (object, ...) {

  index <- index.ichimoku(object)
  if (is.null(dims <- attr(object, "dim"))) {
    cat("ichimoku object with no dimensions")
    dates <- format.POSIXct(c(index[1L], index[xlen <- length(index)]))
    cat("\nVector <numeric> w/ length:", xlen)
    cat("\n index: <POSIXct>", dates[1L], "...", dates[2L])
  } else {
    dates <- format.POSIXct(c(index[1L], index[dim1 <- dims[1L]]))
    cat("ichimoku object [", dates[1L], " / ", dates[2L], "]", if (hasStrat(object)) " w/ strat", sep = "")
    cat("\nMatrix <numeric> w/ dim: (", dim1, " rows, ", dims[2L], " cols)\n dimnames[[2L]]: $", sep = "")
    cat(attr(object, "dimnames")[[2L]], sep = " $")
    cat("\n index: <POSIXct>", dates[1L], "...", dates[2L])
  }
  cat("\nAttributes:\n periods:", attr(object, "periods"),
      "\n periodicity:",
      if ((periodicity <- attr(object, "periodicity")) >= 86400) {
        paste0(round(periodicity / 86400, digits = 1), " days")
      } else if (periodicity >= 3600) {
        paste0(round(periodicity / 3600, digits = 1), " hours")
      } else if (periodicity >= 60) {
        paste0(round(periodicity / 60, digits = 1), " mins")
      } else {
        paste0(periodicity, " secs")
      },
      "\n ticker:", attr(object, "ticker"), "\n")
  if (hasStrat(object)) cat(" strat: [strategy: ", attr(object, "strat")["Strategy", ][[1L]],
                            " w/ direction: ", attr(object, "strat")["Direction", ][[1L]], "... ]\n", sep = "")

}

#' Summary of Ichimoku Objects and Strategies
#'
#' Display summary information for an ichimoku object or its strategy.
#'
#' @param object an object of class 'ichimoku'.
#' @param strat [default TRUE] to show the strategy summary if present. Set to
#'     FALSE to show the object summary instead.
#' @param ... arguments passed to or from other methods.
#'
#' @return A matrix containing the strategy summary, if present and 'strat' is
#'     set to TRUE, otherwise a character vector containing an abbreviated object
#'     summary (the full object summary is output to the console).
#'
#' @details This function is an S3 method for the generic function summary() for
#'     class 'ichimoku'. It can be invoked by calling summary(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#'     Performs basic validation for an ichimoku object and will inform if an
#'     ichimoku object contains invalid information.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")} and the strategies
#'     vignette by calling: \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' summary(cloud)
#'
#' strat <- strat(cloud)
#' summary(strat)
#'
#' @method summary ichimoku
#' @export
#'
summary.ichimoku <- function(object, strat = TRUE, ...) {

  if (hasStrat(object) && isTRUE(strat)) {
    summary <- NULL
    tryCatch(attr(object, "strat")["Strategy", ],
             error = function(e) cat(summary <<- "ichimoku object with invalid strategy"))
    if (is.null(summary)) attr(object, "strat") else invisible(summary)

  } else {
    (!is.integer(periods <- attr(object, "periods")) || length(periods) != 3L ||
       !is.numeric(periodicity <- attr(object, "periodicity")) || length(periodicity) != 1L) && {
         cat(summary <- "ichimoku object with invalid attributes")
      return(invisible(summary))
    }
    if (is.null(dims <- attr(object, "dim"))) {
      cat(summary <- "ichimoku object with no dimensions", "\n")
    } else if ((dim2 <- dims[2L]) < 12L) {
      cat(summary <- "incomplete ichimoku object (partial or subset)", "\n")
    } else {
      cat(summary <- paste0("ichimoku object with dimensions (", dim1 <- dims[1L], ", ", dim2, ")"), "\n")
      if (dim1 != 0L) {
        idx <- index.ichimoku(object)
        core <- coredata.ichimoku(object)
        end <- sum(!is.na(core[, "close"]))
        high <- which.max(core[1:end, "high"])
        low <- which.min(core[1:end, "low"])
        dates <- format.POSIXct(c(idx[1L], idx[high], idx[low], idx[end]))
        cat("\n            Max: ", dates[2L], " [", core[high, "high"],
            "]\nStart: ", dates[1L], " [", core[1L, "open"],
            "]   End: ", dates[4L], " [", core[end, "close"],
            "]\n            Min: ", dates[3L], " [", core[low, "low"], "]\n", sep = "")
      }
    }

    cat("\nCloud periods:", periods, "\nPeriodicity:",
        if (periodicity >= 86400) {
          paste0(round(periodicity / 86400, digits = 1), " days")
        } else if (periodicity >= 3600) {
          paste0(round(periodicity / 3600, digits = 1), " hours")
        } else if (periodicity >= 60) {
          paste0(round(periodicity / 60, digits = 1), " mins")
        } else {
          paste0(periodicity, " secs")
        },
        "\nTicker:", attr(object, "ticker"))

    invisible(summary)

  }

}

#' Convert ichimoku to data.frame
#'
#' An optimised 'ichimoku' to 'data.frame' constructor.
#'
#' @param x an object of class 'ichimoku'.
#' @param row.names not used.
#' @param optional not used.
#' @param keep.attrs (optional) if set to TRUE, will preserve any custom
#'     attributes set on the original object.
#' @param ... arguments passed to or from other methods.
#'
#' @return A 'data.frame' object. The ichimoku object index is preserved as the
#'     first column with header 'index'.
#'
#' @details This function is an S3 method for the generic function
#'     as.data.frame() for class 'ichimoku'. It can be invoked by calling
#'     as.data.frame(x) on an object 'x' of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' df <- as.data.frame(cloud)
#' str(df)
#'
#' df2 <- as.data.frame(cloud, keep.attrs = TRUE)
#' str(df2)
#'
#' @method as.data.frame ichimoku
#' @export
#'
as.data.frame.ichimoku <- function(x, row.names, optional, keep.attrs, ...) {
  core <- coredata.ichimoku(x)
  dims <- dim(core)
  len <- dims[2L]
  df <- vector(mode = "list", length = len + 1L)
  df[[1L]] <- index.ichimoku(x)
  for (i in seq_len(len)) {
    df[[i + 1L]] <- core[, i]
  }
  attributes(df) <- c(list(names = c("index", dimnames(core)[[2L]]),
                           class = "data.frame",
                           row.names = .set_row_names(dims[1L])),
                      if (!missing(keep.attrs) && isTRUE(keep.attrs)) look(x))
  df
}

#' @name coredata
#' @rdname coredata.ichimoku
#' @export
NULL

#' Extract the Core Data of Ichimoku Objects
#'
#' Method for extracting the core data matrix of ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param fmt (optional) set to TRUE to retain the index as row names of the
#'     returned matrix, or a character string passed on to the 'format' argument
#'     of \code{format.POSIXct()} to format these values in a specific way.
#' @param ... arguments passed to or from other methods.
#'
#' @return A numeric matrix containing the ichimoku object data, stripped of the
#'     index unless 'fmt' is specified in which case the index will be retained
#'     as character values in the matrix row names.
#'
#' @details This function is an S3 method for the generic function coredata()
#'     for class 'ichimoku'. It can be invoked by calling coredata(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' coredata(cloud)[101:120, ]
#'
#' @rdname coredata.ichimoku
#' @method coredata ichimoku
#' @export
#'
coredata.ichimoku <- function(x, fmt, ...) {
  attributes(x) <- if (missing(fmt)) {
    list(dim = attr(x, "dim"), dimnames = attr(x, "dimnames"))
  } else if (is.null(attr(x, "dim"))) {
    list(names = if (is.character(fmt)) format.POSIXct(index.ichimoku(x), format = fmt) else format.POSIXct(index.ichimoku(x)))
  } else {
    list(dim = attr(x, "dim"),
         dimnames = list(if (is.character(fmt)) format.POSIXct(index.ichimoku(x), format = fmt) else format.POSIXct(index.ichimoku(x)),
                         attr(x, "dimnames")[[2L]]))
  }
  x
}

#' @name index
#' @rdname index.ichimoku
#' @export
NULL

#' Extract the Index of Ichimoku Objects
#'
#' Method for extracting the date-time index of ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param ... arguments passed to or from other methods.
#'
#' @return The date-time index of the ichimoku object as a vector of POSIXct
#'     values.
#'
#' @details This function is an S3 method for the generic function index()
#'     for class 'ichimoku'. It can be invoked by calling index(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' index(cloud)[101:120]
#'
#' @rdname index.ichimoku
#' @method index ichimoku
#' @export
#'
index.ichimoku <- function(x, ...) {
  idx <- attr(x, "index")
  class(idx) <- c("POSIXct", "POSIXt")
  idx
}

#' is.ichimoku
#'
#' A function for checking if an object is an ichimoku object.
#'
#' @param x an object.
#'
#' @return A logical value of TRUE if 'x' is of class 'ichimoku', otherwise FALSE.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#'
#' # TRUE:
#' is.ichimoku(cloud)
#' # FALSE:
#' is.ichimoku(sample_ohlc_data)
#'
#' @export
#'
is.ichimoku <- function(x) inherits(x, "ichimoku")

