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
#'     data.table, tibble, and matrix formats.
#' @param ticker (optional) specify a ticker to identify the instrument,
#'     otherwise this is set to the name of the input object 'x'.
#' @param periods [default c(9L, 26L, 52L)] a vector defining the length of
#'     periods used for the cloud. This parameter shoud not normally be modified
#'     as using other values would be invalid in the context of traditional
#'     Ichimoku analysis.
#' @param ... additional arguments, for instance 'holidays', passed along to
#'     \code{\link{tradingDays}} for calculating the future cloud on daily data.
#'
#' @return An ichimoku object is returned with S3 classes of 'ichimoku', 'xts'
#'     and 'zoo'.
#'
#' @details Calling an ichimoku object automatically invokes its print method,
#'     which will by default produce a printout of the data to the console as
#'     well as a static plot of the cloud chart to the graphical device.
#'
#'     For further options, please use plot() on the returned ichimoku object to
#'     pass further arguments for customising the chart. Use iplot() for
#'     interactive charting.
#'
#'     Where an ichimoku object is passed to ichimoku(), the ichimoku object is
#'     re-calculated using the OHLC pricing data contained within.
#'
#' @section Object Specification:
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
#' @section Working with ichimoku objects:
#'     An ichimoku object inherits the 'xts' and 'zoo' classes. For convenience,
#'     the following functions are re-exported by ichimoku:
#'
#'     \emph{from 'zoo':}
#'      \itemize{
#'         \item{\code{index()}:} { to extract the index of an ichimoku object}
#'         \item{\code{coredata()}:} { to extract the columns of an ichimoku
#'         object as a numeric matrix}
#'      }
#'
#'      \emph{from 'xts':}
#'      \itemize{
#'         \item{\code{xts()}:} { to create an 'xts' object from data and a
#'         date-time index use \code{xts(data, index)}}
#'      }
#'
#'      Additional methods are available by loading the 'xts' package.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9L, 26L, 52L))
#'
#' @rdname ichimoku
#' @export
#'
ichimoku <- function(x, ...) UseMethod("ichimoku")

#' @rdname ichimoku
#' @method ichimoku ichimoku
#' @export
#'
ichimoku.ichimoku <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(ticker)) ticker <- attr(x, "ticker")
  x <- xts_df(x)
  x <- x[!is.na(x$close), ]

  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)

}

#' @rdname ichimoku
#' @method ichimoku xts
#' @export
#'
ichimoku.xts <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  x <- xts_df(x)

  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)

}

#' @rdname ichimoku
#' @method ichimoku data.frame
#' @export
#'
ichimoku.data.frame <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))

  cnames <- attr(x, "names")
  coli <- grep("index|date|time", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (!is.na(coli)) {
    index <- tryCatch(as.POSIXct(x[, coli, drop = TRUE]), error = function(e) {
      stop("Index/date/time column not convertible to a POSIXct date-time format",
           call. = FALSE)
      })
  } else {
    index <- tryCatch(as.POSIXct(attr(x, "row.names")), error = function(e) {
      stop("Valid date-time index not found within the dataset",
           call. = FALSE)
    })
  }
  colh <- grep("high", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  coll <- grep("low", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  colc <- grep("close", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (anyNA(c(colh, coll, colc))) {
    stop("Clearly-defined high/low/close columns not found within the dataset", call. = FALSE)
  }
  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - using defaults c(9L, 26L, 52L) instead",
            call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p1 <- periods[1L]
  p2 <- periods[2L]
  p3 <- periods[3L]
  xlen <- dim(x)[1L]
  if (p2 >= xlen) stop("Dataset must be longer than the medium cloud period '",
                       p2, "'", call. = FALSE)

  high <- as.numeric(x[, colh, drop = TRUE])
  low <- as.numeric(x[, coll, drop = TRUE])
  close <- as.numeric(x[, colc, drop = TRUE])
  colo <- grep("open", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (!is.na(colo)) {
    open <- as.numeric(x[, colo, drop = TRUE])
  } else {
    warning("Opening prices not found - using previous closing prices as substitute",
            "\nThis affects the candles but not the calculation of the cloud", call. = FALSE)
    open <- c(NA, close[1:(xlen - 1L)])
  }
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
  future <- switch(attr(periodicity, "units"),
                   days = {
                     seq <- seq.POSIXt(from = index[length(index)], by = periodicity,
                                       length.out = p2 + p2)[-1L]
                     seq[tradingDays(seq, ...)][1:(p2 - 1L)]
                     },
                   seq.POSIXt(from = index[length(index)], by = periodicity,
                              length.out = p2)[-1L])

  structure(xts(cbind(open = c(open, rep(NA, p2 - 1L)),
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
                      cloudB = c(rep(NA, p2 - 1L), cloudB)),
                order.by = c(index, future)),
            class = c("ichimoku", "xts", "zoo"),
            periods = periods,
            periodicity = as.double.difftime(periodicity, units = "secs"),
            ticker = ticker)

}

#' @rdname ichimoku
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(ticker)) ticker <- deparse(substitute(x))
  x <- matrix_df(x)

  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)

}

#' @rdname ichimoku
#' @method ichimoku default
#' @export
#'
ichimoku.default <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {

  if (missing(x)) stop("No object specified for ichimoku()", call. = FALSE)
  tryExists <- tryCatch(exists(x), error = function(e) {
    stop("Cannot create an ichimoku object from a '", class(x)[1L], "' object", call. = FALSE)
  })

  if (!tryExists) {
    stop("object '", x, "' not found")
  } else if (missing(ticker)) {
    ichimoku(get(x), ticker = x, periods = periods, ...)
  } else {
    ichimoku(get(x), ticker = ticker, periods = periods, ...)
  }

}

##' @importFrom ggplot2 autoplot
##' @name autoplot
##' @rdname autoplot.ichimoku
##' @export
NULL

#' autoplot.ichimoku
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param object an object of class 'ichimoku'.
#' @param window (optional) a date-time window to subset the plot, in ISO-8601
#'     compatible range strings of the format used for 'xts' objects, for example
#'     "2020-02-15/2020-08-15" or "2020-02-15/", "/2020-08" or "2020-07".
#' @param ticker (optional) specify a ticker (or other text) to include in the
#'     chart heading. If not set, the ticker saved within the ichimoku object
#'     will be used.
#' @param message (optional) specify a chart message to display under the title.
#' @param theme [default 'original'] with alternative choices of 'dark',
#'     'solarized' or 'mono'.
#' @param strat [default TRUE] if the ichimoku object contains a strategy, the
#'     periods for which the strategy results in a position will be shaded, and
#'     the strategy printed as the chart message (if a message is not already
#'     specified). Set to FALSE to turn off this behaviour.
#' @param ... other arguments not used by this method.
#'
#' @return Returns a ggplot2 object with S3 classes 'gg' and 'ggplot'.
#'
#' @details This function is an S3 method for the generic function autoplot()
#'     for class 'ichimoku'. It can be invoked by calling autoplot(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' autoplot(cloud)
#' autoplot(cloud, window = "2020-05-01/2020-11-02", theme = "dark")
#' autoplot(cloud, window = "2020-04/", ticker = "TKR Co.", theme = "solarized")
#' autoplot(cloud, window = "/2020-11-05", message = "Sample Price Data", theme = "mono")
#'
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @export
#'
autoplot.ichimoku <- function(object,
                              window,
                              ticker,
                              message,
                              theme = c("original", "dark", "solarized", "mono"),
                              strat = TRUE,
                              ...) {

  theme <- match.arg(theme)
  pal <- ichimoku_themes[, theme]
  periodicity <- attr(object, "periodicity")
  if (missing(ticker)) ticker <- attr(object, "ticker")
  if (missing(message)) {
    message <- if (hasStrat(object) && isTRUE(strat)) paste0("Strategy: ",
                                                             attr(object, "strat")["Strategy", ][[1]])
  }

  if (!missing(window)) object <- object[window]
  xlen <- dim(object)[1L]
  data <- xts_df(object)
  data$idx <- seq_len(xlen)
  data$cd <- as.character(data$cd)

  layers <- list(
    if (hasStrat(object) && isTRUE(strat)) {
      geom_rect(aes(xmin = .data$posn * (.data$idx - 0.5),
                    xmax = .data$posn * (.data$idx + 0.5),
                    ymin = -Inf, ymax = Inf), fill = pal[1L], alpha = 0.2, na.rm = TRUE)
    },
    if (!all(is.na(data$senkouB))) {
      geom_ribbon(aes(ymax = .data$senkouA, ymin = .data$senkouB),
                  fill = pal[1L], alpha = 0.6, na.rm = TRUE)
    },
    geom_line(aes(y = .data$senkouA), col = pal[2L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouB), col = pal[3L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$tenkan), col = pal[4L], na.rm = TRUE),
    geom_line(aes(y = .data$kijun), col = pal[5L], na.rm = TRUE),
    geom_segment(aes(xend = .data$idx, y = .data$high, yend = .data$low, colour = .data$cd),
                 size = 0.3, na.rm = TRUE),
    geom_rect(aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4,
                  ymin = .data$open, ymax = .data$close,
                  colour = .data$cd, fill = .data$cd),
              size = 0.3, na.rm = TRUE),
    geom_line(aes(y = .data$chikou), col = pal[6L], na.rm = TRUE),
    scale_x_continuous(breaks = function(x) {
      if (periodicity > 80000) {
        len <- length(endpoints(object, on = "months"))
        if (len < 100L) {
          k <- ceiling(len / 13)
          breaks <- endpoints(object, on = "months", k = k) + 1L
        } else {
          k <- ceiling(len / 156)
          breaks <- endpoints(object, on = "years", k = k) + 1L
        }
        if (len > 4L) {
          cond <- (breaks[length(breaks)] - breaks[(length(breaks) - 1L)]) < 0.7 * (breaks[3L] - breaks[2L])
          cond2 <- (breaks[2L] - breaks[1L]) < 0.7 * (breaks[3L] - breaks[2L])
          if (cond) breaks <- breaks[-length(breaks)]
          if (cond2) breaks <- breaks[-1L]
        }
        if (breaks[length(breaks)] > xlen) breaks[length(breaks)] <- breaks[length(breaks)] - 1L
      } else {
        breaks <- pretty.default(data$idx, n = 9L) + 1
        if (breaks[length(breaks)] > xlen) breaks <- breaks[-length(breaks)]
      }
      breaks
    }, labels = function(x) {
      labels <- data$index[x]
      if (periodicity > 80000) {
        format(labels, paste("%d-%b", "%Y", sep = "\n"))
      } else {
        format(labels, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
      }
    }),
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)),
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])),
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])),
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker),
         subtitle = message),
    theme_light(),
    switch(theme,
           dark = theme(legend.position = "none",
                        plot.title = element_text(colour = "#eee8d5"),
                        plot.subtitle = element_text(colour = "#eee8d5"),
                        plot.background = element_rect(fill = "#586e75", colour = NA),
                        panel.background = element_rect(fill = "#002b36", colour = NA),
                        panel.grid = element_line(colour = "#073642"),
                        axis.title = element_text(colour = "#eee8d5"),
                        axis.text = element_text(colour = "#eee8d5"),
                        axis.ticks = element_line(colour = "#eee8d5")),
           theme(legend.position = "none"))
  )

  ggplot(data = data, aes(x = .data$idx)) + layers

}

#' Plot Ichimoku Cloud Chart
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param ... additional arguments passed along to the print method for 'ggplot'
#'     objects.
#' @inheritParams autoplot
#'
#' @return Returns a ggplot2 object with classes 'gg' and 'ggplot'.
#'
#' @details This function is an S3 method for the generic function plot() for
#'     class 'ichimoku'. It can be invoked by calling plot(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' plot(cloud)
#' plot(cloud, window = "2020-05-01/2020-11-02", theme = "dark")
#' plot(cloud, window = "2020-04/", ticker = "TKR Co.", theme = "solarized")
#' plot(cloud, window = "/2020-11-05", message = "Sample Price Data", theme = "mono")
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x,
                          window,
                          ticker,
                          message,
                          theme = c("original", "dark", "solarized", "mono"),
                          strat = TRUE,
                          ...) {

  print(autoplot.ichimoku(x, window = window, ticker = ticker, message = message,
                          theme = theme, strat = strat), ...)

}

#' Print Ichimoku Objects
#'
#' Custom print method for ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot [default TRUE] set to FALSE to prevent automatic plotting of
#'     the ichimoku cloud chart.
#' @param ... additional arguments passed along to print and plot functions.
#'
#' @return The ichimoku object 'x' passed as parameter.
#'
#' @details This function is an S3 method for the generic function print() for
#'     class 'ichimoku'. It can be invoked by calling print(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
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

  if (isTRUE(plot)) tryCatch(plot.ichimoku(x, ...),
                             error = function(e) invisible(),
                             warning = function(w) invisible())
  NextMethod(print)
  invisible(x)

}

#' Summary of Ichimoku Strategies
#'
#' Custom summary method for ichimoku objects for viewing strategies.
#'
#' @param object an object of class 'ichimoku'.
#' @param strat [default TRUE] to show the strategy summary if present. Set to
#'     FALSE to show the data summary instead.
#' @param ... additional arguments to be passed along.
#'
#' @return A matrix containing the strategy summary if present, otherwise a table
#'     containing the data summary.
#'
#' @details This function is an S3 method for the generic function summary() for
#'     class 'ichimoku'. It can be invoked by calling summary(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' strat <- strat(ichimoku(sample_ohlc_data, ticker = "TKR"))
#' summary(strat)
#'
#' @method summary ichimoku
#' @export
#'
summary.ichimoku <- function(object, strat = TRUE, ...) {

  if (hasStrat(object) && isTRUE(strat)) attr(object, "strat") else NextMethod(summary)

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

