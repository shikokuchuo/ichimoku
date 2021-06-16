# Ichimoku: Visualization Layer ------------------------------------------------

#' ichimoku
#'
#' Create an ichimoku object containing the values for all components of the
#'     Ichimoku Kinko Hyo cloud chart, ready for plotting. The object includes
#'     a date-time index, OHLC pricing data, candle direction, the cloud lines
#'     Tenkan-sen, Kijun-sen, Senkou span A, Senkou span B and Chikou span, as
#'     well as values for the cloud top and base.
#'
#' @param x a data.frame or other compatible object, which includes xts,
#'     data.table, tibble, and matrix.
#' @param ticker (optional) specify a ticker to identify the instrument,
#'     otherwise this is set to the name of the input object 'x'.
#' @param periods a vector defining the length of periods used for the cloud,
#'     with a default of c(9, 26, 52). This parameter shoud not normally be
#'     changed as using other values would be invalid in the context of
#'     traditional Ichimoku analysis.
#' @param ... other arguments to be passed along. For instance, 'holidays' may
#'     be passed to the \code{\link{tradingDays}} function, used by ichimoku when
#'     calculating the future cloud.
#'
#' @return An ichimoku object is returned with S3 classes of 'ichimoku', 'xts'
#'     and 'zoo'.
#'
#'     This object contains a date-time index, OHLC pricing data, candle
#'     direction, the computed ichimoku cloud values, and cloud top and base
#'     values, with ticker, periods, and periodicity parameters set as
#'     attributes.
#'
#' @details Calling an ichimoku object automatically invokes its print method,
#'     which will by default produce a printout of the data to the console as
#'     well as a static plot of the cloud chart to the graphical device.
#'
#'     For further options, including interactive charting, use plot() on the
#'     returned ichimoku object to pass further arguments for customising
#'     the chart.
#'
#'     Where an ichimoku object is passed to ichimoku(), the ichimoku object is
#'     re-calculated using the OHLC pricing data contained within.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' ichimoku(sample_ohlc_data, ticker = "TKR", periods = c(9, 26, 52))
#'
#' @rdname ichimoku
#' @export
#'
ichimoku <- function(x, ...) UseMethod("ichimoku")

#' @rdname ichimoku
#' @method ichimoku ichimoku
#' @export
#'
ichimoku.ichimoku <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(ticker)) ticker <- attr(x, "ticker")
  x <- as.data.frame(x)
  x <- x[!is.na(x$close),]
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku xts
#' @export
#'
ichimoku.xts <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(ticker)) ticker <- deparse(substitute(x))
  x <- as.data.frame(x)
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(ticker)) ticker <- deparse(substitute(x))
  x <- as.data.frame(x)
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku data.frame
#' @export
#'
ichimoku.data.frame <- function(x, ticker, periods = c(9, 26, 52), ...) {

  if(missing(ticker)) ticker <- deparse(substitute(x))

  if(!is.null(rownames(x)) &&
     !inherits(try(as.POSIXct(rownames(x)), silent = TRUE), "try-error")) {
    index <- as.POSIXct(rownames(x))
  } else if (length(grep("index|date|time", colnames(x), ignore.case = TRUE)) == 1) {
    tryCatch(index <- as.POSIXct(x[ ,grep("index|date|time", colnames(x), ignore.case = TRUE)]),
      error = function(e) {
        stop("ichimoku: dataset index not convertible to a POSIXct date-time format",
             call. = FALSE)
      })
  } else {
    stop("ichimoku: unique date-time index not found within the dataset", call. = FALSE)
  }

  tryCatch(
  stopifnot(length(grep("high", colnames(x), ignore.case = TRUE)) == 1,
            length(grep("low", colnames(x), ignore.case = TRUE)) == 1,
            length(grep("close", colnames(x), ignore.case = TRUE)) == 1),
  error = function (e) {
  stop("ichimoku: clearly-defined High/Low/Close columns not found within the dataset",
       call. = FALSE)
  })

  tryPeriods <- tryCatch(
    stopifnot(is.vector(periods, mode = "numeric"), length(periods) == 3, all(periods > 0)),
    error = function(e) {
      warning("ichimoku: specified cloud periods invalid - using defaults c(9, 26, 52) instead",
              call. = FALSE)
      c(9L, 26L, 52L)
      })
  if(!is.null(tryPeriods)) periods <- tryPeriods
  p1 <- as.integer(periods[1L])
  p2 <- as.integer(periods[2L])
  p3 <- as.integer(periods[3L])
  xlen <- dim(x)[1L]
  tryCatch(stopifnot(p2 < xlen), error = function(e) {
    stop("ichimoku: medium cloud period '", p2, "' must be within the length of the dataset",
         call. = FALSE)
    })

  high <- x[ ,grep("high", colnames(x), ignore.case = TRUE)]
  low <- x[ ,grep("low", colnames(x), ignore.case = TRUE)]
  close <- x[ ,grep("close", colnames(x), ignore.case = TRUE)]
  if(length(grep("open", colnames(x), ignore.case = TRUE)) == 1) {
    open <- x[ ,grep("open", colnames(x), ignore.case = TRUE)]
  } else {
    open <- c(NA, close[1:(xlen - 1)])
    warning("ichimoku: opening prices not found - using previous closing prices as substitute",
            "\nThis affects the candles but not the calculation of the cloud", call. = FALSE)
  }

  cd <- rep(0, xlen)
  cd[open < close] <- 1
  cd[open > close] <- -1
  tenkan <- (maxOver(high, p1) + minOver(low, p1)) / 2
  kijun <- (maxOver(high, p2) + minOver(low, p2)) / 2
  senkouA <- (tenkan + kijun) / 2
  senkouB <- (maxOver(high, p3) + minOver(low, p3)) / 2
  chikou <- c(close[(p2 + 1):xlen], rep(NA, p2))
  cloudTop <- pmax.int(senkouA, senkouB)
  cloudBase <- pmin.int(senkouA, senkouB)

  periodicity <- min(diff(index[1:4]))
  extra <- seq.POSIXt(from = index[length(index)], by = periodicity, length.out = p2 + p2)[-1]
  if(attr(periodicity, "units") == "days") {
    extra <- extra[tradingDays(extra, ...)][1:p2]
  } else {
    extra <- extra[1:p2]
  }

  cloud <- xts(cbind(
    open = c(open, rep(NA, p2)),
    high = c(high, rep(NA, p2)),
    low = c(low, rep(NA, p2)),
    close = c(close, rep(NA, p2)),
    cd = c(cd, rep(NA, p2)),
    tenkan = c(tenkan, rep(NA, p2)),
    kijun = c(kijun, rep(NA, p2)),
    senkouA = c(rep(NA, p2), senkouA),
    senkouB = c(rep(NA, p2), senkouB),
    chikou = c(chikou, rep(NA, p2)),
    cloudTop = c(rep(NA, p2), cloudTop),
    cloudBase = c(rep(NA, p2), cloudBase)
  ), order.by = c(index, extra))
  attr(cloud, "periods") <- c(p1, p2, p3)
  attr(cloud, "periodicity") <- as.numeric(periodicity, units = "secs")
  attr(cloud, "ticker") <- ticker

  structure(cloud, class = c("ichimoku", "xts", "zoo"))
}

#' @rdname ichimoku
#' @method ichimoku default
#' @export
#'
ichimoku.default <- function(x = NULL, ticker, periods = c(9, 26, 52), ...) {
  tryExists <- try(exists(x), silent = TRUE)
  if(inherits(tryExists, "try-error")) {
    message("ichimoku: cannot create an ichimoku object from a '", class(x), "' object")
  } else {
    if(!tryExists) message("Error in ichimoku(", x, "): object '", x, "' not found")
    else if(missing(ticker)) ichimoku(get(x), ticker = x, periods = periods, ...)
    else ichimoku(get(x), ticker = ticker, periods = periods, ...)
  }
}

#' Print Ichimoku Objects
#'
#' Custom print method for ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot defaults to TRUE. Set to FALSE to prevent automatic plotting of
#'     the ichimoku cloud chart.
#' @param ... other arguments to be passed along.
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
  if(isTRUE(plot) && (dim(x)[2L] == 12 || dim(x)[2L] == 19)) plot.ichimoku(x, ...)
  NextMethod(print)
  invisible(x)
}

##' @importFrom ggplot2 autoplot
##' @name autoplot
##' @rdname autoplot.ichimoku
##' @export
NULL

#' autoplot.ichimoku
#'
#' Plot static Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param object an object of class 'ichimoku'.
#' @param window (optional) a date-time window to subset the plot, in ISO-8601
#'     compatible range strings of the format used for 'xts' objects, for example
#'     "2020-02-15/2020-08-15" or "2020-02-15/", "/2020-08" or "2020-07".
#' @param ticker (optional) specify a ticker (or other text) to include in the
#'     chart heading. If not set, the ticker saved within the ichimoku object
#'     will be used.
#' @param theme defaults to 'original'. This can also be set to 'dark',
#'     'solarized' or 'mono'.
#' @param gaps defaults to FALSE to remove weekend and holiday gaps. Set to TRUE
#'     for a continuous timescale axis, but with gaps for non-trading days.
#' @param strat set to TRUE by default. If the ichimoku object contains a
#'     strategy, the periods for which the strategy results in a position will
#'     be shaded. Set to FALSE to turn off this behaviour.
#' @param ... other arguments to be passed along.
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
#' autoplot(cloud, window = "2020-05-15/2020-10-30", theme = "dark")
#' autoplot(cloud, ticker = "TKR Co.", theme = "solarized", gaps = TRUE)
#'
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @export
#'
autoplot.ichimoku <- function(object, window, ticker,
                              theme = c("original", "dark", "solarized", "mono"),
                              gaps = FALSE, strat = TRUE, ...) {

  periodicity <- attr(object, "periodicity")
  if(missing(ticker)) ticker <- attr(object, "ticker")
  if(!missing(window)) object <- object[window]
  index <- index(object)
  theme <- tryCatch(match.arg(theme), error = function(e) {
    message("ichimoku: theme '", theme, "' not found - using 'original' theme instead")
    "original"
  })
  pal <- ichimoku_themes[, theme]
  candle <- as.factor(object$cd)

  ichimoku_layers_strat <- list(
    if(isTRUE(strat) && hasStrat(object)) {
      geom_rect(aes(xmin = .data$posn * (seq_along(index) - 0.5),
                    xmax = .data$posn * (seq_along(index) + 0.5),
                    ymin = -Inf, ymax = Inf, na.rm = TRUE), fill = pal[1], alpha = 0.2)
    })

  ichimoku_layers_base <- list(
    if(!all(is.na(object$cloudTop))) {
    geom_ribbon(aes(ymax = .data$cloudTop, ymin = .data$cloudBase),
                fill = pal[1L], alpha = 0.6, na.rm = TRUE)},
    geom_line(aes(y = .data$senkouA), col = pal[2L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouB), col = pal[3L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$tenkan), col = pal[4L], na.rm = TRUE),
    geom_line(aes(y = .data$kijun), col = pal[5L], na.rm = TRUE),
    geom_line(aes(y = .data$chikou), col = pal[6L], na.rm = TRUE),
    geom_linerange(aes(ymax = .data$high, ymin = .data$low, colour = candle),
                   size = 0.3, na.rm = TRUE)
  )

  ichimoku_layers_front <- list(
    scale_y_continuous(breaks = function(x) pretty(x, n = 9L)),
    scale_color_manual(values = c(`1` = pal[7L], `-1` = pal[8L], `0` = pal[9L])),
    scale_fill_manual(values = c(`1` = pal[10L], `-1` = pal[11L], `0` = pal[12L])),
    labs(x = "Date | Time", y = "Price", title = paste("Ichimoku Kinko Hyo : :", ticker)),
    theme_light(),
    theme(legend.position = "none"),
    if(theme == "dark") {
      theme(plot.title = element_text(colour = "#eee8d5"),
            plot.background = element_rect(fill = "#586e75", colour = NA),
            panel.background = element_rect(fill = "#002b36", colour = NA),
            panel.grid = element_line(colour = "#073642"),
            axis.title = element_text(colour = "#eee8d5"),
            axis.text = element_text(colour = "#eee8d5"),
            axis.ticks = element_line(colour = "#eee8d5"))
    }
  )

  if(isTRUE(gaps)) {
    w <- periodicity * 0.4
    chart <- ggplot(data = object, aes(x = index)) +
      ichimoku_layers_base +
      geom_rect(aes(xmin = index - w,
                    xmax = index + w,
                    ymin = pmin.int(.data$open, .data$close),
                    ymax = pmax.int(.data$open, .data$close),
                    colour = candle, fill = candle,
                    open = .data$open, close = .data$close,
                    date = index), size = 0.3, na.rm = TRUE) +
      scale_x_datetime(breaks = function(x) pretty(x, n = 9L),
                       labels = function(x) {
                         if(periodicity < 80000) {
                           labs <- format(x, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
                           } else {
                             labs <- format(x, paste("%d-%b", "%Y", sep = "\n"))
                           }
                         }) +
      ichimoku_layers_front
  } else {
    chart <- ggplot(data = object, aes(x = seq_along(index), date = index)) +
      ichimoku_layers_strat +
      ichimoku_layers_base +
      geom_rect(aes(xmin = seq_along(index) - 0.4,
                    xmax = seq_along(index) + 0.4,
                    ymin = pmin.int(.data$open, .data$close),
                    ymax = pmax.int(.data$open, .data$close),
                    colour = candle, fill = candle,
                    open = .data$open, close = .data$close),
                size = 0.3, na.rm = TRUE) +
      scale_x_continuous(breaks = function(x) {
        x <- pretty(seq_along(index), n = 9L) + 1
        if(x[length(x)] > dim(object)[1L]) x <- x[-length(x)]
        x
      }, labels = function(x) {
        if(periodicity < 80000) {
          labs <- format(index[x], paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
        } else {
          labs <- format(index[x], paste("%d-%b", "%Y", sep = "\n"))
        }
     }) +
      ichimoku_layers_front
  }

  chart
}

#' iplot Interactive Ichimoku Plot
#'
#' Plot interactive Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @inheritParams autoplot
#'
#' @return Returns a plotly object with classes 'plotly' and 'htmlwidget'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' \donttest{
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' iplot(cloud)
#' iplot(cloud, window = "2020-05-15/2020-10-30", theme = "dark")
#' iplot(cloud, ticker = "TKR Co.", theme = "solarized", gaps = TRUE)
#' }
#'
#' @export
#'
iplot <- function(x, window, ticker, theme = c("original", "dark", "solarized", "mono"),
                  gaps = FALSE, ...) {

  if(is.ichimoku(x)) {
    if(requireNamespace("plotly", quietly = TRUE)) {
      if(isTRUE(gaps)) {
        suppressWarnings(plotly::ggplotly(
          autoplot(x, window = window, ticker = ticker, theme = theme, gaps = gaps, ...),
          source = "select", tooltip = c("date", "x", "y", "ymax", "ymin", "open", "close")))
        } else {
          suppressWarnings(plotly::ggplotly(
            autoplot(x, window = window, ticker = ticker, theme = theme, gaps = gaps, ...),
            source = "select", tooltip = c("date", "y", "ymax", "ymin", "open", "close")))
          }
      } else {
        message("ichimoku: please install the 'plotly' package for interactive charting")
        suppressWarnings(print(autoplot(x, window = window, ticker = ticker,
                                        theme = theme, gaps = gaps, ...)
        ))
      }
  } else {
    message("ichimoku: iplot only works with ichimoku objects")
  }
}

#' Plot Ichimoku Cloud Chart
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
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
#' plot(cloud, window = "2020-05-15/2020-10-30", theme = "mono")
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x, window, ticker,
                          theme = c("original", "dark", "solarized", "mono"),
                          gaps = FALSE, strat = TRUE, ...) {

  suppressWarnings(print(
    autoplot(x, window = window, ticker = ticker, theme = theme, gaps = gaps, strat = strat, ...)
    ))
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

