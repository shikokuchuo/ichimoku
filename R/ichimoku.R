# Ichimoku ---------------------------------------------------------------------

#' ichimoku
#'
#' Create an ichimoku object containing the computed values of all components of
#'     the Ichimoku Kinko Hyo cloud chart, ready for plotting. The object
#'     includes the candlesticks as well as the cloud lines themselves:
#'     Tenkan-sen, Kijun-sen, Senkou span A, Senkou span B and Chikou span.
#'
#' @param x a data.frame or other compatible object, which includes xts,
#'     data.table, tibble, and matrix.
#' @param ticker (optional) specify a ticker to identify the instrument,
#'     otherwise this will be set to the name of the input object 'x'.
#' @param periods a vector defining the length of periods used for the cloud,
#'     with a default of c(9, 26, 52). This parameter shoud not normally be
#'     changed as using other values would be invalid in the context of
#'     traditional Ichimoku analysis.
#' @param ... other arguments to be passed along.
#'
#' @return An ichimoku object is returned with S3 classes of 'ichimoku' and
#'     'data.frame'.
#'
#'     This object contains a date-time index, OHLC pricing data, candlestick
#'     direction, as well as the computed ichimoku cloud values, with ticker and
#'     periodicity parameters set as attributes.
#'
#' @details Calling an ichimoku object automatically invokes its print method,
#'     which in the absence of arguments, will produce a printout of the data to
#'     the console as well as a static plot of the cloud chart to the graphical
#'     device.
#'
#'     For further options, including interactive charting, use plot() on the
#'     returned ichimoku object to pass further arguments for customising
#'     the chart.
#'
#'     Where an ichimoku object is passed to ichimoku(), the original object
#'     will be returned (with the ticker amended if the corresponding parameter
#'     is set).
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' \dontrun{
#' ichimoku(x)
#' ichimoku(x, ticker = "TKR", periods = c(9, 26, 52))
#' }
#'
#' @rdname ichimoku
#' @export
#'
ichimoku <- function(x, ...) UseMethod("ichimoku")

#' @rdname ichimoku
#' @method ichimoku ichimoku
#' @export
#'
ichimoku.ichimoku <- function(x, ticker, ...) {
  if(!missing(ticker)) attr(x, "ticker") <- ticker
  print(x, ...)
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

  tryPeriods <- tryCatch(stopifnot(is.vector(periods, mode = "numeric"), length(periods) == 3),
           error = function(e) {
             warning("ichimoku: specified periods are invalid - ",
             "using defaults c(9, 26, 52) instead", call. = FALSE)
             c(9, 26, 52)
           })
  if(!is.null(tryPeriods)) periods <- tryPeriods
  tryCatch(stopifnot(periods[2] < nrow(x)), error = function(e) {
    stop("ichimoku cannot construct a meaningful cloud as the medium period '",
    periods[2], "' is not within the length of your dataset", call. = FALSE)})

  if(!is.null(rownames(x)) &
     !inherits(try(as.POSIXct(rownames(x)), silent = TRUE), "try-error")) {
    date <- as.POSIXct(rownames(x))
  } else if (length(grep("Index|Date|Time", colnames(x), ignore.case = TRUE)) == 1) {
    tryCatch(date <- as.POSIXct(x[[grep("Index|Date|Time",
                                         colnames(x), ignore.case = TRUE)]]),
      error = function(e) {
        stop("ichimoku is unable to convert the dataset index into a ",
             "POSIXct date-time format", call. = FALSE)
      })
  } else {
    stop("ichimoku cannot find a unique date-time index within the dataset.", call. = FALSE)
  }

  tryCatch(
  stopifnot(length(grep("High", colnames(x), ignore.case = TRUE)) == 1,
            length(grep("Low", colnames(x), ignore.case = TRUE)) == 1,
            length(grep("Close", colnames(x), ignore.case = TRUE)) == 1),
  error = function (e) {
  stop("ichimoku cannot find clearly-defined High/Low/Close columns in the dataset", call. = FALSE)
  })

  high <- x[[grep("High", colnames(x), ignore.case = TRUE)]]
  low <- x[[grep("Low", colnames(x), ignore.case = TRUE)]]
  close <- x[[grep("Close", colnames(x), ignore.case = TRUE)]]
  if(length(grep("Open", colnames(x), ignore.case = TRUE)) == 1) {
    open <- x[[grep("Open", colnames(x), ignore.case = TRUE)]]
  } else {
    open <- c(NA, close[1:(nrow(x) - 1)])
    warning("Note: ichimoku did not find opening price data, using previous closing prices ",
            "as substitute - this affects the candles but not the calculation of the cloud")
  }

  tenkan <- (roll_maxr(high, n = periods[1]) + roll_minr(low, n = periods[1])) / 2
  kijun <- (roll_maxr(high, n = periods[2]) + roll_minr(low, n = periods[2])) / 2
  senkouA <- (tenkan + kijun) / 2
  senkouB <- (roll_maxr(high, n = periods[3]) + roll_minr(low, n = periods[3])) / 2
  chikou <- c(close[(periods[2] + 1):nrow(x)], rep(NA, periods[2]))

  periodicity <- min(difftime(date[2], date[1]), difftime(date[3], date[2]),
                     difftime(date[4], date[3]))
  extra <- seq.POSIXt(from = date[nrow(x)], by = periodicity,
                      length.out = periods[2] * 2)[-1]
  if(attr(periodicity, "units") == "days") {
    extra <- extra[isBizday(as.timeDate(extra))][1:periods[2]]
  } else {
    extra <- extra[1:periods[2]]
  }

  cloud <- data.frame(
    date = c(date, extra),
    open = c(open, rep(NA, periods[2])),
    high = c(high, rep(NA, periods[2])),
    low = c(low, rep(NA, periods[2])),
    close = c(close, rep(NA, periods[2])),
    candle = factor(c(ifelse(close > open, "up", ifelse(close < open, "down", "flat")),
               rep(NA, periods[2])), levels = c("down", "flat", "up")),
    tenkan = c(tenkan, rep(NA, periods[2])),
    kijun = c(kijun, rep(NA, periods[2])),
    senkouA = c(rep(NA, periods[2]), senkouA),
    senkouB = c(rep(NA, periods[2]), senkouB),
    chikou = c(chikou, rep(NA, periods[2]))
  )
  attr(cloud, "periodicity") <- as.numeric(periodicity, units = "secs")
  attr(cloud, "ticker") <- ticker

  structure(cloud, class = c("ichimoku", "data.frame"))
}

#' @rdname ichimoku
#' @method ichimoku default
#' @export
#'
ichimoku.default <- function(x = NULL, ticker, ...) {
  tryExists <- try(exists(x), silent = TRUE)
  if(inherits(tryExists, "try-error")) {
    message("ichimoku: cannot create an ichimoku object from a '", class(x), "' object")
  } else {
    if(!tryExists) message("Error in ichimoku(", x, "): object '", x, "' not found")
    else if(missing(ticker)) ichimoku(get(x), ticker = x)
    else ichimoku(get(x), ticker = ticker)
  }
}

#' print.ichimoku
#'
#' Custom print method for ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot set to TRUE by default. Set to FALSE to prevent automatic
#'     plotting of the ichimoku cloud chart.
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
#' \dontrun{
#' # where 'cloud' is an ichimoku object created by the ichimoku() function:
#'
#' print(cloud)
#' print(cloud, plot = FALSE)
#' }
#'
#' @method print ichimoku
#' @export
#'
print.ichimoku <- function(x, plot = TRUE, ...) {
  NextMethod(print)
  if(plot == TRUE) plot.ichimoku(x, ...)
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
#' @param from (optional) a starting date/time to subset the plot.
#' @param to (optional) an ending date/time to subset the plot.
#' @param ticker (optional) specify a ticker (or other text) to include in the
#'     chart heading. If not set, the ticker saved within the ichimoku object
#'     will be used.
#' @param theme with a default of 'default'. This can also be set to 'dark' or
#'     'solarized' to select the desired colour scheme.
#' @param gaps set to FALSE by default to remove weekend and holiday gaps. Set
#'     to TRUE for a continuous timescale axis, but with gaps for non-trading
#'     days.
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
#' \dontrun{
#' # where 'cloud' is an ichimoku object created by the ichimoku() function:
#'
#' autoplot(cloud)
#' autoplot(cloud, from = "2020-01-15", to = "2021-02-15", theme = "dark")
#' autoplot(cloud, ticker = "TKR", theme = "solarized", gaps = TRUE)
#' }
#'
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @export
#'
autoplot.ichimoku <- function(object, from, to, ticker, theme = "default", gaps = FALSE, ...) {

  periodicity <- attr(object, "periodicity")
  if(missing(ticker)) ticker <- attr(object, "ticker")
  if(!missing(from)) object <- object[object$date >= from,]
  if(!missing(to)) object <- object[object$date <= to,]
  if(theme %in% colnames(ichimoku_themes)) {
    pal <- ichimoku_themes[[grep(theme, colnames(ichimoku_themes))]]
  } else {
    pal <- ichimoku_themes[["default"]]
    message("ichimoku: theme '", theme, "' was not found, using 'default' theme instead",
            "\n currently available themes are 'default', 'dark' and 'solarized'")
  }

  ichimoku_layers_base <- list(
    geom_ribbon(aes(ymax = .data$senkouA, ymin = .data$senkouB), fill = pal[1],
                alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouA), col = pal[2], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouB), col = pal[3], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$tenkan), col = pal[4], na.rm = TRUE),
    geom_line(aes(y = .data$kijun), col = pal[5], na.rm = TRUE),
    geom_line(aes(y = .data$chikou), col = pal[6], na.rm = TRUE),
    geom_linerange(aes(ymax = .data$high, ymin = .data$low, colour = .data$candle),
                   size = 0.3, na.rm = TRUE)
  )
  ichimoku_layers_front <- list(
    scale_y_continuous(breaks = function(x) pretty(x, n = 9)),
    scale_color_manual(values = c("up" = pal[7], "down" = pal[8], "flat" = pal[9])),
    scale_fill_manual(values = c("up" = pal[10], "down" = pal[11], "flat" = pal[12])),
    labs(x = "Date | Time", y = "Price", title = paste("Ichimoku Kinko Hyo : :", ticker)),
    theme_light(),
    theme(legend.position = "none")
  )
  ichimoku_dark_theme <- list(
    theme(
      plot.title = element_text(colour = "#eee8d5"),
      plot.background = element_rect(fill = "#586e75", colour = NA),
      panel.background = element_rect(fill = "#002b36", colour = NA),
      panel.grid = element_line(colour = "#073642"),
      axis.title = element_text(colour = "#eee8d5"),
      axis.text = element_text(colour = "#eee8d5"),
      axis.ticks = element_line(colour = "#eee8d5", size = rel(0.5))
    )
  )

  if(gaps == TRUE) {
    w <- periodicity / 2 * 0.8
    chart <- ggplot(data = object, aes(x = .data$date)) +
      ichimoku_layers_base +
      geom_rect(aes(xmin = .data$date - w,
                    xmax = .data$date + w,
                    ymin = pmin(.data$open, .data$close),
                    ymax = pmax(.data$open, .data$close),
                    colour = .data$candle, fill = .data$candle,
                    open = .data$open, close = .data$close,
                    date = .data$date, na.rm = TRUE), size = 0.3) +
      scale_x_datetime(breaks = function(x) pretty(x, n = 9),
                       labels = function(x) {
                         if(periodicity < 80000) {
                           labs <- format(x, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
                           } else {
                             labs <- format(x, paste("%d-%b", "%Y", sep = "\n"))
                           }
                         }) +
      ichimoku_layers_front

  } else {
    w <- 1 / 2 * 0.8
    chart <- ggplot(data = object, aes(x = seq_along(.data$date), date = .data$date)) +
      ichimoku_layers_base +
      geom_rect(aes(xmin = seq_along(.data$date) - w,
                    xmax = seq_along(.data$date) + w,
                    ymin = pmin(.data$open, .data$close),
                    ymax = pmax(.data$open, .data$close),
                    colour = .data$candle, fill = .data$candle,
                    open = .data$open, close = .data$close),
                size = 0.3, na.rm = TRUE) +
      scale_x_continuous(breaks = function(x) {
        x <- pretty(seq_along(object$date), n = 9) + 1
        if(x[length(x)] > nrow(object)) x <- x[-length(x)]
        x
      }, labels = function(x) {
        if(periodicity < 80000) {
          labs <- format(object$date[x], paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
        } else {
          labs <- format(object$date[x], paste("%d-%b", "%Y", sep = "\n"))
        }
     }) +
      ichimoku_layers_front
  }

  if(theme == "dark") chart <- chart + ichimoku_dark_theme

  chart
}

#' iplot interactive ichimoku plot
#'
#' Plot interactive Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param from (optional) a starting date/time to subset the plot.
#' @param to (optional) an ending date/time to subset the plot.
#' @param ticker (optional) specify a ticker (or other text) to include in the
#'     chart heading. If not set, the ticker saved within the ichimoku object
#'     will be used.
#' @param theme with a default of 'default'. This can also be set to 'dark' or
#'     'solarized' to select the desired colour scheme.
#' @param gaps set to FALSE by default to remove weekend and holiday gaps. Set
#'     to TRUE for a continuous timescale axis, but with gaps for non-trading
#'     days.
#' @param ... other arguments to be passed along.
#'
#' @return Returns a plotly object with classes 'plotly' and 'htmlwidget'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' \dontrun{
#' # where 'cloud' is an ichimoku object created by the ichimoku() function:
#'
#' iplot(cloud)
#' iplot(cloud, from = "2020-01-15", to = "2021-02-15", theme = "dark")
#' iplot(cloud, ticker = "TKR", theme = "solarized", gaps = TRUE)
#' }
#'
#' @export
#'
iplot <- function(x, from, to, ticker, theme = "default", gaps = FALSE, ...) {

  if(is.ichimoku(x)) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      if (gaps == TRUE) {
        suppressWarnings(plotly::ggplotly(
          autoplot(x, from = from, to = to, ticker = ticker, theme = theme, gaps = gaps, ...),
          source = "select", tooltip = c("date", "x", "y", "ymax", "ymin", "open", "close")))
        } else {
          suppressWarnings(plotly::ggplotly(
            autoplot(x, from = from, to = to, ticker = ticker, theme = theme, gaps = gaps, ...),
            source = "select", tooltip = c("date", "y", "ymax", "ymin", "open", "close")))
          }
      } else {
        message("The 'plotly' package needs to be installed for interactive plotting")
        suppressWarnings(print(autoplot(x, from = from, to = to, ticker = ticker, theme = theme, gaps = gaps, ...)
        ))
      }
  } else {
    message("ichimoku: iplot only works with ichimoku objects - '",
            deparse(substitute(x)),"' is an object of class ", class(x))
  }
}

#' plot.ichimoku
#'
#' Plot static or interactive Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param i interactive parametmer set to FALSE by default. Set to TRUE to plot
#'     an interactive rather than a static chart.
#' @param from (optional) a starting date/time to subset the plot.
#' @param to (optional) an ending date/time to subset the plot.
#' @param ticker (optional) specify a ticker, or other text, to include
#'     in the chart heading. If not set, the ticker saved within the ichimoku
#'     object will be used.
#' @param theme with a default of 'default'. This can also be set to 'dark' or
#'     'solarized' to select the desired colour scheme.
#' @param gaps set to FALSE by default to remove weekend and holiday gaps. Set
#'     to TRUE for a continuous timescale axis, but with gaps for non-trading
#'     days.
#' @param ... other arguments to be passed along.
#'
#' @return Returns either a ggplot2 object with classes 'gg' and 'ggplot', or a
#'     plotly object with classes 'plotly' and 'htmlwidget' depending on the
#'     parameters set.
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
#' \dontrun{
#' # where 'cloud' is an ichimoku object created by the ichimoku() function:
#'
#' plot(cloud)
#' plot(cloud, from = "2020-01-15", to = "2021-02-15", theme = "dark")
#' plot(cloud, i = TRUE, ticker = "TKR", theme = "solarized", gaps = TRUE)
#'
#' # quickest way to get to an interactive chart:
#' plot(cloud, T)
#' }
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x, i = FALSE, from, to, ticker, theme = "default", gaps = FALSE, ...) {

  if(i == TRUE) {
    iplot(x, from = from, to = to, ticker = ticker, theme = theme, gaps = gaps, ...)
  } else {
    suppressWarnings(print(
      autoplot(x, from = from, to = to, ticker = ticker, theme = theme, gaps = gaps, ...)
    ))
  }
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
#' \dontrun{
#' # where 'cloud' is an ichimoku object created by the ichimoku() function:
#'
#' is.ichimoku(cloud)
#' # [1] TRUE
#' }
#'
#' @export
#'
is.ichimoku <- function(x) inherits(x, "ichimoku")

