# Ichimoku - Visualization Layer -----------------------------------------------

#' ichimoku
#'
#' Create an ichimoku object containing the values for all components of the
#'     Ichimoku Kinko Hyo cloud chart, ready for visualization and quantitative
#'     analysis. The object includes a date-time index, OHLC pricing data,
#'     candle direction, the cloud lines Tenkan-sen, Kijun-sen, Senkou span A,
#'     Senkou span B and Chikou span, as well as values for the cloud top and
#'     base.
#'
#' @param x a data.frame or other compatible object, which includes xts,
#'     data.table, tibble, and matrix.
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
#'     This object contains a date-time index, OHLC pricing data, candle
#'     direction, the computed ichimoku cloud values, and cloud top and base
#'     values, with ticker, periods, and periodicity parameters set as
#'     attributes.
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
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9L, 26L, 52L), ...) {
  if (missing(ticker)) ticker <- deparse(substitute(x))
  x <- matrix_df(x)
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
    index <- tryCatch(as.POSIXct(x[, coli]), error = function(e) {
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

  high <- x[, colh]
  low <- x[, coll]
  close <- x[, colc]
  colo <- grep("open", cnames, ignore.case = TRUE, perl = TRUE)[1L]
  if (!is.na(colo)) {
    open <- x[, colo]
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
  chikou <- c(close[(p2 + 1L):xlen], rep(NA, p2))
  cloudTop <- pmax.int(senkouA, senkouB)
  cloudBase <- pmin.int(senkouA, senkouB)

  periodicity <- min(diff.POSIXt(index[1:4]))
  future <- switch(attr(periodicity, "units"),
                   days = {
                     seq <- seq.POSIXt(from = index[length(index)], by = periodicity,
                                       length.out = p2 + p2)[-1L]
                     seq[tradingDays(seq, ...)][1:p2]
                     },
                   seq.POSIXt(from = index[length(index)], by = periodicity,
                              length.out = p2 + 1L)[-1L])

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
  ), order.by = c(index, future))

  structure(cloud,
            class = c("ichimoku", "xts", "zoo"),
            periods = periods,
            periodicity = as.numeric(periodicity, units = "secs"),
            ticker = ticker)

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
  if (!tryExists) stop("object '", x, "' not found")
  else if (missing(ticker)) ichimoku(get(x), ticker = x, periods = periods, ...)
  else ichimoku(get(x), ticker = ticker, periods = periods, ...)
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
#' autoplot(cloud, window = "2020-05-15/2020-10-30", theme = "dark")
#' autoplot(cloud, ticker = "TKR Co.", theme = "solarized")
#'
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @export
#'
autoplot.ichimoku <- function(object, window, ticker, message,
                              theme = c("original", "dark", "solarized", "mono"),
                              strat = TRUE, ...) {

  theme <- match.arg(theme)
  pal <- ichimoku_themes[, theme]
  periodicity <- attr(object, "periodicity")
  if (missing(ticker)) ticker <- attr(object, "ticker")
  if (missing(message)) {
    message <- if (hasStrat(object) && isTRUE(strat)) paste0("Strategy: ",
                                                             attr(object, "strat")["Strategy", ]$Strategy)
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
    if (!all(is.na(data$cloudTop))) {
      geom_ribbon(aes(ymax = .data$cloudTop, ymin = .data$cloudBase),
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
      x <- pretty.default(data$idx, n = 9L) + 1
      if (x[length(x)] > xlen) x <- x[-length(x)]
      x
    }, labels = function(x) {
      if (periodicity > 80000) format(data$index[x], paste("%d-%b", "%Y", sep = "\n"))
      else format(data$index[x], paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
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
#' plot(cloud, window = "2020-05-15/2020-10-30", theme = "mono")
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x, window, ticker, message,
                          theme = c("original", "dark", "solarized", "mono"),
                          strat = TRUE, ...) {

  print(autoplot.ichimoku(x, window = window, ticker = ticker, message = message,
                          theme = theme, strat = strat), ...)
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


#' gplot Plot Ichimoku Cloud Chart with Weekend Gaps
#'
#' Deprecated plot method for Ichimoku Kinko Hyo cloud charts with weekend gaps
#'     for non-trading days. This feature is deprecated and this function is
#'     included as a convenience only.
#'
#' @inheritParams autoplot
#'
#' @return Returns a ggplot2 object with S3 classes 'gg' and 'ggplot'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' gplot(cloud)
#'
#' @export
#'
gplot <- function(object, window, ticker, message,
                  theme = c("original", "dark", "solarized", "mono"), ...) {

  theme <- match.arg(theme)
  pal <- ichimoku_themes[, theme]
  periodicity <- attr(object, "periodicity")
  if (missing(ticker)) ticker <- attr(object, "ticker")
  if (!missing(window)) object <- object[window]

  data <- xts_df(object)
  data$cd <- as.character(data$cd)

  layers <- list(
    if (!all(is.na(data$cloudTop))) {
      geom_ribbon(aes(ymax = .data$cloudTop, ymin = .data$cloudBase),
                  fill = pal[1L], alpha = 0.6, na.rm = TRUE)
    },
    geom_line(aes(y = .data$senkouA), col = pal[2L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouB), col = pal[3L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$tenkan), col = pal[4L], na.rm = TRUE),
    geom_line(aes(y = .data$kijun), col = pal[5L], na.rm = TRUE),
    geom_segment(aes(xend = .data$index, y = .data$high, yend = .data$low, colour = .data$cd),
                 size = 0.3, na.rm = TRUE),
    geom_rect(aes(xmin = .data$index - periodicity * 0.4, xmax = .data$index + periodicity * 0.4,
                  ymin = .data$open, ymax = .data$close,
                  colour = .data$cd, fill = .data$cd),
              size = 0.3, na.rm = TRUE),
    geom_line(aes(y = .data$chikou), col = pal[6L], na.rm = TRUE),
    scale_x_datetime(breaks = function(x) pretty(x, n = 9L),
                     labels = function(x) {
                       if (periodicity > 80000) format(x, paste("%d-%b", "%Y", sep = "\n"))
                       else format(x, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
                     }),
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)),
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])),
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])),
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker),
         subtitle = if(!missing(message)) message),
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
  ggplot(data = data, aes(x = .data$index)) + layers
}

#' iplot Interactive Ichimoku Cloud Plot
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects in a Shiny app,
#'     allowing full customisation of chart elements in an interactive environment.
#'     Intuitive cursor infotip allows ready access to the data directly from the
#'     chart.
#'
#' @param x an object of class 'ichimoku'.
#' @inheritParams autoplot
#' @param ... additional parameters passed along to the 'options' argument of
#'     \code{shiny::shinyApp()}.
#' @param launch.browser [default TRUE] If TRUE, the system's default web
#'     browser will be launched automatically after the app is started. The value
#'     of this argument can also be a function to call with the application's URL.
#'     To use the default Shiny viewer in RStudio, please specify
#'     \code{getOption("shiny.launch.browser")}.
#'
#' @return Returns a Shiny app object with class 'shiny.appobj'.
#'
#' @details This function has a dependency on the 'shiny' package.
#'
#' @examples
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' iplot(cloud)
#'
#' # To open in RStudio viewer instead of default browser
#' iplot(cloud, launch.browser = getOption("shiny.launch.browser"))
#' }
#'
#' @export
#'
iplot <- function(x, ticker, theme = c("original", "dark", "solarized", "mono"),
                  message, strat = TRUE, ..., launch.browser = TRUE) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    if (!is.ichimoku(x)) stop("iplot() only works with ichimoku objects", call. = FALSE)
    theme <- match.arg(theme)
    if (missing(ticker)) ticker <- attr(x, "ticker")
    if (missing(message)) {
      message <- if (hasStrat(x) && isTRUE(strat)) paste0("Strategy: ",
                                                          attr(x, "strat")["Strategy", ]$Strategy)
    }
    tformat <- if (attr(x, "periodicity") > 80000) "%F" else "%F %T"
    start <- index(x)[1L]
    end <- index(x)[dim(x)[1L]]
    xadj <- if (nchar(as.character(start)) > 10) -17 else 5

    ui <- shiny::fluidPage(
      shiny::fillPage(
        padding = 20,
        shiny::tags$style(type = "text/css", "#chart {height: calc(100vh - 190px) !important;}"),
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover",
                                                   delay = 80, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("infotip")
        ),
      shiny::fluidRow(
        shiny::column(width = 10, offset = 1,
                      shiny::sliderInput("dates", label = NULL,
                                         min = start, max = end,
                                         value = c(start, end),
                                         width = "100%", timeFormat = tformat))
        ),
      shiny::fluidRow(
        shiny::column(width = 2, offset = 1,
                      shiny::selectInput("theme", label = "Theme",
                                         choices = c("original", "dark", "solarized", "mono"),
                                         selected = theme,
                                         selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::textInput("ticker", label = "Ticker",
                                       value = ticker, width = "100%")),
        shiny::column(width = 2,
                      shiny::textInput("message", label = "Message",
                                       value = message, width = "100%")),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>Show</label>"),
                      shiny::checkboxInput("infotip", "Infotip", value = TRUE)),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>&nbsp;</label>"),
                      if (hasStrat(x)) shiny::checkboxInput("strat", "Strategy",
                                                            value = isTRUE(strat)))
        )
    )

    server <- function(input, output, session) {
      window <- shiny::reactive(paste0(input$dates[1L], "/", input$dates[2L]))
      left_px <- shiny::reactive(input$plot_hover$coords_css$x)
      top_px <- shiny::reactive(input$plot_hover$coords_css$y)
      posi_x <- shiny::reactive(round(input$plot_hover$x, digits = 0))

      pdata <- shiny::reactive(x[window()])

      output$chart <- shiny::renderPlot(
        autoplot.ichimoku(pdata(), ticker = input$ticker, message = input$message,
                          theme = input$theme, strat = input$strat)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawGuide(label = index(pdata())[posi_x()], left = left_px() + xadj, top = 60)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5L), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawInfotip(sdata = pdata()[posi_x(), ], left_px = left_px(), top_px = top_px())
      })

      session$onSessionEnded(function() shiny::stopApp())
    }

    shiny::shinyApp(ui, server, options = list(launch.browser = launch.browser, ...))

  } else {
    message("Note: please install the 'shiny' package to enable interactive charting",
            "\nAlternatively use plot() for static charts")
  }
}

#' drawInfotip
#'
#' Internal function used by ichimoku to draw the infotip for interactive Shiny
#'     plots.
#'
#' @param sdata the selected data frame row.
#' @param left_px the horizontal cursor position in pixels.
#' @param top_px the vertical cursor position in pixels.
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @keywords internal
#'
drawInfotip <- function(sdata, left_px, top_px) {
  shiny::wellPanel(
    style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                   "left:", left_px + 50, "px; top:", top_px + 40, "px; ",
                   "font-size: 0.8em; padding: 1px 5px 5px 5px;"),
    shiny::HTML(paste0("<div style='margin:0; padding:0; font-weight:bold'>",
                       if (isTRUE(sdata$cd == 1)) "&uarr;<br />"
                       else if (isTRUE(sdata$cd == -1)) "&darr;<br />"
                       else "&rarr;<br />",
                       index(sdata),
                       "</div><div style='text-align:center; margin:2px 0 0 0; padding:0'>H: ",
                       signif(sdata$high, digits = 5L),
                       "</div><div style='margin:0; padding:0'>O: ",
                       signif(sdata$open, digits = 5L),
                       "&nbsp;&nbsp;C: ", signif(sdata$close, digits = 5L),
                       "</div><div style='text-align:center; margin:0; padding:0'>L: ",
                       signif(sdata$low, digits = 5L),
                       "</div><div style='margin:2px 0 0 0; padding:0'>Tenkan: ",
                       signif(sdata$tenkan, digits = 5L),
                       "<br />Kijun: ", signif(sdata$kijun, digits = 5L),
                       "<br />Senkou A: ", signif(sdata$senkouA, digits = 5L),
                       "<br />Senkou B: ", signif(sdata$senkouB, digits = 5L),
                       "<br />Chikou: ", signif(sdata$chikou, digits = 5L), "</div>"))
  )
}

#' drawGuide
#'
#' Internal function used by ichimoku to draw the axis guides for interactive
#'     Shiny plots.
#'
#' @param label a function returning the character string to be shown.
#' @param left the horizontal position of the guide in pixels.
#' @param top the vertical position of the guide in pixels.
#'
#' @return An object of class 'shiny.tag' comprising the HTML to be rendered.
#'
#' @keywords internal
#'
drawGuide <- function(label, left, top) {
  shiny::wellPanel(
    style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); left:",
                   left, "px; top:", top, "px; font-size: 0.8em; padding:0;"),
    shiny::HTML(paste(label))
  )
}

