# Ichimoku - Visualization Layer -----------------------------------------------

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
#' @param periods [default c(9, 26, 52)] a vector defining the length of periods
#'     used for the cloud. This parameter shoud not normally be changed as using
#'     other values would be invalid in the context of traditional Ichimoku
#'     analysis.
#' @param ... additional arguments to be passed along. For instance, 'holidays'
#'     may be passed to the \code{\link{tradingDays}} function, used by ichimoku
#'     when calculating the future cloud.
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
  x <- xts_df(x)
  x <- x[!is.na(x$close), ]
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku xts
#' @export
#'
ichimoku.xts <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(ticker)) ticker <- deparse(substitute(x))
  x <- xts_df(x)
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku matrix
#' @export
#'
ichimoku.matrix <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(ticker)) ticker <- deparse(substitute(x))
  x <- structure(apply(x, 2, identity, simplify = FALSE),
                 class = "data.frame",
                 row.names = row.names(x))
  ichimoku.data.frame(x, ticker = ticker, periods = periods, ...)
}

#' @rdname ichimoku
#' @method ichimoku data.frame
#' @export
#'
ichimoku.data.frame <- function(x, ticker, periods = c(9, 26, 52), ...) {

  if(missing(ticker)) ticker <- deparse(substitute(x))

  coli <- grep("index|date|time", colnames(x), ignore.case = TRUE, perl = TRUE)[1L]
  if(!is.na(coli)) {
    index <- tryCatch(as.POSIXct(x[, coli]), error = function(e) {
      stop("Index/date/time column not convertible to a POSIXct date-time format",
           call. = FALSE)
      })
  } else if(!is.null(rownames(x))) {
    index <- tryCatch(as.POSIXct(rownames(x)), error = function(e) {
      stop("Valid date-time index not found within the dataset",
           call. = FALSE)
    })
  } else {
    stop("Date-time index not found within the dataset", call. = FALSE)
  }
  colh <- grep("high", colnames(x), ignore.case = TRUE, perl = TRUE)[1L]
  coll <- grep("low", colnames(x), ignore.case = TRUE, perl = TRUE)[1L]
  colc <- grep("close", colnames(x), ignore.case = TRUE, perl = TRUE)[1L]
  if(anyNA(c(colh, coll, colc))) {
    stop("Clearly-defined high/low/close columns not found within the dataset", call. = FALSE)
  }
  if(!is.vector(periods, mode = "numeric") || !length(periods) == 3 || !all(periods > 0)) {
    warning("Specified cloud periods invalid - using defaults c(9, 26, 52) instead",
            call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p1 <- as.integer(periods[1L])
  p2 <- as.integer(periods[2L])
  p3 <- as.integer(periods[3L])
  xlen <- dim(x)[1L]
  if(p2 >= xlen) stop("Dataset must be longer than the medium cloud period '",
                     p2, "'", call. = FALSE)

  high <- x[, colh]
  low <- x[, coll]
  close <- x[, colc]
  colo <- grep("open", colnames(x), ignore.case = TRUE, perl = TRUE)[1L]
  if(!is.na(colo)) {
    open <- x[, colo]
  } else {
    warning("Opening prices not found - using previous closing prices as substitute",
            "\nThis affects the candles but not the calculation of the cloud", call. = FALSE)
    open <- c(NA, close[1:(xlen - 1)])
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

  periodicity <- min(diff.POSIXt(index[1:4]))
  switch(attr(periodicity, "units"),
         days = {
           extra <- seq.POSIXt(from = index[length(index)], by = periodicity, length.out = p2 + p2)[-1]
           extra <- extra[tradingDays(extra, ...)][1:p2]
           },
         extra <- seq.POSIXt(from = index[length(index)], by = periodicity, length.out = p2 + 1)[-1])

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

  structure(cloud,
            class = c("ichimoku", "xts", "zoo"),
            periods = c(p1, p2, p3),
            periodicity = as.numeric(periodicity, units = "secs"),
            ticker = ticker)
}

#' @rdname ichimoku
#' @method ichimoku default
#' @export
#'
ichimoku.default <- function(x, ticker, periods = c(9, 26, 52), ...) {
  if(missing(x)) stop("Argument 'x' must be specified", call. = FALSE)
  tryExists <- tryCatch(exists(x), error = function(e) {
    stop("Cannot create an ichimoku object from a '", class(x)[1L], "' object", call. = FALSE)
  })
  if(!tryExists) message("Error in ichimoku(", x, "): object '", x, "' not found")
  else if(missing(ticker)) ichimoku(get(x), ticker = x, periods = periods, ...)
  else ichimoku(get(x), ticker = ticker, periods = periods, ...)
}

#' Print Ichimoku Objects
#'
#' Custom print method for ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot [default TRUE] set to FALSE to prevent automatic plotting of
#'     the ichimoku cloud chart.
#' @param ... additional arguments to be passed along.
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
  if((dim(x)[2L] == 12 || dim(x)[2L] == 19) && isTRUE(plot)) plot.ichimoku(x, ...)
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
#'     periods for which the strategy results in a position will be shaded. Set
#'     to FALSE to turn off this behaviour.
#' @param ... additional arguments to be passed along.
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
  if(missing(ticker)) ticker <- attr(object, "ticker")
  if(!missing(window)) object <- object[window]
  xlen <- dim(object)[1L]
  data <- xts_df(object)
  data$idx <- seq_len(xlen)
  data$cd <- as.character(data$cd)

  conditional_layers <- list(
    if(hasStrat(object) && isTRUE(strat)) {
      geom_rect(aes(xmin = .data$posn * (.data$idx - 0.5),
                    xmax = .data$posn * (.data$idx + 0.5),
                    ymin = -Inf, ymax = Inf), fill = pal[1L], alpha = 0.2, na.rm = TRUE)
      },
    if(!all(is.na(data$cloudTop))) {
      geom_ribbon(aes(ymax = .data$cloudTop, ymin = .data$cloudBase),
                  fill = pal[1L], alpha = 0.6, na.rm = TRUE)
    }
    )

  chart <- ggplot(data = data, aes(x = .data$idx, date = .data$index, close = .data$close)) +
    conditional_layers +
    geom_line(aes(y = .data$senkouA), col = pal[2L], alpha = 0.6, na.rm = TRUE) +
    geom_line(aes(y = .data$senkouB), col = pal[3L], alpha = 0.6, na.rm = TRUE) +
    geom_line(aes(y = .data$tenkan), col = pal[4L], na.rm = TRUE) +
    geom_line(aes(y = .data$kijun), col = pal[5L], na.rm = TRUE) +
    geom_segment(aes(xend = .data$idx, y = .data$high, yend = .data$low, colour = .data$cd),
                 size = 0.3, na.rm = TRUE) +
    geom_rect(aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4,
                  ymin = .data$open, ymax = .data$close,
                  colour = .data$cd, fill = .data$cd),
              size = 0.3, na.rm = TRUE) +
    geom_line(aes(y = .data$chikou), col = pal[6L], na.rm = TRUE) +
    scale_x_continuous(breaks = function(x) {
      x <- pretty.default(data$idx, n = 9L) + 1
      if(x[length(x)] > xlen) x <- x[-length(x)]
      x
      }, labels = function(x) {
        if(periodicity < 80000) {
          labs <- format(data$index[x], paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
          } else {
            labs <- format(data$index[x], paste("%d-%b", "%Y", sep = "\n"))
          }
        }
      ) +
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)) +
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])) +
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])) +
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker),
         subtitle = if(!missing(message)) message else NULL) +
    theme_light() +
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

  chart
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
plot.ichimoku <- function(x, window, ticker, message,
                          theme = c("original", "dark", "solarized", "mono"),
                          strat = TRUE, ...) {

  print(autoplot.ichimoku(x, window = window, ticker = ticker, message = message,
                          theme = theme, strat = strat, ...))
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

#' iplot Interactive Ichimoku Cloud Plot
#'
#' Plot interactive Ichimoku Kinko Hyo cloud charts from ichimoku objects. iplot()
#'     can be used in conjunction with R Markdown to create portable, self-contained
#'     interactive HTML charts.
#'
#' @param x an object of class 'ichimoku'.
#' @inheritParams autoplot
#'
#' @return Returns a plotly object with classes 'plotly' and 'htmlwidget'.
#'
#' @details This function has a dependency on the 'plotly' package.
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
#' iplot(cloud, window = "2020-05-15/2020-10-30", ticker = "TKR Co.", theme = "dark")
#' }
#'
#' @export
#'
iplot <- function(x, window, ticker, message,
                  theme = c("original", "dark", "solarized", "mono"), ...) {

  if(!is.ichimoku(x)) stop("iplot() only works with ichimoku objects", call. = FALSE)
  if(requireNamespace("plotly", quietly = TRUE)) {
      plotly::ggplotly(
        autoplot.ichimoku(x, window = window, ticker = ticker, message = message,
                          theme = theme, ...),
        tooltip = c("date", "y", "yend", "close"))
  } else {
    message("Note: please install the 'plotly' package to enable interactive charting",
            "\nAlternatively use plot() for static charts")
  }
}

#' rplot Reactive Ichimoku Cloud Plot
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects in a Shiny app,
#'     allowing full customisation of chart elements in an interactive environment.
#'     Intuitive cursor tooltip allows ready access to the data directly from the
#'     chart.
#'
#' @param x an object of class 'ichimoku'.
#' @inheritParams autoplot
#'
#' @return Returns a Shiny app object with class 'shiny.appobj'.
#'
#' @details This function has a dependency on the 'shiny' package.
#'
#' @examples
#' if(interactive()) {
#' # Only run examples in interactive R sessions
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' rplot(cloud)
#' }
#'
#' @export
#'
rplot <- function(x, ticker, theme = c("original", "dark", "solarized", "mono"),
                  message, strat = TRUE, ...) {

  if(!is.ichimoku(x)) stop("rplot() only works with ichimoku objects", call. = FALSE)

  if(requireNamespace("shiny", quietly = TRUE)) {
    theme <- match.arg(theme)
    if(missing(ticker)) ticker <- attr(x, "ticker")
    if(missing(message)) message <- NULL
    tformat <- if(attr(x, "periodicity") > 80000) "%F" else "%F %T"
    start <- index(x)[1]
    end <- index(x)[dim(x)[1L]]
    xadj <- if(nchar(as.character(start)) > 10) -17 else 5

    ui <- shiny::fluidPage(
      shiny::fillPage(
        padding = 20,
        shiny::tags$style(type = "text/css", "#chart {height: calc(100vh - 190px) !important;}"),
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover",
                                                   delay = 80, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("tooltip")
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
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>Show</label>"),
                      shiny::checkboxInput("tooltip", "Tooltip", value = TRUE)),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>&nbsp;</label>"),
                      if(hasStrat(x)) shiny::checkboxInput("strat", "Strategy", value = isTRUE(strat)))
        )
    )

    server <- function(input, output) {
      window <- shiny::reactive(paste0(input$dates[1L], "/", input$dates[2L]))
      pdata <- shiny::reactive(x[window()])
      left_px <- shiny::reactive(input$plot_hover$coords_css$x)
      top_px <- shiny::reactive(input$plot_hover$coords_css$y)
      posi_x <- shiny::reactive(round(input$plot_hover$x, digits = 0))

      output$chart <- shiny::renderPlot(
        autoplot.ichimoku(x, window = window(), ticker = input$ticker, message = message,
                          theme = input$theme, strat = if(hasStrat(x)) input$strat)
      )

      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        shiny::wellPanel(
          style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                         "left:", left_px() + xadj, "px; top:45px; ",
                         "font-size: 0.8em; padding:0;"),
          shiny::HTML(
            paste(index(pdata())[posi_x()])
          ))
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        shiny::wellPanel(
          style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                         "left:75px; top:", top_px() + 11, "px; ",
                         "font-size: 0.8em; padding:0;"),
          shiny::HTML(
            paste(signif(input$plot_hover$y, digits = 5))
          ))
      })

      output$tooltip <- shiny::renderUI({
        shiny::req(input$tooltip, input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        sdata <- pdata()[posi_x(), ]
        shiny::wellPanel(
          style = paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                         "left:", left_px() + 43, "px; top:", top_px() + 30, "px; ",
                         "font-size: 0.8em; padding: 1px 5px 5px 5px;"),
          shiny::HTML(
            paste("<div style = 'margin:0; padding:0; font-weight:bold;'>",
                  if(isTRUE(sdata$cd == 1)) "&uarr;<br />"
                  else if(isTRUE(sdata$cd == -1)) "&darr;<br />" else "&rarr;<br />",
                  index(sdata),"</div><div style = 'text-align:center; margin:2px 0 0 0; padding:0;'>H:",
                  signif(sdata$high, digits = 5),
                  "</div><div style = 'margin:0; padding:0;'>O:",
                  signif(sdata$open, digits = 5),
                  "&nbsp;&nbsp;C:", signif(sdata$close, digits = 5),
                  "</div><div style = 'text-align:center; margin:0; padding:0;'>L:",
                  signif(sdata$low, digits = 5),
                  "</div><div style = 'margin:2px 0 0 0; padding:0;'>Tenkan:",
                  signif(sdata$tenkan, digits = 5),
                  "<br />Kijun:", signif(sdata$kijun, digits = 5),
                  "<br />Senkou A:", signif(sdata$senkouA, digits = 5),
                  "<br />Senkou B:", signif(sdata$senkouB, digits = 5),
                  "<br />Chikou:", signif(sdata$chikou, digits = 5), "</div>")
          ))
      })
    }

    shiny::shinyApp(ui, server)

  } else {
    message("Note: please install the 'shiny' package to enable reactive charting",
            "\nAlternatively use plot() for static charts")
  }
}

#' gplot Plot Ichimoku Cloud Chart with Weekend Gaps
#'
#' Deprecated plot method for Ichimoku Kinko Hyo cloud charts with weekend gaps
#'     for non-trading days. This feature is deprecated and this function is
#'     included as a convenience only.
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
#'     periods for which the strategy results in a position will be shaded. Set
#'     to FALSE to turn off this behaviour.
#' @param ... additional arguments to be passed along.
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
                              theme = c("original", "dark", "solarized", "mono"),
                              strat = TRUE, ...) {

  theme <- match.arg(theme)
  pal <- ichimoku_themes[, theme]
  periodicity <- attr(object, "periodicity")
  if(missing(ticker)) ticker <- attr(object, "ticker")
  if(!missing(window)) object <- object[window]
  xlen <- dim(object)[1L]
  data <- xts_df(object)
  data$idx <- seq_len(xlen)
  data$cd <- as.character(data$cd)

  conditional_layers <- list(if(!all(is.na(data$cloudTop))) {
    geom_ribbon(aes(ymax = .data$cloudTop, ymin = .data$cloudBase),
                fill = pal[1L], alpha = 0.6, na.rm = TRUE)}
  )

  chart <- ggplot(data = data, aes(x = .data$index, date = .data$index)) +
    conditional_layers +
    geom_line(aes(y = .data$senkouA), col = pal[2L], alpha = 0.6, na.rm = TRUE) +
    geom_line(aes(y = .data$senkouB), col = pal[3L], alpha = 0.6, na.rm = TRUE) +
    geom_line(aes(y = .data$tenkan), col = pal[4L], na.rm = TRUE) +
    geom_line(aes(y = .data$kijun), col = pal[5L], na.rm = TRUE) +
    geom_segment(aes(xend = .data$index, y = .data$high, yend = .data$low, colour = .data$cd),
                 size = 0.3, na.rm = TRUE) +
    geom_rect(aes(xmin = .data$index - periodicity * 0.4, xmax = .data$index + periodicity * 0.4,
                  ymin = .data$open, ymax = .data$close,
                  colour = .data$cd, fill = .data$cd),
              size = 0.3, na.rm = TRUE) +
    geom_line(aes(y = .data$chikou), col = pal[6L], na.rm = TRUE) +
    scale_x_datetime(breaks = function(x) pretty(x, n = 9L),
                     labels = function(x) {
                       if(periodicity < 80000) {
                         labs <- format(x, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
                         } else {
                           labs <- format(x, paste("%d-%b", "%Y", sep = "\n"))
                           }
                       }) +
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)) +
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])) +
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])) +
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker),
         subtitle = if(!missing(message)) message else NULL) +
    theme_light() +
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

  chart
}

