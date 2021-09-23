# Ichimoku - Visualization Layer -----------------------------------------------

#' Plot Ichimoku Cloud Chart
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param window (optional) a date-time window to subset the plot, in ISO-8601
#'     compatible range strings of the format used for 'xts' objects, for example
#'     "2020-02-15/2020-08-15" or "2020-02-15/", "/2020-08" or "2020-07".
#' @param ticker (optional) specify a ticker (or other text) to include in the
#'     chart heading. If not set, the ticker saved within the ichimoku object
#'     will be used.
#' @param subtitle (optional) specify a subtitle to display under the chart title.
#' @param theme [default 'original'] with alternative choices of 'dark',
#'     'solarized' or 'mono'.
#' @param strat [default TRUE] if the ichimoku object contains a strategy, the
#'     periods for which the strategy results in a position will be shaded, and
#'     the strategy printed as the chart subtitle (if not otherwise specified).
#'     Set to FALSE to turn off this behaviour.
#' @param type [default 'none'] type of sub-plot to display beneath the ichimoku
#'     cloud chart, with a choice of 'none', 'r' or 's' for the corresponding
#'     oscillator type, and 'bar' or 'line' for custom plots.
#' @param custom (optional) character string (containing a regular expression)
#'     matching the column name of the variable to be displayed as sub-plot.
#'     Specify \code{type = 'bar'} or \code{type = 'line'}, otherwise other type
#'     settings will take precedence.
#' @param ... additional arguments passed along to the print method for 'ggplot'
#'     objects when \code{type = 'none'}.
#'
#' @return The ichimoku object supplied (invisibly). The requested plot is output
#'     to the graphical device.
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
#' plot(cloud, window = "2020-05-01/2020-12-01", theme = "dark")
#' plot(cloud, window = "2020-05/", ticker = "TKR Co.", theme = "solarized", type = "s")
#' plot(cloud, window = "/2020-11-02", subtitle = "Sample Price Data", theme = "mono", type = "r")
#'
#' kumo <- ichimoku(sample_ohlc_data, ticker = "TKR", keep.data = TRUE)
#'
#' plot(kumo, theme = "solarized", type = "bar", custom = "volume")
#' plot(kumo, theme = "original", type = "line", custom = "volume")
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x,
                          window,
                          ticker,
                          subtitle,
                          theme = c("original", "dark", "solarized", "mono"),
                          strat = TRUE,
                          type = c("none", "r", "s", "bar", "line"),
                          custom,
                          ...) {

  type <- match.arg(type)
  switch(type,
         none = print(autoplot.ichimoku(x, window = window, ticker = ticker, subtitle = subtitle,
                                        theme = theme, strat = strat), ...),
         r =,
         s = extraplot(x, window = window, ticker = ticker, subtitle = subtitle, theme = theme,
                       strat = strat, type = type),
         extraplot(x, window = window, ticker = ticker, subtitle = subtitle,
                   theme = theme, strat = strat, type = type, custom = custom))

  invisible(x)

}

#' @name autoplot
#' @rdname autoplot.ichimoku
#' @export
NULL

#' autoplot.ichimoku
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
#'
#' @inheritParams plot.ichimoku
#' @param object an object of class 'ichimoku'.
#' @param ... other arguments not used by this method.
#'
#' @return A ggplot2 object with S3 classes 'gg' and 'ggplot'.
#'
#' @details This function is an S3 method for the generic function autoplot()
#'     for class 'ichimoku'. It can be invoked by calling autoplot(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @keywords internal
#' @export
#'
autoplot.ichimoku <- function(object,
                              window,
                              ticker,
                              subtitle,
                              theme = c("original", "dark", "solarized", "mono"),
                              strat = TRUE,
                              ...) {

  theme <- match.arg(theme)
  pal <- ichimoku_themes[[theme]]
  if (missing(ticker)) ticker <- attr(object, "ticker")
  if (missing(subtitle)) {
    subtitle <- if (hasStrat(object) && isTRUE(strat)) paste0("Strategy: ", attr(object, "strat")["Strategy", ][[1L]])
  }

  if (!missing(window)) object <- object[window]
  data <- xts_df(object)
  data$idx <- attr(data, "row.names")
  data$cd <- as.character(.subset2(data, "cd"))

  layers <- list(
    if (hasStrat(object) && isTRUE(strat)) {
      geom_rect(aes(xmin = .data$posn * (.data$idx - 0.5), xmax = .data$posn * (.data$idx + 0.5),
                    ymin = -Inf, ymax = Inf), fill = pal[1L], alpha = 0.2, na.rm = TRUE)
    },
    if (!all(is.na(.subset2(data, "senkouB")))) {
      geom_ribbon(aes(ymax = .data$senkouA, ymin = .data$senkouB),
                  fill = pal[1L], alpha = 0.6, na.rm = TRUE)
    },
    geom_line(aes(y = .data$senkouB), color = pal[2L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$senkouA), color = pal[3L], alpha = 0.6, na.rm = TRUE),
    geom_line(aes(y = .data$kijun), color = pal[5L], na.rm = TRUE),
    geom_line(aes(y = .data$tenkan), color = pal[4L], na.rm = TRUE),
    geom_segment(aes(xend = .data$idx, y = .data$high, yend = .data$low, color = .data$cd),
                 size = 0.3, na.rm = TRUE),
    geom_rect(aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4, ymin = .data$open,
                  ymax = .data$close, color = .data$cd, fill = .data$cd),
              size = 0.3, na.rm = TRUE),
    geom_line(aes(y = .data$chikou), color = pal[6L], na.rm = TRUE),
    scale_x_continuous(breaks = breaks_ichimoku(data = data, object = object),
                       labels = labels_ichimoku(data = data, object = object)),
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)),
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])),
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])),
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker), subtitle = subtitle),
    switch(theme, dark = theme_ichimoku_dark(), theme_ichimoku_light())
  )

  ggplot(data = data, aes(x = .data$idx)) + layers

}

#' extraplot
#'
#' Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects with a sub-plot for
#'     oscillators or a custom specified variable.
#'
#' @inheritParams autoplot.ichimoku
#' @inheritParams plot.ichimoku
#'
#' @return A gtable object with S3 classes 'gtable', 'gTree', 'grob' and 'gDesc',
#'     or else a ggplot2 object with S3 classes 'gg' and 'ggplot' when falling
#'     back to a standard plot.
#'
#' @details The oscillator choices are between 'R-type', which is a modified form
#'     of a relative strength index (RSI), and 'S-type', which is a modified form
#'     of a stochastic oscillator. The oscillator look-back parameters are based
#'     on the fast and medium cloud periods of the ichimoku object.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by running:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @keywords internal
#' @export
#'
extraplot <- function(object,
                      window,
                      ticker,
                      subtitle,
                      theme = c("original", "dark", "solarized", "mono"),
                      strat = TRUE,
                      type = c("none", "r", "s", "bar", "line"),
                      custom,
                      ...) {

  type <- match.arg(type)
  theme <- match.arg(theme)
  pal <- ichimoku_themes[[theme]]
  aplot <- autoplot.ichimoku(object = object, window = window, ticker = ticker,
                             subtitle = subtitle, theme = theme, strat = strat)

  if (type == "none") {
    warning("Required argument 'type' not specified or set to 'none'", call. = FALSE)
    return(print(aplot))
  }
  if (type == "bar" || type == "line") {
    if(missing(custom)) {
      warning("For type = 'bar' or 'line': required argument 'custom' not specified", call. = FALSE)
      return(print(aplot))
    }
    cnames <- dimnames(object)[[2L]]
    sel <- grep(custom, cnames, ignore.case = TRUE, perl = TRUE)[1L]
    if (is.na(sel)) {
      warning("Specified value '", custom, "' for 'custom' does not match any columns", call. = FALSE)
      return(print(aplot))
    }

  } else if (type == "r") {
    p2 <- attr(object, "periods")[2L]
    object$rtype_osc <- 100 - 100 / (1 + meanOver((object[, "cd"] == 1) * (object[, "close"] - object[, "open"]), p2) / meanOver((object[, "cd"] == -1) * (object[, "open"] - object[, "close"]), p2))

  } else {
    periods <- attr(object, "periods")
    p1 <- periods[1L]
    p2 <- periods[2L]
    object$stype_fast <- 100 * (object[, "close"] - minOver(object[, "low"], p1)) / (maxOver(object[, "high"], p1) - minOver(object[, "low"], p1))
    object$stype_slow <- 100 * (object[, "close"] - minOver(object[, "low"], p2)) / (maxOver(object[, "high"], p2) - minOver(object[, "low"], p2))
  }

  if (!missing(window)) object <- object[window]
  data <- xts_df(object)
  data$idx <- attr(data, "row.names")

  if (type == "r" || type == "s") {
    layers <- list(
      if (type == "r") geom_line(aes(y = .data$rtype_osc), color = pal[5L], na.rm = TRUE),
      if (type == "s") geom_line(aes(y = .data$stype_fast), color = pal[4L], na.rm = TRUE),
      if (type == "s") geom_line(aes(y = .data$stype_slow), color = pal[5L], na.rm = TRUE),
      scale_x_continuous(breaks = breaks_ichimoku(data = data, object = object), labels = NULL),
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100), expand = c(0,0)),
      labs(x = NULL, y = switch(type, r = "R-type", s = "S-type")),
      switch(theme, dark = theme_ichimoku_dark(), theme_ichimoku_light()),
      theme(axis.ticks.x = element_blank())
    )

  } else {

    cols <- cnames[sel]
    llen <- nchar(as.character(trunc(max(.subset2(data, cols), na.rm = TRUE))))
    data$cd <- as.character(.subset2(data, "cd"))

    layers <- list(
      if (type == "line") {
        geom_line(aes(y = .data[[cols]]), color = pal[7L], na.rm = TRUE)
      } else {
        geom_rect(aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4, ymin = 0,
                      ymax = .data[[cols]], color = .data$cd, fill = .data$cd),
                  size = 0.3, na.rm = TRUE)
      },
      scale_x_continuous(breaks = breaks_ichimoku(data = data, object = object), labels = NULL),
      scale_y_continuous(),
      scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])),
      scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])),
      labs(x = NULL, y = cols),
      switch(theme, dark = theme_ichimoku_dark(), theme_ichimoku_light()),
      theme(axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = rel(min(3 / llen, 1))))
    )
  }

  subplot <- ggplot(data = data, aes(x = .data$idx)) + layers

  gp <- ggplotGrob(aplot + labs(x = NULL))
  gs <- ggplotGrob(subplot)
  gs$widths[2:5] <- gp$widths[2:5]
  gt <- gtable(widths = unit(1, "null"), heights = unit(c(0.75, 0.25), "null"))
  gt <- gtable_add_grob(gt, list(gp, gs), t = c(1, 2), l = c(1, 1), clip = "off")
  grid.newpage()
  grid.draw(gt)
  invisible(gt)

}

#' Ichimoku Breaks for ggplot2
#'
#' Internal function used to create custom pretty breaks for ggplot2.
#'
#' @param data the data frame used by ggplot2.
#' @param object the original ichimoku object.
#'
#' @return A vector of integer values representing the break locations.
#'
#' @noRd
#'
breaks_ichimoku <- function(data, object) {

  if (attr(object, "periodicity") > 80000) {
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
    if (breaks[length(breaks)] > dim(object)[1L]) breaks[length(breaks)] <- breaks[length(breaks)] - 1L

  } else {
    breaks <- pretty.default(.subset2(data, "idx"), n = 9L) + 1
    if (breaks[length(breaks)] > dim(object)[1L]) breaks <- breaks[-length(breaks)]
  }

  breaks

}

#' Ichimoku Break Labels for ggplot2
#'
#' Internal function used to create custom break labels for ggplot2.
#'
#' @param data the data frame used by ggplot2.
#' @param object the original ichimoku object.
#'
#' @return A vector of character labels corresponding to the breaks.
#'
#' @noRd
#'
labels_ichimoku <- function(data, object) {

  function(x) {
    labels <- .subset2(data, "index")[x]
    if (attr(object, "periodicity") > 80000) {
      format(labels, paste("%d-%b", "%Y", sep = "\n"))
    } else {
      format(labels, paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
    }
  }
}

#' Ichimoku Light Theme for ggplot2
#'
#' Internal function used by ichimoku to create the default theme for ggplot2.
#'
#' @return A list with classes 'theme' and 'gg'.
#'
#' @noRd
#'
theme_ichimoku_light <- function() {
  theme_grey() %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey70", size = rel(1)),
          panel.grid = element_line(colour = "grey87"),
          panel.grid.major = element_line(size = rel(0.5)),
          panel.grid.minor = element_line(size = rel(0.25)),
          axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
          legend.position = "none",
          strip.background = element_rect(fill = "grey70", colour = NA),
          strip.text = element_text(colour = "white", size = rel(0.8), margin = margin(4.4, 4.4, 4.4, 4.4)),
          complete = TRUE)
}

#' Ichimoku Dark Theme for ggplot2
#'
#' Internal function used by ichimoku to create the default dark theme for ggplot2.
#'
#' @return A list with classes 'theme' and 'gg'.
#'
#' @noRd
#'
theme_ichimoku_dark <- function() {
  theme_ichimoku_light() +
    theme(plot.title = element_text(colour = "#eee8d5"),
          plot.subtitle = element_text(colour = "#eee8d5"),
          plot.background = element_rect(fill = "#586e75", colour = NA),
          panel.background = element_rect(fill = "#002b36", colour = NA),
          panel.grid = element_line(colour = "#073642"),
          axis.title = element_text(colour = "#eee8d5"),
          axis.text = element_text(colour = "#eee8d5"),
          axis.ticks = element_line(colour = "#eee8d5", size = rel(0.5)))
}

