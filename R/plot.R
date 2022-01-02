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
#' @param theme [default 'original'] with alternative choices of 'conceptual',
#'     'dark', 'fresh', 'mono', or 'solarized'.
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
#'     objects.
#'
#' @return The ichimoku object supplied (invisibly). The requested plot is output
#'     to the graphical device.
#'
#' @details This function is an S3 method for the generic function plot() for
#'     class 'ichimoku'. It can be invoked by calling plot(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#'     For further details please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' plot(cloud)
#' plot(cloud, window = "2020-05-01/2020-12-01", theme = "dark")
#' plot(cloud, window = "2020-05/", ticker = "TKR Co.", theme = "conceptual", type = "s")
#' plot(cloud, window = "/2020-11-02", subtitle = "Sample Price Data", theme = "mono", type = "r")
#'
#' kumo <- ichimoku(sample_ohlc_data, ticker = "TKR", keep.data = TRUE)
#' plot(kumo, window = "2020-05/", theme = "solarized", type = "bar", custom = "volume")
#' plot(kumo, window = "2020-05/", theme = "fresh", type = "line", custom = "volume")
#'
#' @method plot ichimoku
#' @export
#'
plot.ichimoku <- function(x,
                          window,
                          ticker,
                          subtitle,
                          theme = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                          strat = TRUE,
                          type = c("none", "r", "s", "bar", "line"),
                          custom,
                          ...) {

  print(autoplot.ichimoku(x, window = window, ticker = ticker, subtitle = subtitle,
                          theme = theme, strat = strat, type = type, custom = custom), ...)
  invisible(x)

}

#' @name autoplot
#' @rdname autoplot.ichimoku
#' @export
NULL

#' Plot Ichimoku Objects with ggplot2
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
#' @rdname autoplot.ichimoku
#' @method autoplot ichimoku
#' @keywords internal
#' @export
#'
autoplot.ichimoku <- function(object,
                              window,
                              ticker,
                              subtitle,
                              theme = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                              strat = TRUE,
                              type = c("none", "r", "s", "bar", "line"),
                              custom,
                              ...) {

  theme <- match.arg(theme)
  type <- match.arg(type)
  object <- create_data(object = object, window = window, type = type)
  plot_ichimoku(object = object, ticker = ticker, subtitle = subtitle, theme = theme,
                strat = strat, type = type, custom = custom, ...)

}

#' Create Data from an ichimoku Object
#'
#' Internal function used to create a dataframe for ggplot2.
#'
#' @inheritParams plot.ichimoku
#'
#' @return A dataframe.
#'
#' @noRd
#'
create_data <- function(object, window, type) {

  if (type == "r") {
    core <- coredata.ichimoku(object)
    p2 <- attr(object, "periods")[2L]
    cd <- core[, "cd"]
    close <- core[, "close"]
    open <- core[, "open"]
    object$osc_typ_slw <- 1 - 1 /
      (1 + .Call(ichimoku_wmean, (cd == 1) * (close - open), p2) /
         .Call(ichimoku_wmean, (cd == -1) * (open - close), p2))

  } else if (type == "s") {
    core <- coredata.ichimoku(object)
    p1 <- attr(object, "periods")[1L]
    p2 <- attr(object, "periods")[2L]
    close <- core[, "close"]
    low <- core[, "low"]
    high <- core[, "high"]
    object$osc_typ_fst <- (close - .Call(ichimoku_wmin, low, p1)) /
      (.Call(ichimoku_wmax, high, p1) - .Call(ichimoku_wmin, low, p1))
    object$osc_typ_slw <- (close - .Call(ichimoku_wmin, low, p2)) /
      (.Call(ichimoku_wmax, high, p2) - .Call(ichimoku_wmin, low, p2))
  }

  if (!missing(window)) object <- object[window]
  object

}

#' Create ggplot Plot from an ichimoku Object
#'
#' Internal function used to create a ggplot2 object.
#'
#' @inheritParams plot.ichimoku
#'
#' @return A ggplot2 object with S3 classes 'gg' and 'ggplot'.
#'
#' @noRd
#'
plot_ichimoku <- function(object, ticker, subtitle, theme, strat, type, custom, ...) {

  data <- .Call(ichimoku_df, object)
  is.null(data) && stop("attempt to plot incomplete (partial or subset) ichimoku object", call. = FALSE)
  pal <- .ichimoku_themes[[theme]]
  showstrat <- hasStrat(object) && isTRUE(strat)
  if (missing(ticker)) ticker <- attr(object, "ticker")
  if (missing(subtitle)) {
    subtitle <- if (showstrat) paste0("Strategy: ", attr(object, "strat")["Strategy", ][[1L]])
  }

  if (type == "line" || type == "bar") {
    if(missing(custom)) {
      warning("For type = 'bar' or 'line': required argument 'custom' not specified", call. = FALSE)
      type <- "none"
    } else {
      cnames <- attr(data, "names")
      sel <- grep(custom, cnames, ignore.case = TRUE, perl = TRUE)[1L]
      if (is.na(sel)) {
        warning("Specified value '", custom, "' for 'custom' does not match any columns", call. = FALSE)
        type <- "none"
      } else {
        cols <- cnames[sel]
      }
    }
  }

  layers <- list(
    if (showstrat)
      layer(geom = GeomRect, mapping = aes(xmin = .data$posn * (.data$idx - 0.5),
                                           xmax = .data$posn * (.data$idx + 0.5),
                                           ymin = -Inf, ymax = Inf),
            stat = StatIdentity, position = PositionIdentity,
            params = list(na.rm = TRUE, fill = pal[1L], alpha = 0.2),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    if (!all(is.na(.subset2(data, "senkouB"))))
      layer(geom = GeomRibbon, mapping = aes(ymax = .data$senkouA, ymin = .data$senkouB),
            stat = StatIdentity, position = PositionIdentity,
            params = list(na.rm = TRUE, outline.type = "both", fill = pal[1L], alpha = 0.6),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomLine, mapping = aes(y = .data$senkouB),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, colour = pal[2L], alpha = 0.6),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomLine, mapping = aes(y = .data$senkouA),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, colour = pal[3L], alpha = 0.6),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomLine, mapping = aes(y = .data$kijun),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, colour = pal[5L]),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomLine, mapping = aes(y = .data$tenkan),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, colour = pal[4L]),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomSegment, mapping = aes(xend = .data$idx, y = .data$high,
                                            yend = .data$low, colour = .data$cd),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, size = 0.3),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomRect, mapping = aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4,
                                         ymin = .data$open, ymax = .data$close,
                                         colour = .data$cd, fill = .data$cd),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, size = 0.3),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomLine, mapping = aes(y = .data$chikou),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = TRUE, colour = pal[6L]),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    if (type == "r" || type == "s")
      layer(geom = GeomLine, mapping = aes(y = .data$osc_typ_slw, ext = .data$low),
            stat = StatIndicator, position = PositionIdentity,
            params = list(na.rm = TRUE, colour = pal[5L], alpha = 0.8),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    if (type == "s")
      layer(geom = GeomLine, mapping = aes(y = .data$osc_typ_fst, ext = .data$low),
            stat = StatIndicator, position = PositionIdentity,
            params = list(na.rm = TRUE, colour = pal[4L], alpha = 0.8),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    if (type == "line")
      layer(geom = GeomLine, mapping = aes(y = .data[[cols]], ext = .data$low),
            stat = StatLine, position = PositionIdentity,
            params = list(na.rm = TRUE, colour = pal[7L], alpha = 0.8),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    if (type == "bar")
      layer(geom = GeomRect,
            mapping = aes(xmin = .data$idx - 0.4, xmax = .data$idx + 0.4,
                          ymin = 0, ymax = .data[[cols]], ext = .data$low),
            stat = StatBar, position = PositionIdentity,
            params = list(na.rm = TRUE, colour = pal[7L], fill = pal[10L], size = 0.3, alpha = 0.8),
            inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    scale_x_continuous(breaks = breaks_ichimoku(object), labels = labels_ichimoku(object)),
    scale_y_continuous(breaks = function(x) pretty.default(x, n = 9L)),
    scale_color_manual(values = c("1" = pal[7L], "-1" = pal[8L], "0" = pal[9L])),
    scale_fill_manual(values = c("1" = pal[10L], "-1" = pal[11L], "0" = pal[12L])),
    labs(x = "Date | Time", y = "Price", title = paste0("Ichimoku Kinko Hyo : : ", ticker), subtitle = subtitle),
    switch(theme, dark = theme_ichimoku_dark(), theme_ichimoku_light())
  )

  ggplot(data = data, mapping = aes(x = .data$idx)) + layers

}

#' Ichimoku Breaks for ggplot2
#'
#' Internal function used to create custom pretty breaks for ggplot2.
#'
#' @param object an ichimoku object.
#'
#' @return A vector of integer values representing the break locations.
#'
#' @noRd
#'
breaks_ichimoku <- function(object) {

  xlen = attr(object, "dim")[1L]
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
    if (breaks[length(breaks)] > xlen) breaks[length(breaks)] <- breaks[length(breaks)] - 1L

  } else {
    breaks <- pretty.default(seq_len(xlen), n = 9L) + 1
    if (breaks[length(breaks)] > xlen) breaks <- breaks[-length(breaks)]
  }

  breaks

}

#' Ichimoku Break Labels for ggplot2
#'
#' Internal function used to create custom break labels for ggplot2.
#'
#' @param object an ichimoku object.
#'
#' @return A vector of character labels corresponding to the breaks.
#'
#' @noRd
#'
labels_ichimoku <- function(object) {

  function(x) {
    labels <- .Call(ichimoku_psxct, .subset(attr(object, "index"), x))
    if (attr(object, "periodicity") > 80000) {
      format.POSIXct(labels, format = paste("%d-%b", "%Y", sep = "\n"))
    } else {
      format.POSIXct(labels, format = paste("%H:%M", "%d-%b", "%Y", sep = "\n"))
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

#' StatIndicator
#'
#' A Stat for plotting indicators in ggplot2.
#'
#' @return A ggplot2 'Stat' (an environment).
#'
#' @noRd
#'
StatIndicator <- ggproto(
  "StatIndicator", Stat,
  compute_group = function(data, scales) {
    ext <- .subset2(data, "ext")
    data$y = min(ext) + 0.2 * (max(ext) - min(ext)) * (.subset2(data, "y") - 1)
    data
  },
  required_aes = c("x", "y", "ext")
)

#' StatLine
#'
#' A Stat for plotting 'line' type subplots in ggplot2.
#'
#' @return A ggplot2 'Stat' (an environment).
#'
#' @noRd
#'
StatLine <- ggproto(
  "StatLine", Stat,
  compute_group = function(data, scales) {
    y <- .subset2(data, "y")
    ynorm <- (y - min(y)) / (max(y) - min(y))
    ext <- .subset2(data, "ext")
    data$y = min(ext) + 0.2 * (max(ext) - min(ext)) * (ynorm - 1)
    data
  },
  required_aes = c("x", "y", "ext")
)

#' StatBar
#'
#' A Stat for plotting 'bar' type subplots in ggplot2.
#'
#' @return A ggplot2 'Stat' (an environment).
#'
#' @noRd
#'
StatBar <- ggproto(
  "StatBar", Stat,
  compute_group = function(data, scales) {
    y <- .subset2(data, "ymax")
    ymin <- min(y[!is.na(y)])
    ymax <- max(y[!is.na(y)])
    ynorm <- (y - ymin) / (ymax - ymin)
    ext <- .subset2(data, "ext")
    extmin <- min(ext[!is.na(ext)])
    extmax <- max(ext[!is.na(ext)])
    data$ymax = extmin + 0.2 * (extmax - extmin) * (ynorm - 1)
    data$ymin = rep(extmin - 0.2 * (extmax - extmin), length(y))
    data
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax", "ext")
)

