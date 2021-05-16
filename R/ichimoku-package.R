#' ichimoku: Ichimoku Kinko Hyo Cloud Charts
#'
#' An implementation of the Ichimoku Kinko Hyo charting technique, also commonly
#'     known as 'cloud charts'. Originating from and popularised in Japan, it is
#'     a refinement on candlestick charting that is in widespread use on trading
#'     floors worldwide. Translating to 'one-glance equilibrium chart', it
#'     allows the price action and market structure of financial securities to
#'     be determined 'at-a-glance'. This package contains functions to compute
#'     and plot both static and interactive ichimoku cloud charts.
#'
#' @section Principal ichimoku functions:
#' \itemize{
#'     \item{\code{\link{ichimoku}}} {to create an ichimoku object}
#'     \item{\code{\link{plot.ichimoku}}} {to plot an ichimoku cloud chart from
#'     an ichimoku object}
#'     \item{\code{\link{iplot}}} {to plot an interactive ichimoku cloud chart
#'     from an ichimoku object}
#' }
#' @author Charlie Gao <\email{charlie.gao@@shikokuchuo.net}>
#' @references Package website:
#'     \url{https://shikokuchuo.net/ichimoku/}
#'
#'     The most recent version of the package may be found at
#'     \url{https://github.com/shikokuchuo/ichimoku/}
#'
#' @importFrom RcppRoll roll_maxr roll_minr
#' @importFrom timeDate isBizday as.timeDate
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_linerange geom_rect
#'     scale_color_manual scale_fill_manual guides scale_x_datetime
#'     scale_x_continuous scale_y_continuous labs theme_light theme element_rect
#'     element_line element_text rel
#' @importFrom rlang .data
#'
#' @docType package
#' @name ichimoku-package
NULL
