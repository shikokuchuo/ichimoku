#' ichimoku: Compute and Plot Ichimoku Kinko Hyo Cloud Charts
#'
#' An implementation of the 'Ichimoku Kinko Hyo' charting system, also commonly
#'     known as 'cloud charts', providing both publication-ready and fully-interactive
#'     charts for analysis. As described in Sasaki (1996, ISBN:4925152009), the
#'     technique is a refinement on candlestick charting originating from Japan,
#'     now in widespread use in technical analysis worldwide. Translating to
#'     'one-glance equilibrium chart', it allows the price action and market
#'     structure of financial securities to be determined 'at-a-glance'.
#'
#' @section Principal ichimoku functions:
#' \itemize{
#'     \item{\code{\link{ichimoku}}} {to create an ichimoku object.}
#'     \item{\code{\link{plot.ichimoku}}} {to plot a cloud chart from an
#'     ichimoku object.}
#'     \item{\code{\link{iplot}}} {to plot an interactive cloud chart from an
#'     ichimoku object.}
#' }
#' @author Charlie Gao <\email{charlie.gao@@shikokuchuo.net}>
#' @references Package website:
#'     \url{https://shikokuchuo.net/ichimoku/}
#'
#'     The most recent version of the package may be found at
#'     \url{https://github.com/shikokuchuo/ichimoku/}
#'
#' @useDynLib ichimoku, .registration = TRUE
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_linerange geom_rect
#'     scale_color_manual scale_fill_manual guides scale_x_datetime
#'     scale_x_continuous scale_y_continuous labs theme_light theme element_rect
#'     element_line element_text
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#'
#' @docType package
#' @name ichimoku-package
NULL
