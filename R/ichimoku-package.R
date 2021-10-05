#' ichimoku: Visualization and Tools for Ichimoku Kinko Hyo Strategies
#'
#' An implementation of 'Ichimoku Kinko Hyo', also commonly known as 'cloud
#'     charts'. Static and interactive visualizations with tools for creating,
#'     backtesting and development of quantitative 'ichimoku' strategies. As
#'     described in Sasaki (1996, ISBN:4925152009), the technique is a refinement
#'     on candlestick charting, originating from Japan and now in widespread use
#'     in technical analysis worldwide. Translating as 'one-glance equilibrium
#'     chart', it allows the price action and market structure of financial
#'     securities to be determined 'at-a-glance'. Incorporates an interface with
#'     the OANDA fxTrade API \url{https://developer.oanda.com/} for retrieving
#'     historical and live streaming price data for major currencies, metals,
#'     commodities, government bonds and stock indices.
#'
#' @section Principal ichimoku functions:
#' Data & Visualization
#' \itemize{
#'     \item{\code{\link{ichimoku}}} {to create an ichimoku object from price
#'     data.}
#'     \item{\code{\link{plot.ichimoku}} / \code{\link{iplot}}} {to plot
#'     (interactive) cloud charts from ichimoku objects.}
#'     \item{\code{\link{archive}}} {for reading/writing objects to/from archive
#'     files with data verification.}
#'     \item{\code{\link{oanda}}} {to retrieve price data from the OANDA fxTrade
#'     API.}
#' }
#' Strategies & ML
#' \itemize{
#'     \item{\code{\link{strat}}} {to augment an ichimoku object with a strategy,
#'     including combined and asymmetric strategies.}
#'     \item{\code{\link{stratcombine}}} {to create custom combined strategies.}
#'     \item{\code{\link{autostrat}}} {to automatically evaluate and rank
#'     top-performing strategies.}
#'     \item{\code{\link{mlgrid}}} {to generate a numeric representation of the
#'     relationship between ichimoku cloud chart elements.}
#' }
#' Real-time
#' \itemize{
#'     \item{\code{\link{oanda_chart}}} {to plot real-time ichimoku cloud
#'     charts using OANDA data.}
#'     \item{\code{\link{oanda_studio}}} {a complete live analysis environment
#'     using OANDA data implemented in R Shiny.}
#'     \item{\code{\link{oanda_stream}} / \code{\link{oanda_quote}}} {to obtain
#'     the latest live data stream / quote from the OANDA fxTrade API.}
#'     \item{\code{\link{oanda_view}}} {for a market overview showing the
#'     relative performance of constituents.}
#' }
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#' @references Sasaki, H. (1996), \emph{ichimoku kinkouhyou no kenkyuu}. Tokyo,
#'     Japan: Toushi Radar.
#'
#'     OANDA' and 'fxTrade' are trademarks owned by OANDA Corporation, an entity
#'     unaffiliated with the ichimoku package.
#'
#'     Gao, C. (2021), \emph{ichimoku: Visualization and Tools for Ichimoku
#'     Kinko Hyo Strategies}. R package version 1.2.2,
#'     \url{https://CRAN.R-project.org/package=ichimoku}.
#'
#' @useDynLib ichimoku, .registration = TRUE
#' @importFrom curl curl_fetch_memory curl_fetch_stream handle_setheaders
#'     new_handle
#' @importFrom ggplot2 aes autoplot coord_flip element_blank element_line
#'     element_rect element_text geom_col geom_line geom_rect geom_ribbon
#'     geom_segment geom_vline ggplot ggplotGrob guides labs margin rel
#'     scale_color_manual scale_fill_manual scale_x_continuous
#'     scale_y_continuous theme theme_grey %+replace%
#' @importFrom grid grid.draw grid.newpage unit
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom jsonlite parse_json
#' @importFrom stats na.omit setNames
#' @importFrom xts endpoints xts
#' @importFrom zoo coredata index
#'
#' @docType package
#' @name ichimoku-package
NULL

#' @export
zoo::index

#' @export
zoo::coredata

#' @export
xts::xts

utils::globalVariables(".data")

.onLoad <- function(libname, pkgname) {
  do_oanda <- do_oanda()
  do_oanda <<- do_oanda
  invisible()
}

