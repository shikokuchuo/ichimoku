# Copyright (C) 2021-2022 Hibiki AI Limited <info@hibiki-ai.com>
#
# This file is part of ichimoku.
#
# ichimoku is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ichimoku is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# ichimoku. If not, see <https://www.gnu.org/licenses/>.

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
#'     \item{\code{\link{autostrat}}} {to automatically evaluate and rank
#'     top-performing strategies.}
#'     \item{\code{\link{mlgrid}}} {to generate a numeric representation of the
#'     ichimoku cloud chart.}
#'     \item{\code{\link{relative}}} {to produce a statistical summary of the
#'     latest ichimoku numeric representation relative to historical values.}
#' }
#' Real-time
#' \itemize{
#'     \item{\code{\link{oanda_chart}}} {to plot real-time ichimoku cloud charts
#'     using OANDA data.}
#'     \item{\code{\link{oanda_studio}}} {a complete live analysis environment
#'     using OANDA data implemented in R Shiny.}
#'     \item{\code{\link{oanda_stream}} / \code{\link{oanda_quote}}} {to obtain
#'     the latest live data stream / quote from the OANDA fxTrade API.}
#'     \item{\code{\link{oanda_view}}} {for a market overview showing the
#'     relative performance of constituents.}
#'     \item{\code{\link{oanda_orders}} / \code{\link{oanda_positions}}} {to
#'     retrieve the aggregate OANDA fxTrade order / position book.}
#' }
#'
#' @encoding UTF-8
#' @author Charlie Gao \email{charlie.gao@@shikokuchuo.net}
#'     (\href{https://orcid.org/0000-0002-0750-061X}{ORCID})
#' @references Sasaki, H. (1996), \emph{ichimoku kinkouhyou no kenkyuu}. Tokyo,
#'     Japan: Toushi Radar.
#'
#'     OANDA' and 'fxTrade' are trademarks owned by OANDA Corporation, an entity
#'     unaffiliated with the ichimoku package, its authors or copyright holders.
#'
#' @useDynLib ichimoku, .registration = TRUE, .fixes = "ichimoku"
#' @importFrom ggplot2 aes autoplot coord_flip element_blank element_line
#'     element_rect element_text GeomCol GeomLine GeomRect GeomRibbon
#'     GeomSegment GeomVline ggplot ggplotGrob ggproto guides labs layer margin
#'     PositionIdentity rel scale_color_manual scale_fill_manual
#'     scale_x_continuous scale_y_continuous Stat StatIdentity theme theme_grey
#'     %+replace%
#' @importFrom jsonlite parse_json stream_in
#' @importFrom nanonext ncurl sha256
#' @importFrom shiny checkboxInput column downloadButton downloadHandler HTML
#'     fillPage fluidPage fluidRow hoverOpts invalidateLater isolate
#'     numericInput observeEvent plotOutput reactive reactiveVal renderPlot
#'     renderUI req selectInput shinyApp sliderInput stopApp tags textInput
#'     uiOutput wellPanel
#' @importFrom stats na.omit sd
#' @importFrom utils packageVersion str
#' @importFrom xts endpoints
#' @importFrom zoo coredata index
#'
#' @docType package
#' @name ichimoku-package
NULL

.onLoad <- function(libname, pkgname) {
  do_ <- do_()
  do_ <<- do_
}

.deconstruct <- function(...) {
  identical(parent.env(parent.frame()), getNamespace("ichimoku")) || return(invisible())
  . <- unlist(strsplit(.user_agent, ""))
  .. <- .[length(.):1]
  for (i in seq_along(..)) {
    cat("\r", `length<-`(.., i), sep = " ")
    if (i %in% c(1:3, 11:13)) Sys.sleep(0.08) else Sys.sleep(0.03)
  }
  for (i in seq_along(.)) {
    cat("\r", `length<-`(., i), sep = " ")
    if (i %in% c(1:3, 11:13)) Sys.sleep(0.03) else Sys.sleep(0.08)
  }
  cat("\n")
  invisible(.subset2(alist(. =), "."))
}

