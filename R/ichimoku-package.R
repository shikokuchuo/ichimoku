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

.__global__ <- ".data"

.user_agent <- sprintf("r-ichimoku/%s", as.character(packageVersion("ichimoku")))

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

.mlgrid_pairs <- {
  cols <- c("chikou", "close", "high", "low", "tenkan", "kijun", "senkouA", "senkouB", "cloudT", "cloudB")
  pairs <- list(character(37L), character(37L))
  ctr <- 0L
  for (i in 1:7) {
    colm <- cols[(i + 1L):10]
    for (j in seq_along(colm)) {
      colsi <- cols[i]
      colmj <- colm[j]
      if(colsi == "close" && (colmj == "high" || colmj == "low") ||
         colsi == "high" && colmj == "low") next
      ctr <- ctr + 1L
      pairs[[1]][ctr] <- colsi
      pairs[[2]][ctr] <- colmj
      if (colsi == "senkouA" && colmj == "senkouB") break
    }
  }
  pairs
}

.ichimoku_themes <- list(
  classic = c("#ffc6cb", "#c3dede", "#e0a9e0", "#db4525", "#1aa1a6", "#a4d1eb",
              "#00008b", "#00008b", "#00008b", "#ffffff", "#191970", "#00008b"),
  dark = c("#ffb6c1", "#b4cdcd", "#dda0dd", "#c71585", "#40e0d0", "#b58900",
           "#eee8d5", "#fdf6e3", "#fdf6e3", "#002b36", "#eee8d5", "#fdf6e3"),
  mono = c("#d9d9d9", "#d7d7d7", "#d1d1d1", "#737373", "#1f1f1f", "#b8b8b8",
           "#1a1a1a", "#1a1a1a", "#1a1a1a", "#ffffff", "#333333", "#1a1a1a"),
  noguchi = c("#feddbf", "#ffceba", "#f2af07", "#f81e00", "#009039", "#297bb1",
              "#00163c", "#00163c", "#00163c", "#e4e2cc", "#e26100", "#e26100"),
  `okabe-ito` = c("#cc79a7", "#000000", "#999999", "#d55e00", "#009e73", "#e69f00",
                  "#0072b2", "#0072b2", "#0072b2", "#ffffff", "#56b4e9", "#0072b2"),
  solarized = c("#d33682", "#eee8d5", "#6c71c4", "#cb4b16", "#859900", "#268bd2",
                "#002b36", "#002b36", "#002b36", "#fdf6e3", "#073642", "#002b36")
)

.oanda_instruments <- structure(
  list(name = c("AU200_AUD", "AUD_CAD", "AUD_CHF", "AUD_HKD",
                "AUD_JPY", "AUD_NZD", "AUD_SGD", "AUD_USD", "BCO_USD", "CAD_CHF",
                "CAD_HKD", "CAD_JPY", "CAD_SGD", "CH20_CHF", "CHF_HKD", "CHF_JPY",
                "CHF_ZAR", "CHINAH_HKD", "CN50_USD", "CORN_USD", "DE10YB_EUR",
                "DE30_EUR", "ESPIX_EUR", "EU50_EUR", "EUR_AUD", "EUR_CAD", "EUR_CHF",
                "EUR_CZK", "EUR_DKK", "EUR_GBP", "EUR_HKD", "EUR_HUF", "EUR_JPY",
                "EUR_NOK", "EUR_NZD", "EUR_PLN", "EUR_SEK", "EUR_SGD", "EUR_TRY",
                "EUR_USD", "EUR_ZAR", "FR40_EUR", "GBP_AUD", "GBP_CAD", "GBP_CHF",
                "GBP_HKD", "GBP_JPY", "GBP_NZD", "GBP_PLN", "GBP_SGD", "GBP_USD",
                "GBP_ZAR", "HK33_HKD", "HKD_JPY", "JP225_USD", "JP225Y_JPY",
                "NAS100_USD", "NATGAS_USD", "NL25_EUR", "NZD_CAD", "NZD_CHF",
                "NZD_HKD", "NZD_JPY", "NZD_SGD", "NZD_USD", "SG30_SGD", "SGD_CHF",
                "SGD_JPY", "SOYBN_USD", "SPX500_USD", "SUGAR_USD", "TRY_JPY",
                "TWIX_USD", "UK100_GBP", "UK10YB_GBP", "US2000_USD", "US30_USD",
                "USB02Y_USD", "USB05Y_USD", "USB10Y_USD", "USB30Y_USD", "USD_CAD",
                "USD_CHF", "USD_CNH", "USD_CZK", "USD_DKK", "USD_HKD", "USD_HUF",
                "USD_INR", "USD_JPY", "USD_MXN", "USD_NOK", "USD_PLN", "USD_SEK",
                "USD_SGD", "USD_THB", "USD_TRY", "USD_ZAR", "WHEAT_USD", "WTICO_USD",
                "XAG_AUD", "XAG_CAD", "XAG_CHF", "XAG_EUR", "XAG_GBP", "XAG_HKD",
                "XAG_JPY", "XAG_NZD", "XAG_SGD", "XAG_USD", "XAU_AUD", "XAU_CAD",
                "XAU_CHF", "XAU_EUR", "XAU_GBP", "XAU_HKD", "XAU_JPY", "XAU_NZD",
                "XAU_SGD", "XAU_USD", "XAU_XAG", "XCU_USD", "XPD_USD", "XPT_USD",
                "ZAR_JPY"),
       displayName = c("Australia 200", "AUD/CAD", "AUD/CHF",
                       "AUD/HKD", "AUD/JPY", "AUD/NZD", "AUD/SGD", "AUD/USD", "Brent Crude Oil",
                       "CAD/CHF", "CAD/HKD", "CAD/JPY", "CAD/SGD", "Switzerland 20",
                       "CHF/HKD", "CHF/JPY", "CHF/ZAR", "China H Shares", "China A50",
                       "Corn", "Bund", "Germany 30", "Spain 35", "Europe 50", "EUR/AUD",
                       "EUR/CAD", "EUR/CHF", "EUR/CZK", "EUR/DKK", "EUR/GBP", "EUR/HKD",
                       "EUR/HUF", "EUR/JPY", "EUR/NOK", "EUR/NZD", "EUR/PLN", "EUR/SEK",
                       "EUR/SGD", "EUR/TRY", "EUR/USD", "EUR/ZAR", "France 40", "GBP/AUD",
                       "GBP/CAD", "GBP/CHF", "GBP/HKD", "GBP/JPY", "GBP/NZD", "GBP/PLN",
                       "GBP/SGD", "GBP/USD", "GBP/ZAR", "Hong Kong 33", "HKD/JPY",
                       "Japan 225", "Japan 225 (JPY)", "US Nas 100", "Natural Gas",
                       "Netherlands 25", "NZD/CAD", "NZD/CHF", "NZD/HKD", "NZD/JPY",
                       "NZD/SGD", "NZD/USD", "Singapore 30", "SGD/CHF", "SGD/JPY", "Soybeans",
                       "US SPX 500", "Sugar", "TRY/JPY", "Taiwan Index", "UK 100", "UK 10Y Gilt",
                       "US Russ 2000", "US Wall St 30", "US 2Y T-Note", "US 5Y T-Note",
                       "US 10Y T-Note", "US T-Bond", "USD/CAD", "USD/CHF", "USD/CNH",
                       "USD/CZK", "USD/DKK", "USD/HKD", "USD/HUF", "USD/INR", "USD/JPY",
                       "USD/MXN", "USD/NOK", "USD/PLN", "USD/SEK", "USD/SGD", "USD/THB",
                       "USD/TRY", "USD/ZAR", "Wheat", "West Texas Oil", "Silver/AUD",
                       "Silver/CAD", "Silver/CHF", "Silver/EUR", "Silver/GBP", "Silver/HKD",
                       "Silver/JPY", "Silver/NZD", "Silver/SGD", "Silver", "Gold/AUD",
                       "Gold/CAD", "Gold/CHF", "Gold/EUR", "Gold/GBP", "Gold/HKD", "Gold/JPY",
                       "Gold/NZD", "Gold/SGD", "Gold", "Gold/Silver", "Copper", "Palladium",
                       "Platinum", "ZAR/JPY"),
       type = c("CFD", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD", "CURRENCY",
                "CURRENCY", "CURRENCY", "CFD", "CFD", "CFD", "CFD", "CFD", "CFD",
                "CFD", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CFD", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD",
                "CURRENCY", "CFD", "CFD", "CFD", "CFD", "CFD", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD",
                "CURRENCY", "CURRENCY", "CFD", "CFD", "CFD", "CURRENCY", "CFD",
                "CFD", "CFD", "CFD", "CFD", "CFD", "CFD", "CFD", "CFD", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD", "CFD",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "CFD", "CFD", "CFD", "CURRENCY")),
  class = "data.frame",
  row.names = c(NA, -125L)
)

