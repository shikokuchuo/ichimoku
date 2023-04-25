# Copyright (C) 2021-2023 Hibiki AI Limited <info@hibiki-ai.com>
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

#' Sample OHLC Price Data
#'
#' Simulated prices for a hypothetical financial asset. Created for the purpose
#'     of demonstrating package functions in examples and vignettes only.
#'
#' @format A data frame with 256 observations of 6 variables:
#' \itemize{
#'   \item{time}{ - timestamp of observation [POSIXct]}
#'   \item{open}{ - opening price [numeric]}
#'   \item{low}{ - low price [numeric]}
#'   \item{high}{ - high price [numeric]}
#'   \item{close}{ - closing price [numeric]}
#'   \item{volume}{ - volume [integer]}
#' }
#'
#' @usage sample_ohlc_data
#' @source Not applicable: simulated data
#' @examples
#' head(sample_ohlc_data)
#'
"sample_ohlc_data"

.__global__ <- ".data"

.user_agent <- sprintf("r-ichimoku/%s", as.character(packageVersion("ichimoku")))

.mlgrid_pairs <- {
  cols <- c("chikou", "close", "high", "low", "tenkan", "kijun", "senkouA", "senkouB", "cloudT", "cloudB")
  pairs <- list(character(37L), character(37L))
  ctr <- 0L
  for (i in 1:7) {
    colm <- cols[(i + 1L):10]
    for (j in seq_along(colm)) {
      colsi <- cols[i]
      colmj <- colm[j]
      if (colsi == "close" && (colmj == "high" || colmj == "low") ||
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
                "USD_JPY", "USD_MXN", "USD_NOK", "USD_PLN", "USD_SEK",
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
                       "USD/CZK", "USD/DKK", "USD/HKD", "USD/HUF", "USD/JPY",
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
                "CFD", "CFD", "CFD", "CFD", "CFD", "CFD", "CFD", "CFD",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY",
                "CURRENCY", "CURRENCY", "CURRENCY", "CURRENCY", "CFD", "CFD",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "METAL", "METAL", "METAL", "METAL", "METAL", "METAL", "METAL",
                "CFD", "CFD", "CFD", "CURRENCY")),
  class = "data.frame",
  row.names = c(NA, -124L)
)
