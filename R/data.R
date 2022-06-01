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
