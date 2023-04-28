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

# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' Switch Default OANDA Server
#'
#' Switch the default OANDA fxTrade server from 'practice' to 'live' or vice versa.
#'     Settings persist for the current session only.
#'
#' @return Invisible NULL. A message informs the resulting default server setting.
#'
#' @details The default server at the start of a new session is the practice server.
#'     Call this function to switch to the live server.
#'
#'     This function can be used to toggle between the practice and live servers.
#'     Any cached variables for API key, account or instruments list are cleared
#'     each time this function is called.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' oanda_switch()
#' oanda_switch()
#'
#' @export
#'
oanda_switch <- function() do_$switchServer()

#' ichimoku Internal Functions
#'
#' Encapsulates package internal functions in a common environment.
#'
#' @return A list of closures.
#'
#' @noRd
#'
do_ <- function() {

  server_type <- "practice"
  livestore <- keystore <- instruments <- account <- NULL

  list(
    getServer = function() server_type,
    switchServer = function()
      switch(server_type,
             practice = {
               server_type <<- "live"
               livestore <<- keystore <<- instruments <<- account <<- NULL
               message("Default OANDA server switched to 'live'")
             },
             live = {
               server_type <<- "practice"
               livestore <<- keystore <<- instruments <<- account <<- NULL
               message("Default OANDA server switched to 'practice'")
             }),
    getKey = function(server) {
      if (missing(server)) server <- server_type
      switch(server,
             practice = {
               if (is.null(keystore)) {
                 if (requireNamespace("keyring", quietly = TRUE)) {
                   apikey <- tryCatch(keyring::key_get(service = "OANDA_API_KEY"), error = function(e) {
                     message("No API key found for 'practice' account type\nPlease use oanda_set_key() to store your API key for automatic retrieval")
                     if (interactive()) readline("Please enter OANDA API key: ")
                   })
                 } else {
                   apikey <- if (interactive()) readline("Please enter OANDA API key: ")
                 }
                 keystore <<- apikey
               }
               invisible(keystore)
             },
             live = {
               if (is.null(livestore)) {
                 if (requireNamespace("keyring", quietly = TRUE)) {
                   apikey <- tryCatch(keyring::key_get(service = "OANDA_LIVE_KEY"), error = function(e) {
                     message("No API key found for 'live' account type\nPlease use oanda_set_key() to store your API key for automatic retrieval")
                     if (interactive()) readline("Please enter OANDA API key: ")
                   })
                 } else {
                   apikey <- if (interactive()) readline("Please enter OANDA API key: ")
                 }
                 livestore <<- apikey
               }
               invisible(livestore)
             })
    },
    getAccount = function(server, apikey) {
      if (is.null(account)) {
        server <- if (missing(server)) server_type else match.arg(server, c("practice", "live"))
        if (missing(apikey)) apikey <- do_$getKey(server = server)
        url <- switch(server,
                      practice = "https://api-fxpractice.oanda.com/v3/accounts",
                      live = "https://api-fxtrade.oanda.com/v3/accounts")
        resp <- ncurl(url, convert = FALSE, follow = TRUE,
                      headers = c("Authorization" = paste0("Bearer ", apikey),
                                  "User-Agent" = .user_agent))
        parsed <- deserialize_json(resp[["raw"]], simplify_to = 3L)
        length(parsed[["accounts"]]) || stop(parsed, call. = FALSE)
        account <<- parsed[["accounts"]][[1L]][["id"]]
      }
      invisible(account)
    },
    getInstruments = function(server, apikey) {
      if (is.null(instruments)) {
        server <- if (missing(server)) server_type else match.arg(server, c("practice", "live"))
        if (missing(apikey)) apikey <- do_$getKey(server = server)
        url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                      ".oanda.com/v3/accounts/", do_$getAccount(server = server, apikey = apikey),
                      "/instruments")
        resp <- ncurl(url, convert = FALSE, follow = TRUE,
                      headers = c("Authorization" = paste0("Bearer ", apikey),
                                  "User-Agent" = .user_agent))
        if (resp[["status"]] != 200L) {
          resp <- ncurl(url, convert = FALSE, follow = TRUE,
                        headers = c("Authorization" = paste0("Bearer ", apikey),
                                    "User-Agent" = .user_agent))
        }
        parsed <- deserialize_json(resp[["raw"]], simplify_to = 3L)
        length(parsed[["instruments"]]) || {
          warning(parsed,
                  "\nInstruments list could not be retrieved - falling back to internal data",
                  "\nCached instruments list will be used for the rest of the session", call. = FALSE)
          instruments <<- .oanda_instruments
          return(instruments)
        }
        vec <- unlist(parsed[["instruments"]])
        cnames <- attr(vec, "names")
        vec <- unname(vec)
        name <- vec[cnames == "name"]
        dispName <- vec[cnames == "displayName"]
        type <- vec[cnames == "type"]
        reorder <- order(name)
        df <- `attributes<-`(list(name[reorder], dispName[reorder], type[reorder]),
                             list(names = c("name", "displayName", "type"),
                                  class = "data.frame",
                                  row.names = .set_row_names(length(reorder)))
        )
        instruments <<- df
      }
      instruments
    }
  )

}
