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
oanda_switch <- function() {

  do_oanda$switchServer()

}

#' OANDA Internal Functions
#'
#' Encapsulates internal OANDA functions in a common environment.
#'
#' @return A list of closures.
#'
#' @noRd
#'
do_oanda <- function() {

  server_type <- "practice"
  keystore <- NULL
  account <- NULL
  instruments <- NULL

  list(
    getServer = function() {
      server_type
    },
    switchServer = function() {
    if (server_type == "practice") {
      server_type <<- "live"
      keystore <<- account <<- instruments <<- NULL
      message("Default OANDA server switched to 'live'")
    } else {
      server_type <<- "practice"
      keystore <<- account <<- instruments <<- NULL
      message("Default OANDA server switched to 'practice'")
    }
  },
  getKey = function() {
    if (is.null(keystore)) {
      if (requireNamespace("keyring", quietly = TRUE)) {
        actype <- switch(do_oanda$getServer(), practice = "OANDA_API_KEY", live = "OANDA_LIVE_KEY")
        apikey <- tryCatch(keyring::key_get(service = actype), error = function(e) {
          message("No API key found for account type '", do_oanda$getServer(),
                  "'.\nPlease use oanda_set_key() to store your API key for automatic retrieval")
          readline("Please enter OANDA API key: ")
        })
      } else {
        apikey <- readline("Please enter OANDA API key: ")
      }
      keystore <<- apikey
    }
    invisible(keystore)
  },
  getAccount = function(server, apikey) {
    if (is.null(account)) {
      if (missing(apikey)) apikey <- do_oanda$getKey()
      server <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
      url <- switch(server,
                    practice = "https://api-fxpractice.oanda.com/v3/accounts",
                    live = "https://api-fxtrade.oanda.com/v3/accounts")
      h <- new_handle()
      handle_setheaders(handle = h,
                        "Authorization" = paste0("Bearer ", apikey),
                        "User-Agent" = x_user_agent)
      resp <- curl_fetch_memory(url = url, handle = h)
      resp$status_code == 200L || stop("server code ", resp$status_code, " - ",
                                       parse_json(rawToChar(resp$content)), call. = FALSE)
      account <<- parse_json(rawToChar(resp$content))[["accounts"]][[1L]][["id"]]
    }
    invisible(account)
  },
  getInstruments = function(server, apikey) {
    if (is.null(instruments)) {
      if (missing(apikey)) apikey <- do_oanda$getKey()
      server <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
      url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                    ".oanda.com/v3/accounts/", do_oanda$getAccount(server = server, apikey = apikey),
                    "/instruments")
      h <- new_handle()
      handle_setheaders(handle = h,
                        "Authorization" = paste0("Bearer ", apikey),
                        "User-Agent" = x_user_agent)
      resp <- curl_fetch_memory(url = url, handle = h)
      resp$status_code == 200L || {
        warning("Server code ", resp$status_code, " - ",
                parse_json(rawToChar(resp$content)),
                "\nInstruments list could not be retrieved - falling back to internal data",
                "\nCached instruments list will be used for the rest of the session", call. = FALSE)
        instruments <<- x_oanda_instruments
        return(instruments)
      }
      vec <- unlist(parse_json(rawToChar(resp$content))[["instruments"]])
      cnames <- attr(vec, "names")
      vec <- unname(vec)
      name <- vec[cnames == "name"]
      dispName <- vec[cnames == "displayName"]
      type <- vec[cnames == "type"]
      reorder <- order(name)
      df <- list(name[reorder], dispName[reorder], type[reorder])
      attributes(df) <- list(names = c("name", "displayName", "type"),
                             class = "data.frame",
                             row.names = .set_row_names(length(reorder)))
      instruments <<- df
    }
    instruments
  }
  )

}

