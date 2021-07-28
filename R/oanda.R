# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' Get OANDA Price Data
#'
#' Retrieve price data from the OANDA fxTrade API.
#'
#' @param instrument string containing the base currency and quote currency
#'     delimited by a '_' (for example "USD_JPY"). Use the
#'     \code{\link{oanda_instruments}} function to return a list of all valid
#'     instruments.
#' @param granularity [default "D"] the granularity of the price data to fetch,
#'     one of "M", "W", "D", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30",
#'     "M15", "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5".
#' @param count (optional) the number of periods to return. The API supports a
#'     maximum of 5000 for each individual request, and defaults to 500 if not
#'     specified. 'count' should not be specified if both 'from' and 'to'
#'     arguments are provided, as the time range combined with 'granularity'
#'     will determine the number of periods to return.
#' @param from (optional) the start of the time range for which to fetch price
#'     data, for example "2020-02-01".
#' @param to (optional) the end of the time range for which to fetch price data,
#'     for example "2020-06-30".
#' @param price [default "M"] pricing component, one of "M" (midpoint), "B" (bid)
#'     or "A" (ask).
#' @param server [default "practice"] select either the "practice" or "live" server
#'     depending on the account type held with OANDA.
#' @param apikey (optional) string containing the OANDA fxTrade API key (personal
#'     access token), or function that returns this string. Does not need to be
#'     specified if already stored by oanda_set_key(). Can also be entered
#'     interactively if not specified.
#' @param ... additional parameters not used by this function.
#' @param .validate (optional) only used internally by other functions. Do not
#'     set this parameter.
#'
#' @return A data.frame containing the price data requested. Note that returned
#'     times are represented in UTC.
#'
#' @details This function queries the OANDA fxTrade API.
#'
#'     Requires an fxTrade account with OANDA \url{https://www.oanda.com/forex-trading/}.
#'     If you do not already hold a live account, you may register for an OANDA
#'     fxTrade practice / demo account. There is a link on your OANDA fxTrade
#'     account profile page 'Manage API Access' (My Account -> My Services ->
#'     Manage API Access). From there, a personal access token to use with the
#'     OANDA API can be generated, as well as revoked.
#'
#'     The \code{\link{oanda_set_key}} function can be used to store the API key
#'     in the default keyring so that it is automatically recognised in future
#'     (requires the 'keyring' package to be installed).
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#'     'OANDA' and 'fxTrade' are trademarks owned by OANDA Corporation, an
#'     entity unaffiliated with the ichimoku package.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' prices <- oanda("USD_JPY")
#' ichimoku(prices)
#'
#' oanda("EUR_JPY", granularity = "H1", count = 250, from = "2020-02-01", price = "B")
#' }
#'
#' @export
#'
oanda <- function(instrument,
                  granularity = c("D", "W", "M",
                                  "H12", "H8", "H6", "H4", "H3", "H2", "H1",
                                  "M30", "M15", "M10", "M5", "M4", "M2", "M1",
                                  "S30", "S15", "S10", "S5"),
                  count = NULL, from = NULL, to = NULL,
                  price = c("M", "B", "A"),
                  server = c("practice", "live"), apikey,
                  ..., .validate) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  server <- match.arg(server)
  if (missing(apikey)) apikey <- oanda_get_key()
  url <- switch(server,
                practice = paste0("https://api-fxpractice.oanda.com/v3/instruments/",
                                  instrument, "/candles"),
                live = paste0("https://api-fxtrade.oanda.com/v3/instruments/",
                              instrument, "/candles"))
  resp <- GET(url = url,
              add_headers(Authorization = paste0("Bearer ", apikey),
                          `Accept-Datetime-Format` = "RFC3339"),
              user_agent(ichimoku_user_agent),
              query = list(granularity = granularity, price = price,
                           count = count, from = from, to = to))
  data <- fromJSON(rawToChar(resp$content))
  if (resp$status_code != 200) stop("code ", resp$status_code, " - ", data$errorMessage, call. = FALSE)
  cdata <- data$candles
  time <- strptime(cdata[, 3L], format = "%Y-%m-%dT%H:%M:%S")
  if (!missing(.validate) && .validate == FALSE) {
    time <- as.POSIXct.POSIXlt(time)
  } else {
    if (granularity == "M") {
      time <- as.POSIXlt.POSIXct(time + 86400)
      time$mon <- time$mon + 1
    } else if (granularity == "D") {
        keep <- time$wday %in% 0:4
        if (missing(.validate)) {
          keep[time$mon == 11 & time$mday == 31 | (time$mon == 11 & time$mday == 24)] <- FALSE
        }
        cdata <- cdata[keep, ]
        time <- time[keep, ]
    } else if (missing(.validate)){
      cut <- time$wday == 6 | (time$wday == 0 & time$hour < 21)
      cdata <- cdata[!cut, ]
      time <- time[!cut, ]
    }

    periodicity <- switch(granularity,
                          M = -86400, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                          H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                          M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                          M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
    time <- time + periodicity
  }

  structure(list(time = time,
                 open = as.numeric(cdata[, 4L][[1L]]),
                 high = as.numeric(cdata[, 4L][[2L]]),
                 low = as.numeric(cdata[, 4L][[3L]]),
                 close = as.numeric(cdata[, 4L][[4L]]),
                 volume = cdata[, 2L],
                 complete = cdata[, 1L]),
            class = "data.frame",
            row.names = seq_len(dim(cdata)[1L]),
            instrument = instrument,
            price = price,
            timestamp = resp$date,
            oanda = TRUE)
}

#' Stream Live OANDA Price Data
#'
#' Retrieve live price and liquidity data from the OANDA fxTrade Streaming API.
#'
#' @inheritParams oanda
#'
#' @details This function connects to the OANDA fxTrade Streaming API. Use the
#'     'Esc' key to stop the stream.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @return Does not return a value, however the streaming data is output to the
#'     console as a side effect. Note that returned times are represented in UTC.
#'
#' @section Streaming Data:
#'
#'     Get a stream of Account Prices starting from when the request is made.
#'     This pricing stream does not include every single price created for the
#'     Account, but instead will provide at most 4 prices per second (every
#'     250 milliseconds) for each instrument being requested. If more than one
#'     price is created for an instrument during the 250 millisecond window,
#'     only the price in effect at the end of the window is sent. This means
#'     that during periods of rapid price movement, subscribers to this stream
#'     will not be sent every price. Pricing windows for different connections
#'     to the price stream are not all aligned in the same way (i.e. they are
#'     not all aligned to the top of the second). This means that during
#'     periods of rapid price movement, different subscribers may observe
#'     different prices depending on their alignment.
#'
#'     The streamed data is formatted for easier reading, but otherwise
#'     represents the raw feed without omission.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_stream("USD_JPY")
#' }
#'
#' @export
#'
oanda_stream <- function(instrument, server = c("practice", "live"), apikey) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  if (missing(apikey)) apikey <- oanda_get_key()
  server <- match.arg(server)
  url <- switch(server,
                practice = paste0("https://stream-fxpractice.oanda.com/v3/accounts/",
                                  oanda_accounts()[[1L]], "/pricing/stream"),
                live = paste0("https://stream-fxtrade.oanda.com/v3/accounts",
                              oanda_accounts()[[1L]], "/pricing/stream"))
  message("Streaming data... Press 'Esc' to return")
  GET(url = url,
      add_headers(Authorization = paste0("Bearer ", apikey),
                  `Accept-Datetime-Format` = "RFC3339"),
      user_agent(ichimoku_user_agent),
      query = list(instruments = instrument),
      write_stream(function(x) {
        stream <- sub("close", "\033[49m\033[39m\nclose",
                      sub("asks:", "\033[49m\033[39m asks: \033[90m\033[42m",
                          sub("bids:", "\nbids: \033[37m\033[44m",
                              gsub(",", "  ",
                                   gsub('"|{|}|\\[|\\]', "", rawToChar(x), perl = TRUE),
                                   fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
        cat(stream)
      }))
}

#' Live Ichimoku Cloud Charts from OANDA Data
#'
#' Live updating Ichimoku Kinko Hyo cloud charts using OANDA fxTrade API data.
#'
#' @inheritParams oanda
#' @inheritParams ichimoku
#' @param refresh [default 5] data refresh interval in seconds, with a minimum of 1.
#' @param count [default 250] the number of periods to return. The API supports
#'     a maximum of 5000. Note that 78 fewer periods are actually shown on the
#'     chart to ensure a full cloud is always displayed.
#' @param theme [default 'original'] chart theme with alternative choices of
#'     'dark', 'solarized' or 'mono'.
#' @param ... additional arguments passed along to \code{\link{ichimoku}} for
#'     calculating the ichimoku cloud.
#'
#' @return Does not return a value, however a plot of the ichimoku chart for the
#'     price data requested is output to the graphical device at each refresh
#'     interval as a side effect. Note that returned times are represented in UTC.
#'
#' @details This function polls the OANDA fxTrade API for the latest live prices
#'     and updates the plot in the graphical device at each refresh interval.
#'     Use the 'Esc' key to stop updating.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_chart("USD_JPY")
#' oanda_chart("EUR_JPY", granularity = "H1", refresh = 3, count = 300, price = "B", theme = "mono")
#' }
#'
#' @export
#'
oanda_chart <- function(instrument,
                        granularity = c("D", "W", "M",
                                        "H12", "H8", "H6", "H4", "H3", "H2", "H1",
                                        "M30", "M15", "M10", "M5", "M4", "M2", "M1",
                                        "S30", "S15", "S10", "S5"),
                        refresh = 5,
                        count = 250, price = c("M", "B", "A"),
                        theme = c("original", "dark", "solarized", "mono"),
                        server = c("practice", "live"), apikey, ...,
                        periods = c(9L, 26L, 52L)) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  if (missing(apikey)) apikey <- oanda_get_key()
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  theme <- match.arg(theme)
  server <- match.arg(server)
  px <- switch(price, M = "mid", B = "bid", A = "ask")
  if (!is.numeric(refresh) || refresh < 1) {
    message("Invalid refresh interval '", refresh, "' secs specified - using default of 5 secs instead")
    refresh <- 5
  }
  if (!is.numeric(periods) || !length(periods) == 3 || !all(periods > 0)) {
    warning("Invalid cloud periods specified - using defaults c(9L, 26L, 52L) instead",
            call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p2 <- periods[2]
  minlen <- p2 + periods[3]
  if (!is.numeric(count) || count <= minlen) {
    message("Invalid count specified - using default of 250 instead")
    count <- 250
  }
  ins <- oanda_instruments(server = server, apikey = apikey)
  ticker <- ins$displayName[ins$name %in% instrument]
  periodicity <- switch(granularity,
                        M = 2419200, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                        H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                        M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                        M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
  data <- oanda(instrument = instrument, granularity = granularity, count = count,
                price = price, server = server, apikey = apikey, .validate = TRUE)
  xlen <- dim(data)[1L]
  message("Chart updating every ", refresh, " secs in graphical device... Press 'Esc' to return")
  while (TRUE) {
    pdata <- ichimoku.data.frame(data, periods = periods, ...)[minlen:(xlen + p2), ]
    message <- paste(instrument, px, "price [", data$close[xlen], "] at",
                     attr(data, "timestamp"), "| Chart:",
                     switch(granularity, M = "Monthly", W = "Weekly", D = "Daily",
                            H12 = "12 Hour", H8 = "8 Hour", H6 = "6 Hour", H4 = "4 Hour",
                            H3 = "3 Hour", H2 = "2 Hour", H1 = "1 Hour", M30 = "30 Mins",
                            M15 = "15 Mins", M10 = "10 Mins", M5 = "5 Mins", M4 = "4 Mins",
                            M2 = "1 Mins", M1 = "1 Min", S30 = "30 Secs", S15 = "15 Secs",
                            S10 = "10 Secs", S5 = "5 Secs"),
                     "| Cmplt:", data$complete[xlen])
    plot.ichimoku(pdata, ticker = ticker, message = message, theme = theme, newpage = FALSE)
    Sys.sleep(refresh)
    newdata <- oanda(instrument = instrument, granularity = granularity,
                     count = ceiling(refresh / periodicity) + 1,
                     price = price, server = server, apikey = apikey)
    data <- df_append(new = newdata, old = data)
    dlen <- dim(data)[1L]
    if (dlen > xlen) data <- data[(dlen - xlen + 1):dlen, ]
  }
}

#' Get OANDA Instruments
#'
#' Return list of instruments including major currencies, metals, commodities,
#'     government bonds and stock indices for which pricing data is available
#'     from the OANDA fxTrade API.
#'
#' @inheritParams oanda
#'
#' @return A data.frame containing the instrument name, type, and full display name.
#'
#' @details This function returns a data.frame listing the instrument names
#'     available for an account associated with the supplied OANDA fxTrade API key.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_instruments()
#' }
#'
#' @export
#'
oanda_instruments <- function(server = c("practice", "live"), apikey) {

  if (missing(apikey)) apikey <- oanda_get_key()
  server <- match.arg(server)
  url <- switch(server,
                practice = paste0("https://api-fxpractice.oanda.com/v3/accounts/",
                                  oanda_accounts()[[1L]], "/instruments"),
                live = paste0("https://api-fxtrade.oanda.com/v3/accounts/",
                              oanda_accounts()[[1L]], "/instruments"))
  resp <- GET(url = url,
              add_headers(Authorization = paste0("Bearer ", apikey)),
              user_agent(ichimoku_user_agent))
  data <- fromJSON(rawToChar(resp$content))
  if (resp$status_code != 200) stop("code ", resp$status_code, " - ", data$errorMessage, call. = FALSE)
  df <- data$instruments[order(data$instruments[, 1]), 1:3]
  df
}

#' Accounts for an OANDA fxTrade API Key
#'
#' Return list of accounts authorised to be accessed by an OANDA fxTrade API key
#'     (personal access token). Used by other OANDA functions to access API
#'     endpoints that require an account ID.
#'
#' @inheritParams oanda
#'
#' @return A data.frame containing the list of accounts the authorization bearer
#'     token is authorized to access and their associated properties.
#'
#' @details This function queries the OANDA fxTrade API for account details
#'     associated with the supplied API key.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example:
#' oanda_accounts()
#' }
#'
#' @export
#'
oanda_accounts <- function(server = c("practice", "live"), apikey) {

  if (missing(apikey)) apikey <- oanda_get_key()
  server <- match.arg(server)
  url <- switch(server,
                practice = "https://api-fxpractice.oanda.com/v3/accounts",
                live = "https://api-fxtrade.oanda.com/v3/accounts")
  resp <- GET(url = url,
              add_headers(Authorization = paste0("Bearer ", apikey)),
              user_agent(ichimoku_user_agent))
  data <- fromJSON(rawToChar(resp$content))
  if (resp$status_code != 200) stop("code ", resp$status_code, " - ", data$errorMessage, call. = FALSE)
  df <- data$accounts
  df
}

#' Set OANDA fxTrade API Key
#'
#' Store OANDA fxTrade API key (personal access token) in the keyring.
#'
#' @return A key is set in the default keyring under the service name 'OANDA_API_KEY'.
#'
#' @details The key is read interactively.
#'
#'     This function only needs to be run once to set the key, it does not need to
#'     be run each session.
#'
#'     This function has a dependency on the 'keyring' package.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' if (interactive()) {
#' # Only run example in interactive R sessions
#' oanda_set_key()
#' }
#'
#' @export
#'
oanda_set_key <- function() {
  if (requireNamespace("keyring", quietly = TRUE)) {
    keyring::key_set(service = "OANDA_API_KEY")
  } else {
    message("Note: please install the 'keyring' package in order to store your OANDA API key")
  }
}

#' Get OANDA fxTrade API Key
#'
#' Return OANDA fxTrade API key (personal access token) stored in the keyring,
#'     or else prompts the user to provide a key interactively.
#'
#' @return A character string, respresenting the key stored in the default
#'     keyring under the service name 'OANDA_API_KEY' if present, otherwise the
#'     key supplied by the user interactively.
#'
#' @details For retrieving the key from the keyring, this function has a dependency
#'     on the 'keyring' package, otherwise it can be used to read a key supplied
#'     interactively.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' if (interactive()) {
#' # Only run example in interactive R sessions
#' oanda_get_key()
#' }
#'
#' @export
#'
oanda_get_key <- function() {
  if (requireNamespace("keyring", quietly = TRUE)) {
    apikey <- tryCatch(keyring::key_get(service = "OANDA_API_KEY"), error = function(e) {
      message("Note: oanda_set_key() can be used to store your API key for automatic retrieval")
      readline("Please enter OANDA API key: ")
    })
  } else {
    apikey <- readline("Please enter OANDA API key: ")
  }
  apikey
}

#' Interactive Live Analysis Environment for OANDA Data
#'
#' Dynamically-generated Ichimoku Kinko Hyo cloud charts using OANDA fxTrade API
#'     data in a completely customisable Shiny environment.
#'
#' @inheritParams oanda_chart
#' @inheritParams iplot
#' @param instrument [default 'USD_JPY'] string containing the base currency and
#'     quote currency delimited by a '_'. Use the \code{\link{oanda_instruments}}
#'     function to return a list of all valid instruments.
#' @param count [default 300] the number of periods to return, from 100 to 800.
#'     Note that 78 fewer periods are actually shown on the chart to ensure a
#'     full cloud is always displayed.
#' @param ... additional arguments passed along to \code{\link{ichimoku}} for
#'     calculating the ichimoku cloud, \code{\link{autoplot}} to set chart
#'     parameters, or the 'options' argument of \code{shiny::shinyApp()}.
#'
#' @return Returns a Shiny app object with class 'shiny.appobj'. Note that
#'     returned times within the app are represented in UTC.
#'
#' @details This function polls the OANDA fxTrade API for the latest prices and
#'     updates a customisable reactive Shiny app at each refresh interval.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_studio()
#'
#' # To open in RStudio viewer instead of default browser
#' oanda_studio(launch.browser = getOption("shiny.launch.browser"))
#' }
#'
#' @export
#'
oanda_studio <- function(instrument = "USD_JPY",
                         granularity = c("D", "W", "M",
                                         "H12", "H8", "H6", "H4", "H3", "H2", "H1",
                                         "M30", "M15", "M10", "M5", "M4", "M2", "M1",
                                         "S30", "S15", "S10", "S5"),
                         refresh = 5, count = 300, price = c("M", "B", "A"),
                         theme = c("original", "dark", "solarized", "mono"),
                         server = c("practice", "live"), apikey, ...,
                         launch.browser = TRUE,
                         periods = c(9L, 26L, 52L)) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    if (missing(apikey)) apikey <- oanda_get_key()
    if (!is.numeric(refresh) || refresh < 1) {
      message("Invalid refresh interval '", refresh, "' secs specified - using default of 5 secs instead")
      refresh <- 5
    }
    granularity <- match.arg(granularity)
    price <- match.arg(price)
    theme <- match.arg(theme)
    srv <- match.arg(server)
    if (!is.numeric(periods) || !length(periods) == 3 || !all(periods > 0)) {
      warning("Specified cloud periods invalid - using defaults c(9L, 26L, 52L) instead",
              call. = FALSE)
      periods <- c(9L, 26L, 52L)
    }
    p2 <- periods[2]
    minlen <- p2 + periods[3]
    ins <- oanda_instruments(server = server, apikey = apikey)

    ui <- shiny::fluidPage(
      shiny::fillPage(
        padding = 20,
        shiny::tags$style(type = "text/css", "#chart {height: calc(100vh - 150px) !important;}"),
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover",
                                                   delay = 80, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("infotip")
      ),
      shiny::fluidRow(
        shiny::column(width = 2,
                      shiny::HTML("&nbsp;")
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 2,
                      shiny::selectInput("theme", label = "Theme",
                                         choices = c("original", "dark", "solarized", "mono"),
                                         selected = theme,
                                         selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("instrument", label = "Instrument",
                                         choices = ins$name,
                                         selected = instrument,
                                         selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::selectInput("granularity", label = "Granularity",
                                         choices = c("M", "W", "D",
                                                     "H12", "H8", "H6", "H4", "H3", "H2", "H1",
                                                     "M30", "M15", "M10", "M5", "M4", "M2", "M1",
                                                     "S30", "S15", "S10", "S5"),
                                         selected = granularity,
                                         selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::selectInput("price", label = "Price",
                                         choices = c("M", "B", "A"),
                                         selected = price,
                                         selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::numericInput("refresh", label = "Refresh",
                                          value = refresh, min = 1, max = 86400)),
        shiny::column(width = 4,
                      shiny::sliderInput("count", label = "Data Periods",
                                         min = 100, max = 800,
                                         value = count,
                                         width = "100%")),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>Show</label>"),
                      shiny::checkboxInput("infotip", "Infotip", value = TRUE))
      )
    )

    server <- function(input, output, session) {

      periodicity <- shiny::reactive(
        switch(input$granularity,
               M = 2419200, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
               H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
               M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
               M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
      )
      idata <- shiny::reactive(oanda(instrument = input$instrument,
                                     granularity = input$granularity,
                                     count = input$count,
                                     price = input$price,
                                     server = srv, apikey = apikey, .validate = TRUE))
      left_px <- shiny::reactive(input$plot_hover$coords_css$x)
      top_px <- shiny::reactive(input$plot_hover$coords_css$y)
      posi_x <- shiny::reactive(round(input$plot_hover$x, digits = 0))

      ticker <- shiny::reactive(
        paste(ins$displayName[ins$name %in% input$instrument], "  |",
              input$instrument, switch(input$price, M = "mid", B = "bid", A = "ask"),
              "price [", data()$close[xlen()], "] at",
              attr(data(), "timestamp"), "| Chart:",
              switch(input$granularity, M = "Monthly", W = "Weekly", D = "Daily",
                     H12 = "12 Hour", H8 = "8 Hour", H6 = "6 Hour", H4 = "4 Hour",
                     H3 = "3 Hour", H2 = "2 Hour", H1 = "1 Hour", M30 = "30 Mins",
                     M15 = "15 Mins", M10 = "10 Mins", M5 = "5 Mins", M4 = "4 Mins",
                     M2 = "1 Mins", M1 = "1 Min", S30 = "30 Secs", S15 = "15 Secs",
                     S10 = "10 Secs", S5 = "5 Secs"),
              "| Cmplt:", data()$complete[xlen()]))

      newdata <- shiny::reactivePoll(
        intervalMillis = shiny::reactive({
          shiny::req(input$refresh >= 1)
          input$refresh * 1000
        }),
        session = session,
        checkFunc = function() Sys.time(),
        valueFunc = function() {
          shiny::req(input$refresh >= 1)
          oanda(instrument = input$instrument,
                granularity = input$granularity,
                count = ceiling(input$refresh / periodicity()) + 1,
                price = input$price,
                server = srv, apikey = apikey, .validate = TRUE)
        }
      )

      datastore <- shiny::reactiveVal(shiny::isolate(idata()))
      shiny::observeEvent(newdata(), {
        if (unclass(attr(idata(), "timestamp")) > unclass(attr(datastore(), "timestamp"))) {
          df <- df_append(new = newdata(), old = idata())
          dlen <- dim(df)[1L]
          if (dlen > input$count) df <- df[(dlen - input$count + 1L):dlen, ]
          datastore(df)
        } else {
          df <- df_append(new = newdata(), old = datastore())
          dlen <- dim(df)[1L]
          if (dlen > input$count) df <- df[(dlen - input$count + 1L):dlen, ]
          datastore(df)
        }
      }, domain = session)
      data <- shiny::reactive({
        if (unclass(attr(datastore(), "timestamp")) > unclass(attr(idata(), "timestamp"))) datastore()
        else idata()
      })
      xlen <- shiny::reactive(dim(data())[1L])
      pdata <- shiny::reactive(ichimoku(data(), periods = periods, ...)[minlen:(xlen() + p2), ])

      output$chart <- shiny::renderPlot(
        autoplot.ichimoku(pdata(), ticker = ticker(), theme = input$theme, ...)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawGuide(label = index(pdata())[posi_x()], left = left_px() - 17, top = 45)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawInfotip(sdata = pdata()[posi_x(), ], left_px = left_px(), top_px = top_px())
      })

      session$onSessionEnded(function() shiny::stopApp())
    }

    shiny::shinyApp(ui, server, options = list(launch.browser = launch.browser, ...))

  } else {
    message("Note: please install the 'shiny' package to enable oanda_studio()")
  }
}

