# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' Get OANDA Price Data
#'
#' Retrieve price data for major currencies, metals, commodities, government
#'     bonds and stock indices from the OANDA fxTrade API.
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
#'     specified. If both 'from' and 'to' are specified, 'count' is ignored, as
#'     the time range combined with 'granularity' will determine the number of
#'     periods to return.
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
#'
#' @return A data.frame containing the price data requested.
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
#'     The \code{\link{oanda_set_key}} function can be used to save the API key
#'     in the system credential store so that it is automatically recognised in
#'     future (requires the 'keyring' package to be installed).
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
                  count = NULL,
                  from = NULL,
                  to = NULL,
                  price = c("M", "B", "A"),
                  server = c("practice", "live"),
                  apikey) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  server <- match.arg(server)
  if (missing(apikey)) apikey <- oanda_get_key()

  if (!missing(from) && !missing(to)) {
    d1 <- tryCatch(as.POSIXct(from), error = function(e) {
      stop("Specified value of 'from' is not convertible to a POSIXct date-time format", call. = FALSE)})
    d2 <- tryCatch(as.POSIXct(to), error = function(e) {
      stop("Specified value of 'to' is not convertible to a POSIXct date-time format", call. = FALSE)})
    interval <- unclass(d2) - unclass(d1)
    if (interval < 0) stop("Requested time period invalid - 'to' takes place before 'from'", call. = FALSE)
    denom <- switch(granularity,
                    M = 18144000, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                    H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                    M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                    M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
    requests <- ceiling(interval / denom / 5000)

    if (requests == 1) {
      return(getPrices(instrument = instrument, granularity = granularity,
                       from = strftime(d1, format = "%Y-%m-%dT%H:%M:%S"),
                       to = strftime(d2, format = "%Y-%m-%dT%H:%M:%S"),
                       price = price, server = server, apikey = apikey))

      } else {
      bounds <- lapply(0:requests, function(x) d1 + interval * x / requests)
      first <- bounds[1:requests]
      second <- bounds[2:(requests + 1)]
      continue <- readline(prompt = paste0("Max of 5000 data periods per request. ",
                                           requests, " requests will be made. Continue? [y/N] "))
      if (!continue %in% c("y", "Y", "yes", "YES")) stop("Request cancelled by user", call. = FALSE)
      message("Request started with rate limiting in place >>>")
      list <- lapply(1:requests, function(i) {
        data <- getPrices(instrument = instrument, granularity = granularity,
                          from = strftime(first[[i]], format = "%Y-%m-%dT%H:%M:%S"),
                          to = strftime(second[[i]], format = "%Y-%m-%dT%H:%M:%S"),
                          price = price, server = server, apikey = apikey)
        message("Downloaded data partition ", i, " of ", requests)
        if (i != requests) Sys.sleep(1)
        data
      })
      message("Merging data partitions...")
      df <- do.call(df_merge, list)
      message("Complete.")
      }

    } else {
      if (!missing(from)) {
        from <- tryCatch(as.POSIXct(from), error = function(e) {
          stop("Specified value of 'from' is not convertible to a POSIXct date-time format",
               call. = FALSE)})
        from <- strftime(from, format = "%Y-%m-%dT%H:%M:%S")
      }
      if (!missing(to)) {
        to <- tryCatch(as.POSIXct(to), error = function(e) {
          stop("Specified value of 'to' is not convertible to a POSIXct date-time format",
               call. = FALSE)})
        to <- strftime(to, format = "%Y-%m-%dT%H:%M:%S")
      }

      df <- getPrices(instrument = instrument, granularity = granularity, count = count,
                      from = from, to = to, price = price, server = server, apikey = apikey)
    }

  df

}

#' getPrices
#'
#' Internal function used by ichimoku to retrieve price candles from the OANDA
#'     fxTrade REST API.
#'
#' @param instrument string containing the base currency and quote currency
#'     delimited by a '_' (for example "USD_JPY").
#' @param granularity the granularity of the price data to fetch,
#'     one of "M", "W", "D", "H12", "H8", "H6", "H4", "H3", "H2", "H1", "M30",
#'     "M15", "M10", "M5", "M4", "M2", "M1", "S30", "S15", "S10", "S5".
#' @param count (optional) the number of periods to return. The API supports a
#'     maximum of 5000 for each individual request, and defaults to 500 if not
#'     specified.
#' @param from (optional) the start of the time range for which to fetch price
#'     data, for example "2020-02-01".
#' @param to (optional) the end of the time range for which to fetch price data,
#'     for example "2020-06-30".
#' @param price pricing component, one of "M" (midpoint), "B" (bid)
#'     or "A" (ask).
#' @param server select either the "practice" or "live" server
#'     depending on the account type held with OANDA.
#' @param apikey (optional) string containing the OANDA fxTrade API key (personal
#'     access token), or function that returns this string.
#' @param .validate (optional) only used internally by other functions. Do not
#'     set this parameter.
#'
#' @return A data.frame containing the price data requested.
#'
#' @keywords internal
#'
getPrices <- function(instrument, granularity, count, from, to, price,
                      server, apikey, .validate) {

  url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/instruments/", instrument, "/candles?granularity=",
                granularity, "&price=", price,
                if (!missing(count) && !is.null(count)) paste0("&count=", count),
                if (!missing(from) && !is.null(from)) paste0("&from=", from),
                if (!missing(to) && !is.null(to)) paste0("&to=", to))
  h <- new_handle()
  handle_setheaders(handle = h,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "RFC3339",
                    "User-Agent" = ichimoku_user_agent)
  resp <- curl_fetch_memory(url = url, handle = h)

  if (resp$status_code != 200L) stop("code ", resp$status_code, " - ",
                                     fromJSON(rawToChar(resp$content)), call. = FALSE)
  headers <- rawToChar(resp$headers)
  hdate <- strsplit(headers, "date: | GMT", perl = TRUE)[[1]][2]
  timestamp <- as.POSIXct.POSIXlt(strptime(hdate, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  data <- fromJSON(rawToChar(resp$content))$candles

  time <- strptime(data[, 3L], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  if (!missing(.validate) && .validate == FALSE) {
    time <- as.POSIXct.POSIXlt(time)
  } else {
    if (granularity == "M") {
      time <- as.POSIXlt.POSIXct(time + 86400)
      time$mon <- time$mon + 1L
    } else if (granularity == "W") {
      time <- as.POSIXct.POSIXlt(time)
    } else if (granularity == "D") {
      keep <- time$wday %in% 0:4
      if (missing(.validate)) {
        keep[time$mon == 11L & time$mday == 31L | (time$mon == 11L & time$mday == 24L)] <- FALSE
      }
      data <- data[keep, ]
      time <- time[keep, ]
    } else if (missing(.validate)){
      cut <- (time$wday == 5L & time$hour > 20L) | time$wday == 6L | (time$wday == 0L & time$hour < 21L)
      data <- data[!cut, ]
      time <- time[!cut, ]
    }
    periodicity <- switch(granularity,
                          M = -86400, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                          H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                          M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                          M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
    time <- time + periodicity
  }

  ohlc <- data[, 4L]

  structure(list(time = .POSIXct(time),
                 open = ohlc[, 1L],
                 high = ohlc[, 2L],
                 low = ohlc[, 3L],
                 close = ohlc[, 4L],
                 volume = data[, 2L],
                 complete = data[, 1L]),
            class = "data.frame",
            row.names = seq_len(dim(ohlc)[1L]),
            instrument = instrument,
            price = price,
            timestamp = .POSIXct(timestamp),
            oanda = TRUE)

}

#' Stream Live OANDA Price Data
#'
#' Retrieve live price and liquidity data for major currencies, metals,
#'     commodities, government bonds and stock indices from the OANDA fxTrade
#'     Streaming API.
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
#'     console as a side effect. Note that as this is a raw stream, returned
#'     times are in UTC.
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
  url <- paste0("https://stream-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/accounts/", oandaAccount(),
                "/pricing/stream?instruments=", instrument)
  h <- new_handle()
  handle_setheaders(handle = h,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "RFC3339",
                    "User-Agent" = ichimoku_user_agent)

  message("Streaming data... Press 'Esc' to return")
  curl_fetch_stream(url = url, handle = h, fun = function(x) {
    stream <- sub("close", "\033[49m\033[39m\nclose",
                  sub("asks:", "\033[49m\033[39m asks: \033[90m\033[42m",
                      sub("bids:", "\nbids: \033[37m\033[44m",
                          gsub(",", "  ",
                               gsub('"|{|}|\\[|\\]', "", rawToChar(x), perl = TRUE),
                               fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
    cat(stream)
  })
}

#' Live Ichimoku Cloud Charts from OANDA Data
#'
#' Live updating Ichimoku Kinko Hyo cloud charts for major currencies, metals,
#'     commodities, government bonds and stock indices using OANDA fxTrade API
#'     data.
#'
#' @inheritParams oanda
#' @inheritParams ichimoku
#' @param refresh [default 5] data refresh interval in seconds, with a minimum
#'     of 1.
#' @param count [default 250] the number of periods to return. The API supports
#'     a maximum of 5000. Note that fewer periods are actually shown on the
#'     chart to ensure a full cloud is always displayed.
#' @param theme [default 'original'] chart theme with alternative choices of
#'     'dark', 'solarized' or 'mono'.
#' @param ... additional arguments passed along to \code{\link{ichimoku}} for
#'     calculating the ichimoku cloud or \code{\link{autoplot}} to set chart
#'     parameters.
#'
#' @return Does not return a value, however a plot of the ichimoku chart for the
#'     price data requested is output to the graphical device at each refresh
#'     interval as a side effect.
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
                        count = 250,
                        price = c("M", "B", "A"),
                        theme = c("original", "dark", "solarized", "mono"),
                        server = c("practice", "live"),
                        apikey,
                        ...,
                        periods = c(9L, 26L, 52L)) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  if (missing(apikey)) apikey <- oanda_get_key()
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  theme <- match.arg(theme)
  server <- match.arg(server)
  if (!is.numeric(refresh) || refresh < 1) {
    message("Invalid refresh interval '", refresh, "' secs specified - using default of 5 secs instead")
    refresh <- 5
  }
  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - using defaults c(9L, 26L, 52L) instead",
            call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p2 <- periods[2L]
  minlen <- p2 + periods[3L]
  if (!is.numeric(count) || count < minlen) {
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
  ctype <- switch(granularity, M = "Monthly", W = "Weekly", D = "Daily",
                  H12 = "12 Hour", H8 = "8 Hour", H6 = "6 Hour", H4 = "4 Hour",
                  H3 = "3 Hour", H2 = "2 Hour", H1 = "1 Hour", M30 = "30 Mins",
                  M15 = "15 Mins", M10 = "10 Mins", M5 = "5 Mins", M4 = "4 Mins",
                  M2 = "1 Mins", M1 = "1 Min", S30 = "30 Secs", S15 = "15 Secs",
                  S10 = "10 Secs", S5 = "5 Secs")
  ptype <- switch(price, M = "mid", B = "bid", A = "ask")

  data <- getPrices(instrument = instrument, granularity = granularity, count = count,
                    price = price, server = server, apikey = apikey, .validate = TRUE)
  xlen <- dim(data)[1L]

  message("Chart updating every ", refresh, " secs in graphical device... Press 'Esc' to return")
  while (TRUE) {
    pdata <- ichimoku.data.frame(data, periods = periods, ...)[minlen:(xlen + p2 - 1L), ]
    message <- paste(instrument, ptype, "price [",
                     data$close[xlen], "] at", attr(data, "timestamp"),
                     "| Chart:", ctype, "| Cmplt:", data$complete[xlen])
    plot.ichimoku(pdata, ticker = ticker, message = message, theme = theme,
                  newpage = FALSE, ...)
    Sys.sleep(refresh)
    newdata <- getPrices(instrument = instrument, granularity = granularity,
                         count = ceiling(refresh / periodicity) + 1,
                         price = price, server = server, apikey = apikey,
                         .validate = TRUE)
    data <- df_append(new = newdata, old = data)
    dlen <- dim(data)[1L]
    if (dlen > xlen) data <- data[(dlen - xlen + 1L):dlen, ]
  }
}

#' Interactive Live Analysis Environment for OANDA Data
#'
#' Dynamically-generated Ichimoku Kinko Hyo cloud charts for major currencies,
#'     metals, commodities, government bonds and stock indices using OANDA
#'     fxTrade API data in a fully-customisable and interactive R Shiny
#'     environment. Intuitive cursor infotip provides ready access to the data
#'     directly from the chart.
#'
#' @inheritParams oanda_chart
#' @inheritParams iplot
#' @param instrument [default 'USD_JPY'] string containing the base currency and
#'     quote currency delimited by a '_'. Use the \code{\link{oanda_instruments}}
#'     function to return a list of all valid instruments.
#' @param count [default 300] the number of periods to return, from 100 to 800.
#'     Note that fewer periods are actually shown on the chart to ensure a full
#'     cloud is always displayed.
#' @param ... additional arguments passed along to \code{\link{ichimoku}} for
#'     calculating the ichimoku cloud, \code{\link{autoplot}} to set chart
#'     parameters, or the 'options' argument of \code{shiny::shinyApp()}.
#'
#' @return Returns a Shiny app object with class 'shiny.appobj'.
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
                         refresh = 5,
                         count = 300,
                         price = c("M", "B", "A"),
                         theme = c("original", "dark", "solarized", "mono"),
                         server = c("practice", "live"),
                         apikey,
                         ...,
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
    srvr <- match.arg(server)
    if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
      periods <- as.integer(periods)
    } else {
      warning("Specified cloud periods invalid - using defaults c(9L, 26L, 52L) instead",
              call. = FALSE)
      periods <- c(9L, 26L, 52L)
    }
    p2 <- periods[2L]
    minlen <- p2 + periods[3L]
    if (!is.numeric(count) || count <= minlen) {
      message("Invalid count specified - using default of 300 instead")
      count <- 300
    }

    ins <- oanda_instruments(server = srvr, apikey = apikey)
    ichimoku_stheme <- if (requireNamespace("bslib", quietly = TRUE)) {
      bslib::bs_theme(version = 4, bootswatch = "solar", bg = "#ffffff", fg = "#002b36",
                      primary = "#073642", font_scale = 0.85)
    }

    ui <- shiny::fluidPage(
      theme = ichimoku_stheme,
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

      idata <- shiny::reactive(getPrices(instrument = input$instrument,
                                         granularity = input$granularity,
                                         count = input$count,
                                         price = input$price,
                                         server = srvr,
                                         apikey = apikey,
                                         .validate = TRUE))

      datastore <- shiny::reactiveVal(shiny::isolate(idata()))
      left_px <- shiny::reactive(input$plot_hover$coords_css$x)
      top_px <- shiny::reactive(input$plot_hover$coords_css$y)
      posi_x <- shiny::reactive(round(input$plot_hover$x, digits = 0))

      periodicity <- shiny::reactive(
        switch(input$granularity,
               M = 2419200, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
               H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
               M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
               M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
      )
      ctype <- shiny::reactive(
        switch(input$granularity,
               M = "Monthly", W = "Weekly", D = "Daily", H12 = "12 Hour", H8 = "8 Hour",
               H6 = "6 Hour", H4 = "4 Hour", H3 = "3 Hour", H2 = "2 Hour",
               H1 = "1 Hour", M30 = "30 Mins", M15 = "15 Mins", M10 = "10 Mins",
               M5 = "5 Mins", M4 = "4 Mins", M2 = "1 Mins", M1 = "1 Min",
               S30 = "30 Secs", S15 = "15 Secs", S10 = "10 Secs", S5 = "5 Secs")
      )
      ptype <- shiny::reactive(switch(input$price, M = "mid", B = "bid", A = "ask"))
      dispname <- shiny::reactive(ins$displayName[ins$name %in% input$instrument])

      newdata <- shiny::reactive({
        shiny::req(input$refresh >= 1)
        shiny::invalidateLater(millis = input$refresh * 1000, session = session)
        getPrices(instrument = input$instrument,
                  granularity = input$granularity,
                  count = ceiling(input$refresh / periodicity()) + 1,
                  price = input$price,
                  server = srvr,
                  apikey = apikey,
                  .validate = TRUE)
      })

      shiny::observeEvent(newdata(), {

        if (unclass(attr(datastore(), "timestamp")) > unclass(attr(idata(), "timestamp"))) {
          df <- df_append(new = newdata(), old = datastore())
          dlen <- dim(df)[1L]
          if (dlen > input$count) df <- df[(dlen - input$count + 1L):dlen, ]
          datastore(df)

        } else {
          df <- df_append(new = newdata(), old = idata())
          dlen <- dim(df)[1L]
          if (dlen > input$count) df <- df[(dlen - input$count + 1L):dlen, ]
          datastore(df)
        }

      })

      data <- shiny::reactive({
        if (unclass(attr(datastore(), "timestamp")) > unclass(attr(idata(), "timestamp"))) {
          datastore()
        } else idata()
      })
      xlen <- shiny::reactive(dim(data())[1L])
      pdata <- shiny::reactive(ichimoku(data(), periods = periods, ...)[minlen:(xlen() + p2 - 1L), ])
      ticker <- shiny::reactive(paste(dispname(), "  |", input$instrument, ptype(), "price [",
                                      data()$close[xlen()], "] at", attr(data(), "timestamp"),
                                      "| Chart:", ctype(), "| Cmplt:", data()$complete[xlen()]))

      if (requireNamespace("bslib", quietly = TRUE)) {
        shiny::observe({
          session$setCurrentTheme(
            bslib::bs_theme_update(ichimoku_stheme,
                                   bootswatch = switch(input$theme, dark = "solar", NULL)))
        })
      }

      output$chart <- shiny::renderPlot(
        autoplot.ichimoku(pdata(), ticker = ticker(), theme = input$theme, ...)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawGuide(label = index(pdata())[posi_x()], left = left_px() - 17, top = 45)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5L), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= dim(pdata())[1L])
        drawInfotip(sdata = pdata()[posi_x(), ], left_px = left_px(), top_px = top_px())
      })

      session$onSessionEnded(function() shiny::stopApp())
    }

    shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))

  } else {
    message("Please install the 'shiny' package to enable oanda_studio()")
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
  cache <- NULL
  function(server = c("practice", "live"), apikey) {
    if (is.null(cache)) {
      if (missing(apikey)) apikey <- oanda_get_key()
      server <- match.arg(server)
      url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                    ".oanda.com/v3/accounts/", oandaAccount(), "/instruments")
      h <- new_handle()
      handle_setheaders(handle = h,
                        "Authorization" = paste0("Bearer ", apikey),
                        "User-Agent" = ichimoku_user_agent)
      resp <- curl_fetch_memory(url = url, handle = h)
      if (resp$status_code != 200L) stop("code ", resp$status_code, " - ",
                                         fromJSON(rawToChar(resp$content)), call. = FALSE)
      data <- fromJSON(rawToChar(resp$content))$instruments
      cache <<- data[order(data[, 1L]), 1:3]
    }
    cache
  }
}

#' Account associated with an OANDA fxTrade API Key
#'
#' Return an account authorised to be accessed by an OANDA fxTrade API key
#'     (personal access token). Used by other OANDA functions to access API
#'     endpoints that require an account ID.
#'
#' @inheritParams oanda
#'
#' @return A character string of the first listed account the authorization
#'     bearer token is authorized to access.
#'
#' @keywords internal
#'
oandaAccount <- function(server = c("practice", "live"), apikey) {
  cache <- NULL
  function(server = c("practice", "live"), apikey) {
    if (is.null(cache)) {
      if (missing(apikey)) apikey <- oanda_get_key()
      server <- match.arg(server)
      url <- switch(server,
                    practice = "https://api-fxpractice.oanda.com/v3/accounts",
                    live = "https://api-fxtrade.oanda.com/v3/accounts")
      h <- new_handle()
      handle_setheaders(handle = h,
                        "Authorization" = paste0("Bearer ", apikey),
                        "User-Agent" = ichimoku_user_agent)
      resp <- curl_fetch_memory(url = url, handle = h)
      if (resp$status_code != 200L) stop("code ", resp$status_code, " - ",
                                         fromJSON(rawToChar(resp$content)), call. = FALSE)
      data <- fromJSON(rawToChar(resp$content))$accounts
      cache <<- unlist(data, use.names = FALSE)[[1]]
    }
    cache
  }
}

#' Set OANDA fxTrade API Key
#'
#' Save OANDA fxTrade API key (personal access token) to the system credential
#'     store.
#'
#' @return A key is set in the default keyring under the service name 'OANDA_API_KEY'.
#'
#' @details The key is read interactively.
#'
#'     This function only needs to be run once to set the key, it does not need
#'     to be run each session.
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
    message("Please install the 'keyring' package in order to store your OANDA API key")
  }
}

#' Get OANDA fxTrade API Key
#'
#' Return OANDA fxTrade API key (personal access token) saved in the system
#'     credential store, or else prompts the user to provide a key interactively.
#'
#' @return A temporarily invisible character string, respresenting the key stored
#'     in the default keyring under the service name 'OANDA_API_KEY' if present,
#'     otherwise the key supplied by the user interactively.
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
  keystore <- NULL
  function() {
    if (is.null(keystore)) {
      if (requireNamespace("keyring", quietly = TRUE)) {
        apikey <- tryCatch(keyring::key_get(service = "OANDA_API_KEY"), error = function(e) {
          message("Note: oanda_set_key() can be used to store your API key for automatic retrieval")
          readline("Please enter OANDA API key: ")
        })
      } else {
        apikey <- readline("Please enter OANDA API key: ")
      }
      keystore <<- apikey
    }
    invisible(keystore)
  }
}

