# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' OANDA Price Data
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
#' @param server (optional) specify the "practice" or "live" server according to
#'     the account type held. If not specified, will default to "practice", unless
#'     this has been changed by \code{\link{oanda_switch}}.
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
#' # OANDA fxTrade API key required to run these examples
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
                  server,
                  apikey) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  server <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_oanda$getKey()

  if (!missing(from) && !missing(to)) {
    d1 <- tryCatch(as.POSIXct(from), error = function(e) {
      stop("Specified value of 'from' is not convertible to a POSIXct date-time format", call. = FALSE)
    })
    d2 <- tryCatch(as.POSIXct(to), error = function(e) {
      stop("Specified value of 'to' is not convertible to a POSIXct date-time format", call. = FALSE)
    })
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
                                           requests, " requests will be made. Continue? [Y/n] "))
      if (continue %in% c("n", "N", "no", "NO")) stop("Request cancelled by user", call. = FALSE)
      message("Request started with rate limiting in place >>>")
      list <- vector(mode = "list", length = requests)
      for (i in seq_len(requests)) {
        list[[i]] <- getPrices(instrument = instrument, granularity = granularity,
                               from = strftime(first[[i]], format = "%Y-%m-%dT%H:%M:%S"),
                               to = strftime(second[[i]], format = "%Y-%m-%dT%H:%M:%S"),
                               price = price, server = server, apikey = apikey)
        message("Downloaded data partition ", i, " of ", requests)
        if (i != requests) Sys.sleep(1)
      }
      message("Merging data partitions...")
      df <- do.call(df_merge, list)
      message("Complete")
      }

    } else {
      if (!missing(from)) {
        from <- tryCatch(as.POSIXct(from), error = function(e) {
          stop("Specified value of 'from' is not convertible to a POSIXct date-time format", call. = FALSE)
        })
        from <- strftime(from, format = "%Y-%m-%dT%H:%M:%S")
      }
      if (!missing(to)) {
        to <- tryCatch(as.POSIXct(to), error = function(e) {
          stop("Specified value of 'to' is not convertible to a POSIXct date-time format", call. = FALSE)
        })
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
#' @inheritParams oanda
#' @param .validate (optional) only used internally by other functions. Do not
#'     set this parameter.
#'
#' @return A data.frame containing the price data requested.
#'
#' @noRd
#'
getPrices <- function(instrument, granularity, count, from, to, price, server,
                      apikey, .validate) {

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
                    "User-Agent" = x_user_agent)
  resp <- curl_fetch_memory(url = url, handle = h)

  if (resp$status_code != 200L) stop("server code ", resp$status_code, " - ",
                                     parse_json(rawToChar(resp$content)), call. = FALSE)
  headers <- rawToChar(resp$headers)
  hdate <- strsplit(headers, "date: | GMT", perl = TRUE)[[1L]][2L]
  timestamp <- as.POSIXct.POSIXlt(strptime(hdate, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  data <- parse_json(rawToChar(resp$content), simplifyVector = TRUE)[["candles"]]

  time <- strptime(.subset2(data, "time"), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
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
    } else if (missing(.validate)) {
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

  ohlc <- switch(price, M = .subset2(data, "mid"), B = .subset2(data, "bid"), A = .subset2(data, "ask"))

  df <- list(.POSIXct(time),
             as.numeric(.subset2(ohlc, "o")),
             as.numeric(.subset2(ohlc, "h")),
             as.numeric(.subset2(ohlc, "l")),
             as.numeric(.subset2(ohlc, "c")),
             .subset2(data, "volume"),
             .subset2(data, "complete"))
  attributes(df) <- list(names = c("time", "open", "high", "low", "close", "volume", "complete"),
                         class = "data.frame",
                         row.names = .set_row_names(length(time)),
                         instrument = instrument,
                         price = price,
                         timestamp = .POSIXct(timestamp),
                         oanda = TRUE)
  df

}

#' OANDA Streaming Data
#'
#' Stream live price and liquidity data for major currencies, metals,
#'     commodities, government bonds and stock indices from the OANDA fxTrade
#'     Streaming API.
#'
#' @inheritParams oanda
#'
#' @details This function connects to the OANDA fxTrade Streaming API. Use the
#'     'Esc' key to stop the stream.
#'
#'     The output contains ANSI escape codes for console formatting, but
#'     otherwise represents the raw feed without omission. Note that as this is
#'     a raw stream, returned times are in UTC.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @return Invisible NULL on function exit. The streaming data is output as text
#'     to the console.
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
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_stream("USD_JPY")
#' }
#'
#' @export
#'
oanda_stream <- function(instrument, server, apikey) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  if (missing(apikey)) apikey <- do_oanda$getKey()
  server <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
  url <- paste0("https://stream-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/accounts/", do_oanda$getAccount(server = server, apikey = apikey),
                "/pricing/stream?instruments=", instrument)
  h <- new_handle()
  handle_setheaders(handle = h,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "RFC3339",
                    "User-Agent" = x_user_agent)

  message("Streaming data... Press 'Esc' to return")
  on.exit(expr = return(invisible()))
  curl_fetch_stream(url = url, handle = h, fun = function(x) {
    stream <- sub("close", "\u001b[27m\nclose",
                  sub("asks:", "\u001b[27m asks:\u001b[7m ",
                      sub("bids:", "\nbids:\u001b[7m ",
                          gsub(",", "  ",
                               gsub('"|{|}|\\[|\\]', "", rawToChar(x), perl = TRUE),
                               fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
    cat(stream)
  })
}

#' OANDA Real-time Cloud Charts
#'
#' Plot real-time Ichimoku Kinko Hyo cloud charts for major currencies, metals,
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
#' @return The ichimoku object underlying the chart (invisibly) on function exit.
#'     A plot of the ichimoku chart for the price data requested is output to the
#'     graphical device at each refresh interval.
#'
#' @details This function polls the OANDA fxTrade API for the latest live prices
#'     and updates the plot in the graphical device at each refresh interval.
#'     Use the 'Esc' key to stop updating.
#'
#'     To access the underlying data, assign the function to an object, for
#'     example: \code{cloud <- oanda_chart("USD_JPY")}.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run these examples
#' oanda_chart("USD_JPY")
#' oanda_chart("EUR_JPY", granularity = "H1", refresh = 3, count = 300, price = "B", theme = "mono")
#'
#' # Save data underlying chart at time of function exit
#' cloud <- oanda_chart("USD_JPY")
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
                        server,
                        apikey,
                        ...,
                        periods = c(9L, 26L, 52L)) {

  if (missing(instrument)) stop("Argument 'instrument' must be specified", call. = FALSE)
  if (missing(apikey)) apikey <- do_oanda$getKey()
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  theme <- match.arg(theme)
  server <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
  if (!is.numeric(refresh) || refresh < 1) {
    message("Specified refresh interval invalid - reverting to default of 5 secs")
    refresh <- 5
  }
  if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
    periods <- as.integer(periods)
  } else {
    warning("Specified cloud periods invalid - reverting to defaults c(9L, 26L, 52L)", call. = FALSE)
    periods <- c(9L, 26L, 52L)
  }
  p2 <- periods[2L]
  minlen <- p2 + periods[3L]
  if (!is.numeric(count) || count < minlen) {
    message("Specified 'count' invalid - reverting to default of 250")
    count <- 250
  }

  ins <- do_oanda$getInstruments(server = server, apikey = apikey)
  ticker <- .subset2(ins, "displayName")[.subset2(ins, "name") %in% instrument]
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
  on.exit(expr = return(invisible(pdata)))
  while (TRUE) {
    pdata <- ichimoku.data.frame(data, periods = periods, ...)[minlen:(xlen + p2 - 1L), ]
    subtitle <- paste(instrument, ptype, "price [", data$close[xlen],
                      "] at", attr(data, "timestamp"), "| Chart:", ctype,
                      "| Cmplt:", .subset2(data, "complete")[xlen])
    plot.ichimoku(pdata, ticker = ticker, subtitle = subtitle, theme = theme,
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

#' OANDA Studio Interactive Live Analysis
#'
#' Interactive and fully-customisable R Shiny environment providing real-time
#'     Ichimoku Kinko Hyo cloud charts for major currencies, metals, commodities,
#'     government bonds and stock indices using OANDA fxTrade API data. Intuitive
#'     cursor infotip provides ready access to the data directly from the chart.
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
#' @return A Shiny app object with class 'shiny.appobj'. With default arguments,
#'     the Shiny app is launched in the default browser.
#'
#' @details This function polls the OANDA fxTrade API for the latest prices and
#'     updates a customisable reactive Shiny app at each refresh interval.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     running: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run these examples
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
                         server,
                         apikey,
                         ...,
                         launch.browser = TRUE,
                         periods = c(9L, 26L, 52L)) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    if (missing(apikey)) apikey <- do_oanda$getKey()
    if (!is.numeric(refresh) || refresh < 1) {
      message("Specified refresh interval invalid - reverting to default of 5 secs")
      refresh <- 5
    }
    granularity <- match.arg(granularity)
    price <- match.arg(price)
    theme <- match.arg(theme)
    srvr <- if (missing(server)) do_oanda$getServer() else match.arg(server, c("practice", "live"))
    if (is.numeric(periods) && length(periods) == 3L && all(periods >= 1)) {
      periods <- as.integer(periods)
    } else {
      warning("Specified cloud periods invalid - reverting to defaults c(9L, 26L, 52L)", call. = FALSE)
      periods <- c(9L, 26L, 52L)
    }
    p2 <- periods[2L]
    minlen <- p2 + periods[3L]
    if (!is.numeric(count) || count <= minlen) {
      message("Specified 'count' invalid - reverting to default of 300")
      count <- 300
    }

    ins <- do_oanda$getInstruments(server = srvr, apikey = apikey)
    dispnamevec <- .subset2(ins, "displayName")
    namevec <- .subset2(ins, "name")

    ui <- shiny::fluidPage(
      shiny::tags$head(shiny::tags$style("
    #chart {height: calc(100vh - 147px) !important}
    .control-label {font-weight: 400}
  ")),
      shiny::fillPage(
        padding = 20,
        shiny::plotOutput("chart", width = "100%",
                          hover = shiny::hoverOpts(id = "plot_hover", delay = 80, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("infotip")
      ),
      shiny::fluidRow(
        shiny::column(width = 12, shiny::HTML("&nbsp;")
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 2,
                      shiny::selectInput("theme", label = "Theme",
                                         choices = c("original", "dark", "solarized", "mono"),
                                         selected = theme, selectize = FALSE)),
        shiny::column(width = 2,
                      shiny::selectInput("instrument", label = "Instrument",
                                         choices = ins$name,
                                         selected = instrument, selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::selectInput("granularity", label = "Granularity",
                                         choices = c("M", "W", "D",
                                                     "H12", "H8", "H6", "H4", "H3", "H2", "H1",
                                                     "M30", "M15", "M10", "M5", "M4", "M2", "M1",
                                                     "S30", "S15", "S10", "S5"),
                                         selected = granularity, selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::selectInput("price", label = "Price",
                                         choices = c("M", "B", "A"),
                                         selected = price, selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::numericInput("refresh", label = "Refresh",
                                          value = refresh, min = 1, max = 86400)),
        shiny::column(width = 1,
                      shiny::HTML("<label class='control-label'>Data</label><div class='form-group shiny-input-container'>"),
                      shiny::downloadButton("savedata", label = "> Archive"),
                      shiny::HTML("</div>")),
        shiny::column(width = 3,
                      shiny::sliderInput("count", label = "Data Periods", min = 100,
                                         max = 800, value = count, width = "100%")),
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
      dispname <- shiny::reactive(dispnamevec[namevec %in% input$instrument])

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
        } else {
          idata()
        }
      })
      xlen <- shiny::reactive(dim(data())[1L])
      pdata <- shiny::reactive(ichimoku(data(), ticker = input$instrument,
                                        periods = periods, ...)[minlen:(xlen() + p2 - 1L), ])
      plen <- shiny::reactive(xlen() + p2 - minlen)
      ticker <- shiny::reactive(paste(dispname(), "  |", input$instrument, ptype(), "price [",
                                      data()$close[xlen()], "] at", attr(data(), "timestamp"),
                                      "| Chart:", ctype(), "| Cmplt:",
                                      .subset2(data(), "complete")[xlen()]))

      output$chart <- shiny::renderPlot(
        autoplot.ichimoku(pdata(), ticker = ticker(), theme = input$theme, ...)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawGuide(label = index(pdata())[posi_x()], left = left_px() - 17, top = 45)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawInfotip(sdata = pdata()[posi_x(), ], left_px = left_px(), top_px = top_px())
      })

      output$savedata <- shiny::downloadHandler(filename = function() paste0(input$instrument, "_", input$granularity, "_", input$price, ".rda"),
                                                content = function(file) archive(pdata(), file))

      session$onSessionEnded(function() shiny::stopApp())
    }

    shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = launch.browser, ...))

  } else {
    message("Please install the 'shiny' package to enable oanda_studio()")
  }
}

#' List Available OANDA Instruments
#'
#' Return list of instruments including major currencies, metals, commodities,
#'     government bonds and stock indices for which pricing data is available
#'     from the OANDA fxTrade API.
#'
#' @inheritParams oanda
#'
#' @return A data.frame containing the instrument name, full display name, and type.
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
oanda_instruments <- function(server, apikey) {

  do_oanda$getInstruments(server = server, apikey = apikey)

}

#' Set OANDA fxTrade API Key
#'
#' Save OANDA fxTrade API key (personal access token) to the system credential
#'     store.
#'
#' @return Invisible NULL. A key is set in the default keyring under the service
#'     name 'OANDA_API_KEY' for practice accounts or 'OANDA_LIVE_KEY' for live
#'     accounts.
#'
#' @details The key is read interactively. Different keys can be set for practice
#'     and live accounts; enter 1 when prompted to set a practice account key or
#'     2 to set a live account key.
#'
#'     This function only needs to be run once to set the key; it does not need
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

    type <- readline("Please choose:\n(1) for practice account \n(2) for live account")

    if (type == 1) {
      keyring::key_set(service = "OANDA_API_KEY")
    } else if (type == 2) {
      keyring::key_set(service = "OANDA_LIVE_KEY")
    } else {
      message("Invalid choice - choice must be either 1 or 2")
    }

  } else {
    message("Please install the 'keyring' package to store your OANDA API key")
  }

  invisible()

}

