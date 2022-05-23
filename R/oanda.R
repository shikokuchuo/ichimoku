# Ichimoku - OANDA fxTrade API Interface ---------------------------------------

#' OANDA Price Data
#'
#' Retrieve price data for major currencies, metals, commodities, government
#'     bonds and stock indices from the OANDA fxTrade API.
#'
#' @param instrument string containing the base currency and quote currency
#'     delimited by '_' or '-' (e.g. "USD_JPY" or "usd-jpy"). Use the
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
#'     data, in a format convertible to POSIXct by \code{as.POSIXct()}, for
#'     example "2020-02-01".
#' @param to (optional) the end of the time range for which to fetch price data,
#'     in a format convertible to POSIXct by \code{as.POSIXct()}, for example
#'     "2020-06-30".
#' @param price [default "M"] pricing component, one of "M" (midpoint), "B" (bid)
#'     or "A" (ask).
#' @param server (optional) specify the "practice" or "live" server according to
#'     the account type held. If not specified, will default to "practice", unless
#'     this has been changed by \code{\link{oanda_switch}}.
#' @param apikey (optional) string containing the OANDA fxTrade API key (personal
#'     access token), or function that returns this string. Does not need to be
#'     specified if already stored by oanda_set_key(). Can also be entered
#'     interactively if not specified.
#' @param quietly (optional) if set to TRUE, will suppress printing of auxiliary
#'     output to the console and return quietly.
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
#' @section Further Details:
#'     Please refer to the OANDA fxTrade API vignette by calling:
#'     \code{vignette("xoanda", package = "ichimoku")}.
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
                  apikey,
                  quietly) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)

  if (!missing(from) && !missing(to)) {
    d1 <- tryCatch(as.POSIXct(from), error = function(e) {
      stop("specified value of 'from' is not convertible to a POSIXct date-time format", call. = FALSE)
    })
    d2 <- tryCatch(as.POSIXct(to), error = function(e) {
      stop("specified value of 'to' is not convertible to a POSIXct date-time format", call. = FALSE)
    })
    interval <- unclass(d2) - unclass(d1)
    interval >= 0 || stop("requested time period invalid - 'to' takes place before 'from'", call. = FALSE)
    denom <- switch(granularity,
                    M = 18144000, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                    H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                    M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                    M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
    requests <- ceiling(interval / denom / 5000)

    if (requests == 1) {
      df <- getPrices(instrument = instrument, granularity = granularity,
                      from = strftime(d1, format = "%Y-%m-%dT%H:%M:%S"),
                      to = strftime(d2, format = "%Y-%m-%dT%H:%M:%S"),
                      price = price, server = server, apikey = apikey)

    } else {
      bounds <- d1 + interval * 0:requests / requests
      continue <- if (interactive()) readline(prompt = paste0("Max of 5000 data periods per request. ",
                                                              requests, " requests will be made. Continue? [Y/n] ")) else ""
      continue %in% c("n", "N", "no", "NO") && stop("Request cancelled by user", call. = FALSE)
      output <- missing(quietly) || !isTRUE(quietly)
      list <- vector(mode = "list", length = requests)
      for (i in seq_len(requests)) {
        if (output) cat("\rPerforming request [", rep(".", i), rep(" ", requests - i), "]",
                        file = stdout(), sep = "")
        list[[i]] <- getPrices(instrument = instrument, granularity = granularity,
                               from = strftime(bounds[i], format = "%Y-%m-%dT%H:%M:%S"),
                               to = strftime(bounds[i + 1L], format = "%Y-%m-%dT%H:%M:%S"),
                               price = price, server = server, apikey = apikey)
      }
      if (output) cat("\nMerging data partitions... ", file = stdout())
      df <- do.call(df_merge, list)
      if (output) cat("complete\n", file = stdout())
      }

    } else {
      if (!missing(from)) {
        from <- tryCatch(as.POSIXct(from), error = function(e) {
          stop("specified value of 'from' is not convertible to a POSIXct date-time format", call. = FALSE)
        })
      }
      if (!missing(to)) {
        to <- tryCatch(as.POSIXct(to), error = function(e) {
          stop("specified value of 'to' is not convertible to a POSIXct date-time format", call. = FALSE)
        })
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
  handle <- new_handle()
  handle_setheaders(handle = handle,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "UNIX",
                    "User-Agent" = .user_agent)
  resp <- curl_fetch_memory(url = url, handle = handle)

  .subset2(resp, "status_code") == 200L || stop("server code ",
                                                .subset2(resp, "status_code"), " - ",
                                                parse_json(rawToChar(.subset2(resp, "content"))),
                                                call. = FALSE)

  hdate <- strsplit(rawToChar(.subset2(resp, "headers")), "date: | GMT", perl = TRUE)[[1L]][2L]
  timestamp <- as.POSIXct.POSIXlt(strptime(hdate, format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  ptype <- switch(price, M = "mid", B = "bid", A = "ask")

  !missing(.validate) && .validate == FALSE && {
    data <- .subset2(.subset2(.subset2(parse_json(rawToChar(.subset2(resp, "content"))), "candles"), 1L), ptype)
    data <- `storage.mode<-`(unlist(data), "double")
    return(c(t = unclass(timestamp), data))
  }

  data <- do.call(rbind, .subset2(parse_json(rawToChar(.subset2(resp, "content"))), "candles"))
  time <- as.POSIXlt.POSIXct(unlist(data[, "time", drop = FALSE]))
  if (granularity == "D") {
    keep <- .subset2(time, "wday") %in% 0:4
    if (missing(.validate)) {
      keep[.subset2(time, "mon") == 11L & .subset2(time, "mday") == 31L |
             (.subset2(time, "mon") == 11L & .subset2(time, "mday") == 24L)] <- FALSE
    }
    data <- data[keep, , drop = FALSE]
    time <- .subset(as.POSIXct.POSIXlt(time), keep)
  } else if (granularity == "M") {
    time <- as.POSIXlt.POSIXct(unclass(as.POSIXct.POSIXlt(time)) + 86400)
    time$mon <- .subset2(time, "mon") + 1L
    time <- unclass(as.POSIXct.POSIXlt(time))
  } else if (missing(.validate) && granularity != "W") {
    cut <- (.subset2(time, "wday") == 5L & .subset2(time, "hour") > 20L) | .subset2(time, "wday") == 6L | (.subset2(time, "wday") == 0L & .subset2(time, "hour") < 21L)
    data <- data[!cut, , drop = FALSE]
    time <- .subset(as.POSIXct.POSIXlt(time), !cut)
  }
  periodicity <- switch(granularity,
                        M = -86400, W = 604800, D = 86400, H12 = 43200, H8 = 28800,
                        H6 = 21600, H4 = 14400, H3 = 10800, H2 = 7200, H1 = 3600,
                        M30 = 1800, M15 = 900, M10 = 600, M5 = 300, M4 = 240,
                        M2 = 120, M1 = 60, S30 = 30, S15 = 15, S10 = 10, S5 = 5)
  time <- .Call(ichimoku_psxct, time + periodicity)
  ohlc <- unlist(data[, ptype, drop = FALSE])
  cnames <- names(ohlc)

  df <- `attributes<-`(
    list(time,
         as.numeric(ohlc[cnames == "o"]),
         as.numeric(ohlc[cnames == "h"]),
         as.numeric(ohlc[cnames == "l"]),
         as.numeric(ohlc[cnames == "c"]),
         unlist(data[, "volume", drop = FALSE]),
         unlist(data[, "complete", drop = FALSE])),
    list(names = c("time", "open", "high", "low", "close", "volume", "complete"),
         class = "data.frame",
         row.names = .set_row_names(length(time)),
         instrument = instrument,
         price = price,
         timestamp = .Call(ichimoku_psxct, timestamp),
         oanda = TRUE)
  )

  df

}

#' OANDA Streaming Data
#'
#' Stream live price and liquidity data for major currencies, metals,
#'     commodities, government bonds and stock indices from the OANDA fxTrade
#'     Streaming API.
#'
#' @inheritParams oanda
#' @param display [default 7L] integer rows of data to display in the console
#'     at any one time.
#' @param limit (optional) specify a time in seconds by which to limit the
#'     streaming session. The session will end with data returned automatically
#'     after the specified time has elapsed.
#'
#' @return Returned invisibly, a dataframe containing the data for the streaming
#'     session on function exit. The latest rows of the dataframe are printed to
#'     the console, as governed by the 'display' argument.
#'
#' @details This function connects to the OANDA fxTrade Streaming API. Use the
#'     'Esc' key to stop the stream and return the session data.
#'
#'     Note: only messages of type 'PRICE' are processed. Messages of type
#'     'HEARTBEAT' consisting of only a timestamp are discarded.
#'
#' @section Streaming Data:
#'
#'     Summarised from the streaming API documentation:
#'
#'     \itemize{
#'     \item{Pricing stream does not include every single price created for the
#'     Account}
#'     \item{At most 4 prices are sent per second (every 250 milliseconds) for
#'     each instrument}
#'     \item{If more than one price is created during the 250 millisecond window,
#'     only the price in effect at the end of the window is sent}
#'     \item{This means that during periods of rapid price movement, not every
#'     price is sent}
#'     \item{Pricing windows for different connections to the stream are not all
#'     aligned in the same way (e.g. to the top of the second)}
#'     \item{This means that during periods of rapid price movement, different
#'     prices may be observed depending on the alignment for the connection}
#'     }
#'
#' @section Further Details:
#'     Please refer to the OANDA fxTrade API vignette by calling:
#'     \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' data <- oanda_stream("USD_JPY", display = 7L)
#' }
#'
#' @export
#'
oanda_stream <- function(instrument, display = 7L, limit, server, apikey) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  if (!missing(display) && !is.numeric(display) || display < 1L) display <- 7L
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)
  url <- paste0("https://stream-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/accounts/", do_$getAccount(server = server, apikey = apikey),
                "/pricing/stream?instruments=", instrument)
  handle <- new_handle()
  handle_setheaders(handle = handle,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "UNIX",
                    "User-Agent" = .user_agent)

  data <- NULL
  on.exit(expr = {
    xlen <- dim(data)[1L]
    bids <- unlist(.subset2(data, "bids"))
    ncol <- length(bids) / xlen
    data[["bids"]] <- matrix(as.numeric(bids), nrow = xlen, ncol = ncol, byrow = TRUE,
                             dimnames = list(NULL, names(bids)[1:ncol]))
    asks <- unlist(.subset2(data, "asks"))
    ncol <- length(asks) / xlen
    data[["asks"]] <- matrix(as.numeric(asks), nrow = xlen, ncol = ncol, byrow = TRUE,
                             dimnames = list(NULL, names(asks)[1:ncol]))
    data[["closeoutBid"]] <- as.numeric(.subset2(data, "closeoutBid"))
    data[["closeoutAsk"]] <- as.numeric(.subset2(data, "closeoutAsk"))
    return(invisible(data))
  })

  if (!missing(limit) && is.numeric(limit)) setTimeLimit(elapsed = limit, transient = TRUE)

  con <- curl(url = url, handle = handle)
  stream_in(con = con, pagesize = 1L, verbose = FALSE, handler = function(x) {
    .subset2(x, "type") == "PRICE" || return()
    x[["time"]] <- .POSIXct(as.numeric(.subset2(x, "time")))
    if (is.null(data)) {
      data <<- x
    } else {
      data <<- df_append(old = data, new = x)
    }
    end <- dim(data)[1L]
    start <- max(1L, end - display + 1L)
    cat("\f", file = stdout())
    message("Streaming data... 'Esc' or 'Ctrl+c' to return")
    print.data.frame(data[start:end, ])
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
#' @inheritParams plot.ichimoku
#' @param refresh [default 5] data refresh interval in seconds, with a minimum
#'     of 1.
#' @param count [default 250] the number of periods to return. The API supports
#'     a maximum of 5000. Note that fewer periods are actually shown on the
#'     chart to ensure a full cloud is always displayed.
#' @param type [default 'none'] type of sub-plot to display beneath the ichimoku
#'     cloud chart, with a choice of 'none', 'r' or 's' for the corresponding
#'     oscillator type.
#' @param limit (optional) specify a time in seconds by which to limit the
#'     session. The session will end with data returned automatically after the
#'     specified time has elapsed.
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
#' @section Further Details:
#'     Please refer to the OANDA fxTrade API vignette by calling:
#'     \code{vignette("xoanda", package = "ichimoku")}.
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
                        theme = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                        type = c("none", "r", "s"),
                        limit,
                        server,
                        apikey,
                        ...,
                        periods = c(9L, 26L, 52L)) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  granularity <- match.arg(granularity)
  price <- match.arg(price)
  if (length(theme) != 12L) theme <- match.arg(theme)
  type <- match.arg(type)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)
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

  ins <- do_$getInstruments(server = server, apikey = apikey)
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

  message("Chart updating every ", refresh, " secs in graphical device... 'Esc' or 'Ctrl+c' to return")
  on.exit(expr = return(invisible(pdata)))
  if (!missing(limit) && is.numeric(limit)) setTimeLimit(elapsed = limit, transient = TRUE)
  repeat {
    pdata <- create_data(.ichimoku(data, periods = periods, ...), type = type)[minlen:(xlen + p2 - 1L), ]
    subtitle <- paste(instrument, ptype, "price [", .subset2(data, "close")[xlen],
                      "] at", attr(data, "timestamp"), "| Chart:", ctype,
                      "| Cmplt:", .subset2(data, "complete")[xlen])
    print(plot_ichimoku(pdata, ticker = ticker, subtitle = subtitle, theme = theme, type = type), newpage = FALSE, ...)
    Sys.sleep(refresh)
    newdata <- getPrices(instrument = instrument, granularity = granularity,
                         count = ceiling(refresh / periodicity) + 1,
                         price = price, server = server, apikey = apikey,
                         .validate = TRUE)
    data <- df_append(old = data, new = newdata)
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
#' @param theme [default 'original'] with alternative choices of 'conceptual',
#'     'dark', 'fresh', 'mono', or 'solarized'.
#' @param new.process [default FALSE] if TRUE, will start the shiny session in a
#'     new R process, unblocking the current process and allowing continued use
#'     of the R console.
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
#'     This function has a dependency on the 'shiny' package.
#'
#' @section Further Details:
#'     Please refer to the OANDA fxTrade API vignette by calling:
#'     \code{vignette("xoanda", package = "ichimoku")}.
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
                         theme = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                         type = c("none", "r", "s"),
                         server,
                         apikey,
                         new.process = FALSE,
                         ...,
                         launch.browser = TRUE,
                         periods = c(9L, 26L, 52L)) {

  if (requireNamespace("shiny", quietly = TRUE)) {

    isTRUE(new.process) && {
      mc <- match.call()
      mc[["new.process"]] <- NULL
      cmd <- switch(.subset2(.Platform, "OS.type"),
                    unix = file.path(R.home("bin"), "Rscript"),
                    windows = file.path(R.home("bin"), "Rscript.exe"))
      return(system2(command = cmd, args = c("-e", shQuote(paste0("ichimoku::", deparse(mc)))),
                     stdout = NULL, stderr = NULL, wait = FALSE))
    }
    if (!missing(instrument)) instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
    granularity <- match.arg(granularity)
    price <- match.arg(price)
    theme <- match.arg(theme)
    type <- match.arg(type)
    srvr <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
    if (missing(apikey)) apikey <- do_$getKey(server = srvr)
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
    if (!is.numeric(count) || count <= minlen) {
      message("Specified 'count' invalid - reverting to default of 300")
      count <- 300
    }

    ins <- do_$getInstruments(server = srvr, apikey = apikey)
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
                          hover = shiny::hoverOpts(id = "plot_hover", delay = 100, delayType = "throttle")),
        shiny::uiOutput("hover_x"), shiny::uiOutput("hover_y"), shiny::uiOutput("infotip")
      ),
      shiny::fluidRow(
        shiny::column(width = 12, shiny::HTML("&nbsp;")
        )
      ),
      shiny::fluidRow(
        shiny::column(width = 1,
                      shiny::selectInput("theme", label = "Theme",
                                         choices = c("original", "conceptual", "dark", "fresh", "mono", "solarized"),
                                         selected = theme, selectize = FALSE)),
        shiny::column(width = 1,
                      shiny::selectInput("type", label = "Indicator",
                                         choices = c("none", "r", "s"),
                                         selected = type, selectize = FALSE)),
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
                      shiny::downloadButton("savedata", label = "Archive"),
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
          df <- df_append(old = datastore(), new = newdata())
          dlen <- dim(df)[1L]
          if (dlen > input$count) df <- df[(dlen - input$count + 1L):dlen, ]
          datastore(df)

        } else {
          df <- df_append(old = idata(), new = newdata())
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
      pdata <- shiny::reactive(
        create_data(.ichimoku(data(), ticker = input$instrument, periods = periods, ...),
                    type = input$type)[minlen:(xlen() + p2 - 1L), ])
      plen <- shiny::reactive(xlen() + p2 - minlen)
      ticker <- shiny::reactive(paste(dispname(), "  |", input$instrument, ptype(), "price [",
                                      .subset2(data(), "close")[xlen()], "] at", attr(data(), "timestamp"),
                                      "| Chart:", ctype(), "| Cmplt:",
                                      .subset2(data(), "complete")[xlen()]))

      output$chart <- shiny::renderPlot(
        plot_ichimoku(pdata(), ticker = ticker(), theme = input$theme, type = input$type, ...)
      )
      output$hover_x <- shiny::renderUI({
        shiny::req(input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawGuide(label = index.ichimoku(pdata(), posi_x()), left = left_px() - 17, top = 45)
      })
      output$hover_y <- shiny::renderUI({
        shiny::req(input$plot_hover)
        drawGuide(label = signif(input$plot_hover$y, digits = 5), left = 75, top = top_px() + 11)
      })
      output$infotip <- shiny::renderUI({
        shiny::req(input$infotip, input$plot_hover, posi_x() > 0, posi_x() <= plen())
        drawInfotip(sidx = index.ichimoku(pdata(), posi_x()),
                    sdata = coredata.ichimoku(pdata())[posi_x(), ],
                    left = left_px(), top = top_px(),
                    type = input$type)
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

#' Available OANDA Instruments
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
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
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

  do_$getInstruments(server = server, apikey = apikey)

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
#' @details The key is read interactively. Separate keys can be set for practice
#'     and live accounts - please choose the correct account type when prompted.
#'
#'     This function only needs to be called once to set the key; it does not
#'     need to be called each session.
#'
#'     This function has a dependency on the 'keyring' package.
#'
#' @section Further Details:
#'     Please refer to the OANDA fxTrade API vignette by calling:
#'     \code{vignette("xoanda", package = "ichimoku")}.
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

    type <- if (interactive()) readline("Choose account type, either [p]ractice or [l]ive: ") else ""
    type <- tryCatch(match.arg(type, c("practice", "live")), error = function(e) "")
    switch(type,
           practice = keyring::key_set(service = "OANDA_API_KEY"),
           live = keyring::key_set(service = "OANDA_LIVE_KEY"),
           message("Invalid entry - account type should be one of 'practice', 'live'"))

  } else {
    message("Please install the 'keyring' package to store your OANDA API key")
  }

  invisible()

}

#' OANDA View Market Performance
#'
#' Provides a snapshot overview of markets on an intraday basis, showing the
#'     relative performance of individual constituents.
#'
#' @param market string specifying the market: 'allfx' for all available
#'     currencies, 'bonds' for government bonds, 'commodities' for commodities,
#'     'fx' for major currency pairs, 'metals' for metals and 'stocks' for
#'     global stock markets.
#' @inheritParams oanda
#'
#' @return A data.frame containing the daily open, high, low and last prices,
#'     along with the percentage price change from the open, ordered by the
#'     percentage change. The instrument names are set as row names.
#'
#'     The first timestamp retrieved and the pricing component are printed to
#'     the console as well as saved as attributes to the dataframe. The dataframe
#'     is also printed to the console.
#'
#' @details This function is designed for interactive use.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_view("fx")
#' }
#'
#' @export
#'
oanda_view <- function(market = c("allfx", "bonds", "commodities", "fx", "metals", "stocks"),
                       price = c("M", "B", "A"),
                       server,
                       apikey) {

  if (missing(market) && interactive()) market <- readline("Enter market [a]llfx [b]onds [c]ommodities [f]x [m]etals [s]tocks: ")
  market <- match.arg(market)
  price <- match.arg(price)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)

  ins <- do_$getInstruments(server = server, apikey = apikey)
  sel <- switch(market,
                fx = {
                  vec <- .subset2(ins, "name")[.subset2(ins, "type") == "CURRENCY"]
                  vec[-grep("CNH|CZK|DKK|HKD|HUF|INR|MXN|NOK|PLN|SGD|SEK|THB|TRY|ZAR", vec, perl = TRUE)]
                },
                allfx = .subset2(ins, "name")[.subset2(ins, "type") == "CURRENCY"],
                stocks = .subset2(ins, "name")[grep("20|25|30|33|35|40|50|100|200|225|Shares|Index", .subset2(ins, "displayName"), perl = TRUE)],
                bonds = .subset2(ins, "name")[grep("02Y|05Y|10Y|30Y", .subset2(ins, "name"), perl = TRUE)],
                metals = .subset2(ins, "name")[.subset2(ins, "type") == "METAL"],
                commodities = {
                  vec <- .subset2(ins, "name")[.subset2(ins, "type") == "CFD"]
                  vec[-grep("[0-9]+|EUR|HKD|TWI", vec, perl = TRUE)]
                })
  xlen <- length(sel)
  data <- vector(mode = "list", length = xlen)
  for (i in seq_len(xlen)) {
    cat("\rRetrieving ", market, " [", rep(".", i), rep(" ", xlen - i), "]", file = stdout(), sep = "")
    data[[i]] <- getPrices(instrument = sel[i], granularity = "D", count = 1, price = price,
                           server = server, apikey = apikey, .validate = FALSE)
  }
  data <- do.call(rbind, data)
  time <- .Call(ichimoku_psxct, data[1L, "t", drop = FALSE])
  open <- data[, "o", drop = FALSE]
  high <- data[, "h", drop = FALSE]
  low <- data[, "l", drop = FALSE]
  close <- data[, "c", drop = FALSE]
  change <- round(100 * (close / open - 1), digits = 4L)
  reorder <- order(change, decreasing = TRUE)
  df <- `attributes<-`(
    list(open[reorder], high[reorder], low[reorder], close[reorder], change[reorder]),
    list(names = c("open", "high", "low", "last", "%chg"),
         class = "data.frame",
         row.names = sel[reorder],
         price = price,
         timestamp = time)
  )

  cat("\n", format.POSIXct(time), " / ", price, "\n", file = stdout(), sep = "")
  print(df)

}

#' OANDA Quote Latest Price
#'
#' Provides a single line price quote for an instrument.
#'
#' @inheritParams oanda
#'
#' @return Invisible NULL. The instrument, timestamp, daily open, high, low and
#'     last prices, percentage change from the open, and the pricing component
#'     (M for mid, B for bid, A for ask) is output to the console.
#'
#' @details This function is designed for interactive use.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_quote("USD_JPY")
#' }
#'
#' @export
#'
oanda_quote <- function(instrument, price = c("M", "B", "A"), server, apikey) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  price <- match.arg(price)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)
  data <- getPrices(instrument = instrument, granularity = "D", count = 1, price = price,
                    server = server, apikey = apikey, .validate = FALSE)
  pctchg <- round(100 * (data[["c"]] / data[["o"]] - 1), digits = 4L)
  cat(instrument, format.POSIXct(.Call(ichimoku_psxct, data[["t"]])),
      "open:", data[["o"]], " high:", data[["h"]], " low:", data[["l"]],
      " last:\u001b[7m", data[["c"]], "\u001b[27m %chg:", pctchg, price, file = stdout())

}

#' OANDA Position Book
#'
#' Provides a summary of the aggregate positions held by OANDA fxTrade clients
#'     at each price level.
#'
#' @inheritParams oanda
#' @param time (optional) the time for which to retrieve the position book, in a
#'     format convertible to POSIXct by \code{as.POSIXct()}. If not specified,
#'     the most recent position book will be retrieved.
#'
#' @return Invisibly, a data frame of the position book with parameters saved as
#'     attributes. A chart showing the percentage long and short positions at
#'     each price level is output to the graphical device.
#'
#' @details This feature has been implemented by OANDA only for certain major
#'     currency pairs and should be considered experimental.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_positions("USD_JPY")
#' }
#'
#' @export
#'
oanda_positions <- function(instrument, time, server, apikey) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)

  url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/instruments/", instrument, "/positionBook",
                if (!missing(time)) paste0("?time=", unclass(as.POSIXct(time))))
  handle <- new_handle()
  handle_setheaders(handle = handle,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "UNIX",
                    "User-Agent" = .user_agent)
  resp <- curl_fetch_memory(url = url, handle = handle)

  .subset2(resp, "status_code") == 200L || stop("server code ",
                                                .subset2(resp, "status_code"), " - ",
                                                parse_json(rawToChar(.subset2(resp, "content"))),
                                                call. = FALSE)

  data <- .subset2(parse_json(rawToChar(.subset2(resp, "content"))), "positionBook")
  currentprice <- as.numeric(.subset2(data, "price"))
  timestamp <- .Call(ichimoku_psxct, .subset2(data, "unixTime"))
  bucketwidth <- as.numeric(.subset2(data, "bucketWidth"))

  buckets <- `storage.mode<-`(do.call(rbind, .subset2(data, "buckets")), "double")
  df <- `attributes<-`(list(buckets[, "price"],
                            buckets[, "longCountPercent"],
                            buckets[, "shortCountPercent"]),
                       list(names = c("price", "long", "short"),
                            class = "data.frame",
                            row.names = .set_row_names(dim(buckets)[1L]),
                            instrument = instrument,
                            timestamp = timestamp,
                            currentprice = currentprice,
                            bucketwidth = bucketwidth))

  layers <- list(
    layer(geom = GeomCol, mapping = aes(x = .data$price, y = .data$long),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#1aa1a6", fill = "#1aa1a6"),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomCol, mapping = aes(x = .data$price, y = -.data$short),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#586e75", fill = "#586e75"),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomVline, data = NULL, mapping = aes(xintercept = currentprice),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#db4525", alpha = 0.5),
          inherit.aes = FALSE, check.aes = FALSE, check.param = FALSE),
    scale_x_continuous(breaks = function(x) pretty.default(x, n = 40L)),
    scale_y_continuous(),
    labs(x = "Price", y = "% short / % long",
         title = paste0("OANDA Position Book: ", instrument, " at ",
                        format.POSIXct(timestamp), " / Current Price: ", currentprice)),
    coord_flip(),
    theme_ichimoku_light()
  )

  print(ggplot(data = df) + layers)
  invisible(df)

}

#' OANDA Order Book
#'
#' Provides a summary of the aggregate orders posted by OANDA fxTrade clients
#'     at each price level.
#'
#' @inheritParams oanda
#' @param time (optional) the time for which to retrieve the order book, in a
#'     format convertible to POSIXct by \code{as.POSIXct()}. If not specified,
#'     the most recent order book will be retrieved.
#'
#' @return Invisibly, a data frame of the order book with parameters saved as
#'     attributes. A chart showing the percentage long and short orders at each
#'     price level is output to the graphical device.
#'
#' @details This feature has been implemented by OANDA only for certain major
#'     currency pairs and should be considered experimental.
#'
#'     Note: as certain orders are placed far from the market price, only the
#'     interquartile range of order levels is shown on the chart. The returned
#'     data frame does however contain the entire order book.
#'
#'     For further details please refer to the OANDA fxTrade API vignette by
#'     calling: \code{vignette("xoanda", package = "ichimoku")}.
#'
#' @examples
#' \dontrun{
#' # OANDA fxTrade API key required to run this example
#' oanda_orders("USD_JPY")
#' }
#'
#' @export
#'
oanda_orders <- function(instrument, time, server, apikey) {

  if (missing(instrument) && interactive()) instrument <- readline("Enter instrument:")
  instrument <- sub("-", "_", toupper(force(instrument)), fixed = TRUE)
  server <- if (missing(server)) do_$getServer() else match.arg(server, c("practice", "live"))
  if (missing(apikey)) apikey <- do_$getKey(server = server)

  url <- paste0("https://api-fx", switch(server, practice = "practice", live = "trade"),
                ".oanda.com/v3/instruments/", instrument, "/orderBook",
                if (!missing(time)) paste0("?time=", unclass(as.POSIXct(time))))
  handle <- new_handle()
  handle_setheaders(handle = handle,
                    "Authorization" = paste0("Bearer ", apikey),
                    "Accept-Datetime-Format" = "UNIX",
                    "User-Agent" = .user_agent)
  resp <- curl_fetch_memory(url = url, handle = handle)

  .subset2(resp, "status_code") == 200L || stop("server code ",
                                                .subset2(resp, "status_code"), " - ",
                                                parse_json(rawToChar(.subset2(resp, "content"))),
                                                call. = FALSE)

  data <- .subset2(parse_json(rawToChar(.subset2(resp, "content"))), "orderBook")
  currentprice <- as.numeric(.subset2(data, "price"))
  timestamp <- .Call(ichimoku_psxct, .subset2(data, "unixTime"))
  bucketwidth <- as.numeric(.subset2(data, "bucketWidth"))

  buckets <- `storage.mode<-`(do.call(rbind, .subset2(data, "buckets")), "double")
  xlen <- dim(buckets)[1L]
  df <- `attributes<-`(list(buckets[, "price"],
                            buckets[, "longCountPercent"],
                            buckets[, "shortCountPercent"]),
                       list(names = c("price", "long", "short"),
                            class = "data.frame",
                            row.names = .set_row_names(xlen),
                            instrument = instrument,
                            timestamp = timestamp,
                            currentprice = currentprice,
                            bucketwidth = bucketwidth))

  pdata <- df[trunc(xlen * 0.25):trunc(xlen * 0.75), ]

  layers <- list(
    layer(geom = GeomCol, mapping = aes(x = .data$price, y = .data$long),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#1aa1a6", fill = "#1aa1a6"),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomCol, mapping = aes(x = .data$price, y = -.data$short),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#586e75", fill = "#586e75"),
          inherit.aes = TRUE, check.aes = FALSE, check.param = FALSE),
    layer(geom = GeomVline, data = NULL, mapping = aes(xintercept = currentprice),
          stat = StatIdentity, position = PositionIdentity,
          params = list(na.rm = FALSE, colour = "#db4525", alpha = 0.5),
          inherit.aes = FALSE, check.aes = FALSE, check.param = FALSE),
    scale_x_continuous(breaks = function(x) pretty.default(x, n = 40L)),
    scale_y_continuous(),
    labs(x = "Price", y = "% short / % long",
         title = paste0("OANDA Order Book: ", instrument, " at ",
                        format.POSIXct(timestamp), " / Current Price: ", currentprice)),
    coord_flip(),
    theme_ichimoku_light()
  )

  print(ggplot(data = pdata) + layers)
  invisible(df)

}

