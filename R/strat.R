# Ichimoku - Strategy Layer ----------------------------------------------------

#' Create Ichimoku Strategies
#'
#' Create custom ichimoku cloud strategies using the indicator condition 'long
#'     (or short) while c1 > c2'.
#'
#' @param x an ichimoku object.
#' @param c1 [default 'close'] column name specified as a string.
#' @param c2 [default 'tenkan'] column name specified as a string.
#' @param dir [default 'long'] trade direction, either 'long' or 'short'.
#'
#' @return An ichimoku object augmented with the strategy.
#'
#' @details The following assumption applies to all strategies: confirmation of
#'     whether a condition is satisfied is received at the 'close' of a particular
#'     period, and a transaction is initiated at the next 'open'. All transactions
#'     happen at the 'open'.
#'
#'     The original ichimoku object 'x' is augmented with the following
#'     additional columns used to calculate the strategy:
#'
#'     'cond', a boolean vector if the indicator condition is met, 'posn', a
#'     boolean vector indicating if a position is held, and 'txn', a vector
#'     representing the transactions to implement the position.
#'
#'     logret' is a column of log returns, 'slogret' is a column of log returns
#'     for the strategy.
#'
#'     ret' is a column of discrete returns, and 'sret' is a column of discrete
#'     returns for the strategy.
#'
#'     The strategy summary is saved as an object attribute and may be accessed
#'     via the summary() function.
#'
#'     The periods in which the strategy results in a position is shaded by
#'     default on the ichimoku cloud chart. To turn off this behaviour, pass the
#'     'strat = FALSE' argument to plot().
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' strat <- strat(cloud, c1 = "close", c2 = "tenkan")
#' summary(strat)
#' plot(strat)
#'
#' @export
#'
strat <- function(x,
                  c1 = c("close", "chikou", "open", "high", "low", "tenkan",
                         "kijun", "senkouA", "senkouB", "cloudTop", "cloudBase"),
                  c2 = c("tenkan", "kijun", "senkouA", "senkouB", "cloudTop",
                         "cloudBase", "chikou", "close", "open", "high", "low"),
                  dir = c("long", "short")
                  ) {

  if(!is.ichimoku(x)) stop("strat() only works on ichimoku objects", call. = FALSE)
  c1 <- match.arg(c1)
  c2 <- match.arg(c2)
  dir <- match.arg(dir)
  strategy <- paste0(c1, " > ", c2)
  xlen <- dim(x)[1L]
  p2 <- attr(x, "periods")[2L]
  end <- xlen - p2 - 1L
  offset <- p2 * (c1 == "chikou" | c2 == "chikou")
  x$cond <- c(rep(NA, offset), (x[, c1] > x[, c2])[1:(xlen - offset)])
  x$posn <- c(NA, x[1:(end - 1L), "cond"], rep(NA, p2 + 1L))
  x$txn <- diff(x$posn)
  x[x$posn == 1 & is.na(x$txn), "txn"] <- 1
  if(x[end, "posn"] == 1) x[end + 1L, "txn"] <- -1
  x$logret <- c(diff(log(coredata(x$open))), NA)
  if(dir == "short") x$logret <- -x$logret
  x$logret[is.na(x$posn)] <- NA
  x$slogret <- x$logret * x$posn
  x$ret <- exp(x$logret) - 1
  x$sret <- exp(x$slogret) - 1
  start <- xlen - length(x[!is.na(x$posn), "posn"]) - p2

  writeStrat(x, strategy = strategy, dir = dir, start = start, end = end)

}

#' writeStrat
#'
#' Internal function used by ichimoku to write strategy summaries to ichimoku
#'     objects.
#'
#' @param x an ichimoku object.
#' @param strategy string describing strategy rule.
#' @param dir trade direction, either 'long' or 'short'.
#' @param start integer position of start date in index.
#' @param end integer position of end date in index.
#'
#' @return An ichimoku object with the strategy summary set as the attribute
#'     'strat'.
#'
#' @details The stategy summary may subsequently be accessed via the summary()
#'     function.
#'
#' @keywords internal
#'
writeStrat <- function(x, strategy, dir, start, end) {
  trades <- (coredata(x[x$txn == -1, "open"]) - coredata(x[x$txn == 1, "open"])) /
    coredata(x[x$txn == 1, "open"])
  tlen <- length(trades)
  attr(x, "strat") <- cbind(list(
    Strategy = strategy,
    `---------------------` = "----------",
    `Strategy cuml return %` = round((exp(sum(x[start:end, "slogret"])) - 1) * 100, 2),
    `Per period mean ret %` = round((exp(mean(x[start:end, "slogret"])) - 1) * 100, 4),
    `Periods in market` = sum(x[start:end, "posn"]),
    `Total trades` = tlen,
    `Average trade length` = round(sum(x[start:end, "posn"]) / tlen, 2),
    `Trade success %` = round(length(trades[trades > 0]) / tlen * 100, 2),
    `Worst trade ret %` = round(min(trades) * 100, 2),
    `---------------------` = "----------",
    `Benchmark cuml ret %` = round((exp(sum(x[start:end, "logret"])) - 1) * 100, 2),
    `Per period mean ret %` =  round((exp(mean(x[start:end, "logret"])) - 1) * 100, 4),
    `Periods in market` = end - start + 1L,
    `---------------------` = "----------",
    Direction = dir,
    Start = index(x)[start],
    End = index(x)[end],
    Ticker = attr(x, "ticker")
  ))
  x
}

#' Combine Ichimoku Strategies
#'
#' Create more complex strategies with an indicator condition of the form
#'     'c1 > c2 & c3 > c4' by combining existing strategies with indicator
#'     conditions 'c1 > c2' and 'c3 > c4' respectively.
#'
#' @param s1 an ichimoku object containing a strategy.
#' @param s2 an ichimoku object containing a strategy.
#'
#' @return An ichimoku object augmented with the combined strategy.
#'
#' @details The combined strategy 's1 & s2' means indicator conditions in 's1'
#'     and 's2' have to be met simulateneously for a trade position to be taken.
#'
#'     The boolean values showing whether these conditions are met are stored in
#'     the 'cond' column.
#'
#'     The stategy summary may be accessed via the summary() function.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' strat1 <- strat(cloud, c1 = "close", c2 = "kijun")
#' strat2 <- strat(cloud, c1 = "cloudBase", c2 = "tenkan")
#' cstrat <- stratcombine(strat1, strat2)
#' summary(cstrat)
#' plot(cstrat)
#'
#' @export
#'
stratcombine <- function(s1, s2) {

  if(!is.ichimoku(s1) || !is.ichimoku(s2) || !hasStrat(s1) || !hasStrat(s2)) {
    stop("stratcombine() only works on ichimoku objects containing strategies", call. = FALSE)
  }
  if(!identical(coredata(s1[, 1:4]), coredata(s2[, 1:4]))) {
    stop("Strategies must be for the same data", call. = FALSE)
  }
  dir <- attr(s1, "strat")["Direction", ]$Direction
  if(!identical(dir, attr(s2, "strat")["Direction", ]$Direction)) {
    stop("Trade direction must be the same for all strategies", call. = FALSE)
  }

  strat1 <- attr(s1, "strat")["Strategy", ]$Strategy
  strat2 <- attr(s2, "strat")["Strategy", ]$Strategy
  if(identical(strat1, strat2)) return(s1)

  strategy <- paste0(strat1, " & ", strat2)
  xlen <- dim(s1)[1L]
  p2 <- attr(s1, "periods")[2L]
  end <- xlen - p2 - 1L
  s1$cond <- s1$cond * s2$cond
  s1$posn <- s1$posn * s2$posn
  s1$txn <- diff(s1$posn)
  s1[s1$posn == 1 & is.na(s1$txn), "txn"] <- 1
  if(s1[end, "posn"] == 1) s1[end + 1L, "txn"] <- -1
  s1$logret <- c(diff(log(coredata(s1$open))), NA)
  if(dir == "short") s1$logret <- -s1$logret
  s1$logret[is.na(s1$posn)] <- NA
  s1$slogret <- s1$logret * s1$posn
  s1$ret <- exp(s1$logret) - 1
  s1$sret <- exp(s1$slogret) - 1
  start <- xlen - length(s1[!is.na(s1$posn), "posn"]) - p2

  writeStrat(s1, strategy = strategy, dir = dir, start = start, end = end)

}

#' Automated Ichimoku Strategies
#'
#' Generate a list of the top performing ichimoku cloud strategies based on
#'     indicator conditions of the form 'c1 > c2' or level 2 strategies based on
#'     combined indicator conditions of the form 'c1 > c2 & c3 > c4'.
#'
#' @inheritParams strat
#' @param n [default 8] select top n number of strategies to return.
#' @param level [default 1] set to 2 to also return combined strategies.
#'
#' @return A list of 'n' ichimoku objects containing strategies. The
#'     cumulative log returns for all strategies as well as the summaries for
#'     the 'n' top strategies are saved as attributes to the list. The strategy
#'     summaries are printed to the console as a side effect.
#'
#' @details Ichimoku objects for each strategy are returned as a list. The
#'     cumulative log returns for all strategies as well as the summaries for
#'     the 'n' top strategies are saved as attributes to the list. This
#'     information may be viewed by using look() on the returned list.
#'
#'     Each individual ichimoku object may be accessed via its position in the
#'     list, e.g. [[1]] for the 1st item, or by using \code{\link{look}}
#'     specifying the parameter 'which'.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#' summary(look(stratlist, which = 1))
#'
#' autostrat(cloud, n = 1, dir = "short", level = 2)
#'
#' @export
#'
autostrat <- function(x, n = 8, dir = c("long", "short"), level = 1) {

  if(!is.ichimoku(x)) stop("autostrat() only works on ichimoku objects", call. = FALSE)
  dir <- match.arg(dir)
  grid <- mlgrid(x, y = "logret", dir = dir, type = "boolean", unique = FALSE)
  if(identical(level, 2)) {
    lgrid <- grid[, -1L]
    w <- dim(lgrid)[2L]
    pairs <- expand.grid(seq_len(w), seq_len(w), KEEP.OUT.ATTRS = FALSE)[-grid_dup(w), ]
    mgrid <- do.call(cbind,
                     mapply(function(a, b) lgrid[, a] * lgrid[, b],
                            a = pairs[, 1L], b = pairs[, 2L],
                            SIMPLIFY = FALSE, USE.NAMES = FALSE))
    colnames(mgrid) <- do.call(c,
                               mapply(function(a, b) paste0(colnames(lgrid)[a], "&", colnames(lgrid)[b]),
                                      a = pairs[, 1L], b = pairs[, 2L],
                                      SIMPLIFY = FALSE, USE.NAMES = FALSE))
    matrix <- grid[, 1L] * mgrid
    logret <- sort(colSums(matrix), decreasing = TRUE)
    returns <- logret[!logret == 0]
    args <- do.call(rbind, strsplit(names(returns[1:n]), "&|_"))
    list1 <- mapply(strat, c1 = args[, 1L], c2 = args[, 2L],
                    MoreArgs = list(x = x, dir = dir), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    list2 <- mapply(strat, c1 = args[, 3L], c2 = args[, 4L],
                    MoreArgs = list(x = x, dir = dir), SIMPLIFY = FALSE, USE.NAMES = FALSE)
    list <- mapply(stratcombine, s1 = list1, s2 = list2, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    matrix <- grid[, 1L] * grid[, -1L]
    logret <- sort(colSums(matrix), decreasing = TRUE)
    returns <- logret[!logret == 0]
    args <- do.call(rbind, strsplit(names(returns[1:n]), "_", fixed = TRUE))
    list <- mapply(strat, c1 = args[, 1L], c2 = args[, 2L], MoreArgs = list(x = x, dir = dir),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  attr(list, "logret") <- cbind(logret)
  attr(list, "summary") <- print(do.call(cbind, lapply(list, attr, which = "strat")))
  attr(list, "autostrat") <- TRUE
  invisible(list)
}

#' Summary of Ichimoku Strategies
#'
#' Custom summary method for ichimoku objects for viewing strategies.
#'
#' @param object an object of class 'ichimoku'.
#' @param strat [default TRUE] to show the strategy summary if present. Set to
#'     FALSE to show the data summary instead.
#' @param ... additional arguments to be passed along.
#'
#' @return A matrix containing the strategy summary if present, otherwise a table
#'     containing the data summary.
#'
#' @details This function is an S3 method for the generic function summary() for
#'     class 'ichimoku'. It can be invoked by calling summary(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' strat <- strat(ichimoku(sample_ohlc_data, ticker = "TKR"))
#' summary(strat)
#'
#' @method summary ichimoku
#' @export
#'
summary.ichimoku <- function(object, strat = TRUE, ...) {
  if(isTRUE(strat) && hasStrat(object)) attr(object, "strat")
  else NextMethod(summary)
}

#' hasStrat
#'
#' A function for checking if an object contains a strategy.
#'
#' @param x an object.
#'
#' @return A logical value of TRUE if 'x' has a 'strat' attribute set that is
#'     not NULL, otherwise FALSE.
#'
#' @details  Designed to be used by ichimoku functions that are either S3
#'     methods for class 'ichimoku' or after validation that 'x' is an ichimoku
#'     object, hence there is no check on the class of 'x' within this function.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' strat <- strat(cloud)
#'
#' # TRUE:
#' hasStrat(strat)
#' # FALSE:
#' hasStrat(cloud)
#'
#' @export
#'
hasStrat <- function(x) !is.null(attr(x, "strat"))

