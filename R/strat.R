# Ichimoku - Strategy Layer ----------------------------------------------------

#' Create Ichimoku Strategies
#'
#' Create ichimoku cloud strategies using the indicator condition 'long / short
#'     while c1 > c2'. Complex strategies can be formulated as combined
#'     'c1 > c2 & c3 > c4' (both conditions must be satisfied) or asymmetric
#'     'c1 > c2 x c3 > c4' (where 'c1 > c2' denotes the entry and 'c3 > c4' the
#'     exit indicator).
#'
#' @param x an ichimoku object.
#' @param c1 [default 'close'] column name specified as a string.
#' @param c2 [default 'tenkan'] column name specified as a string.
#' @param c3 (optional) column name specified as a string.
#' @param c4 (optional) column name specified as a string.
#' @param dir [default 'long'] trade direction, either 'long' or 'short'.
#' @param type [default 2] if 'c3' and 'c4' are specified, type 2 will create the
#'     combined strategy 'c1 > c2 & c3 > c4' whilst type 3 will create the
#'     asymmetric strategy 'c1 > c2 x c3 > c4'.
#'
#' @return An ichimoku object augmented with the strategy.
#'
#' @details The following assumption applies to all strategies: confirmation of
#'     whether a condition is satisfied is received at the 'close' of a particular
#'     period, and a transaction is initiated at the immediately following 'open'.
#'     All transactions occur at the 'open'.
#'
#'     By default, the periods in which the strategy results in a position is
#'     shaded on the ichimoku cloud chart and the strategy is printed as the
#'     chart message (if not otherwise specified). To turn off this behaviour,
#'     pass the \code{strat = FALSE} argument to plot() or iplot().
#'
#' @section Ichimoku Object Specification for Strategies:
#'
#'     The ichimoku object is augmented with the following additional elements:
#'
#'     Columns [numeric]:
#'     \itemize{
#'         \item{\code{$cond}:} {a boolean vector if the indicator condition is met}
#'         \item{\code{$posn}:} {a boolean vector indicating if a position is held}
#'         \item{\code{$txn}:} {a vector representing the transactions to implement
#'         the position (1 = enter position, -1 = exit position)}
#'         \item{\code{$logret}:} {a vector of log returns}
#'         \item{\code{$slogret}:} {a vector of log returns for the strategy}
#'         \item{\code{$ret}:} {a vector of discrete returns}
#'         \item{\code{$sret}:} {a vector of of discrete returns for the strategy}
#'      }
#'
#'     Attributes:
#'     \itemize{
#'         \item{\code{$strat}:} {the strategy summary [matrix]}
#'      }
#'
#'     The strategy summary may be accessed by the \code{summary()} method for
#'     ichimoku objects or via \code{\link{look}}.
#'
#' @section Complex Strategies:
#'     For complex strategies: 's1' denotes the strategy 'c1 > c2' and 's2'
#'     denotes the strategy 'c3 > c4'.
#'
#'     \itemize{
#'         \item{Combined strategy 's1 & s2':} {indicator conditions in 's1' and
#'         's2' have to be met simulateneously for a position to be taken. The
#'         column \code{$cond} will show when both conditions are met.}
#'         \item{Asymmetric strategy 's1 x s2':} {indicator condition in 's1' has
#'         to be met to enter a position, and indicator condition in 's2' to exit
#'         a position. These rules are applied recursively over the length of the
#'         data. The column \code{$cond} will show when the indicator condition
#'         is met in 's1'}
#'      }
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' strat <- strat(cloud, c1 = "tenkan", c2 = "cloudB", dir = "short")
#' summary(strat)
#' plot(strat)
#'
#' strat2 <- strat(cloud, c1 = "cloudT", c2 = "kijun", c3 = "cloudT", c4 = "close")
#' summary(strat2)
#' plot(strat2)
#'
#' @export
#'
strat <- function(x,
                  c1 = c("close", "chikou", "open", "high", "low", "tenkan",
                         "kijun", "senkouA", "senkouB", "cloudT", "cloudB"),
                  c2 = c("tenkan", "kijun", "senkouA", "senkouB", "cloudT",
                         "cloudB", "chikou", "close", "open", "high", "low"),
                  c3 = c("close", "chikou", "open", "high", "low", "tenkan",
                         "kijun", "senkouA", "senkouB", "cloudT", "cloudB"),
                  c4 = c("tenkan", "kijun", "senkouA", "senkouB", "cloudT",
                         "cloudB", "chikou", "close", "open", "high", "low"),
                  dir = c("long", "short"),
                  type = 2) {

  if (!is.ichimoku(x)) stop("strat() only works on ichimoku objects", call. = FALSE)
  c1 <- match.arg(c1)
  c2 <- match.arg(c2)
  dir <- match.arg(dir)
  p2 <- attr(x, "periods")[2L]

  core <- coredata(x)
  xlen <- dim(core)[1L]
  end <- xlen - p2
  offset <- (p2 - 1L) * (c1 == "chikou" || c2 == "chikou")

  if (missing(c3) || missing(c4) || (identical(c1, c3) && identical(c2, c4))) {
    strategy <- paste0(c1, " > ", c2)
    cond <- c(rep(NA, offset), (core[, c1] > core[, c2])[1:(xlen - offset)])
    posn <- c(NA, cond[1:(end - 1L)], rep(NA, p2))

  } else if (type == 2) {
    c3 <- match.arg(c3)
    c4 <- match.arg(c4)
    strategy <- paste0(c1, " > ", c2, " & ", c3, " > ", c4)

    s1cond <- c(rep(NA, offset), (core[, c1] > core[, c2])[1:(xlen - offset)])
    s1posn <- c(NA, s1cond[1:(end - 1L)], rep(NA, p2))
    offset2 <- p2 * (c3 == "chikou" || c4 == "chikou")
    s2cond <- c(rep(NA, offset2), (core[, c3] > core[, c4])[1:(xlen - offset2)])
    s2posn <- c(NA, s2cond[1:(end - 1L)], rep(NA, p2))
    cond <- s1cond * s2cond
    posn <- s1posn * s2posn

  } else if (type == 3) {
    c3 <- match.arg(c3)
    c4 <- match.arg(c4)
    strategy <- paste0(c1, " > ", c2, " x ", c3, " > ", c4)

    cond <- c(rep(NA, offset), (core[, c1] > core[, c2])[1:(xlen - offset)])
    s1posn <- c(NA, cond[1:(end - 1L)], rep(NA, p2))
    s1txn <- c(NA, diff(s1posn))
    s1txn[s1posn == 1 & is.na(s1txn)] <- 1
    if (s1posn[end] == 1) s1txn[end + 1L] <- -1

    offset2 <- (p2 - 1L) * (c3 == "chikou" || c4 == "chikou")
    s2cond <- c(rep(NA, offset2), (core[, c3] > core[, c4])[1:(xlen - offset2)])
    s2posn <- c(NA, s2cond[1:(end - 1L)], rep(NA, p2))
    s2txn <- c(NA, diff(s2posn))
    s2txn[s2posn == 1 & is.na(s2txn)] <- 1
    if (s2posn[end] == 1) s2txn[end + 1L] <- -1

    s1entry <- which(s1txn == 1)
    s2exit <- which(s2txn == 1)
    s2exit <- s2exit[s2exit > s1entry[1L]]

    posn <- integer(xlen)
    posn[is.na(s1posn)] <- NA
    while (length(s1entry) > 0 && length(s2exit) > 0) {
      posn[s1entry[1L]:(s2exit[1L] - 1L)] <- 1L
      s1entry <- s1entry[s1entry > s2exit[1L]]
      s2exit <- s2exit[s2exit > s1entry[1L]]
    }

  } else stop("Invalid type specified", call. = FALSE)

  txn <- c(NA, diff(posn))
  txn[posn == 1 & is.na(txn)] <- 1
  if (posn[end] == 1) txn[end + 1L] <- -1
  if (!sum(txn, na.rm = TRUE) == 0) stop("Calculation error - please check validity of data",
                                         call. = FALSE)

  logret <- c(diff(log(core[, "open"])), NA)
  if (dir == "short") logret <- -logret
  logret[is.na(posn)] <- NA
  slogret <- logret * posn

  x$cond <- cond
  x$posn <- posn
  x$txn <- txn
  x$logret <- logret
  x$slogret <- slogret
  x$ret <- exp(logret) - 1
  x$sret <- exp(slogret) - 1

  writeStrat(x = x, strategy = strategy, dir = dir)

}

#' writeStrat
#'
#' Internal function used by ichimoku to write strategy summaries to ichimoku
#'     objects.
#'
#' @param x an ichimoku object augmented with strategy columns.
#' @param strategy string describing strategy rule.
#' @param dir trade direction, either 'long' or 'short'.
#'
#' @return An ichimoku object with the strategy summary set as the attribute
#'     'strat'.
#'
#' @details The stategy summary may subsequently be accessed by the summary()
#'     function or via \code{\link{look}}.
#'
#' @noRd
#'
writeStrat <- function(x, strategy, dir) {

  p2 <- attr(x, "periods")[2L]
  index <- index(x)
  core <- coredata(x)
  xlen <- dim(core)[1L]
  start <- xlen - sum(!is.na(core[, "posn"])) - p2 + 1L
  end <- xlen - p2

  openvec <- na.omit(core[core[, "txn"] == 1, "open"])
  closevec <- na.omit(core[core[, "txn"] == -1, "open"])
  trades <- switch(dir,
                   long = (closevec - openvec) / openvec,
                   short = (openvec - closevec) / openvec)
  tlen <- length(trades)

  attr(x, "strat") <- cbind(list(
    Strategy = strategy,
    `---------------------` = "----------",
    `Strategy cuml return %` = round((exp(sum(core[start:end, "slogret"])) - 1) * 100, 2),
    `Per period mean ret %` = round((exp(mean(core[start:end, "slogret"])) - 1) * 100, 4),
    `Periods in market` = sum(core[start:end, "posn"]),
    `Total trades` = tlen,
    `Average trade length` = round(sum(core[start:end, "posn"]) / tlen, 2),
    `Trade success %` = round(length(trades[trades > 0]) / tlen * 100, 2),
    `Worst trade ret %` = round(min(trades) * 100, 2),
    `---------------------` = "----------",
    `Benchmark cuml ret %` = round((exp(sum(core[start:end, "logret"])) - 1) * 100, 2),
    `Per period mean ret %` =  round((exp(mean(core[start:end, "logret"])) - 1) * 100, 4),
    `Periods in market` = end - start + 1L,
    `---------------------` = "----------",
    Direction = dir,
    Start = index[start],
    End = index[end],
    Ticker = attr(x, "ticker")
  ))

  x

}

#' Combine Ichimoku Strategies
#'
#' Create custom strategies from combining existing strategies contained in
#'     ichimoku objects 's1' and 's2' to form 's1 & s2'.
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
#'     The strategy summary may be accessed by the \code{summary()} method for
#'     ichimoku objects or via \code{\link{look}}.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' strat1 <- strat(cloud, c1 = "close", c2 = "kijun")
#' strat2 <- strat(cloud, c1 = "cloudB", c2 = "tenkan")
#' cstrat <- stratcombine(strat1, strat2)
#' summary(cstrat)
#' plot(cstrat)
#'
#' @export
#'
stratcombine <- function(s1, s2) {

  if (!is.ichimoku(s1) || !is.ichimoku(s2) || !hasStrat(s1) || !hasStrat(s2)) {
    stop("stratcombine() only works on ichimoku objects containing strategies", call. = FALSE)
  }
  core1 <- coredata(s1)
  core2 <- coredata(s2)
  if (!identical(core1[, c("high", "low", "close")], core2[, c("high", "low", "close")])) {
    stop("Strategies must be for the same data", call. = FALSE)
  }
  dir <- attr(s1, "strat")["Direction", ][[1L]]
  if (!identical(dir, attr(s2, "strat")["Direction", ][[1L]])) {
    stop("Trade direction must be the same for all strategies", call. = FALSE)
  }
  strat1 <- attr(s1, "strat")["Strategy", ][[1L]]
  strat2 <- attr(s2, "strat")["Strategy", ][[1L]]
  if (identical(strat1, strat2)) return(s1)

  strategy <- paste0(strat1, " & ", strat2)
  p2 <- attr(s1, "periods")[2L]
  xlen <- dim(core1)[1L]
  end <- xlen - p2

  cond <- core1[, "cond"] * core2[, "cond"]
  posn <- core1[, "posn"] * core2[, "posn"]
  txn <- c(NA, diff(posn))
  txn[posn == 1 & is.na(txn)] <- 1
  if (posn[end] == 1) txn[end + 1L] <- -1
  if (!sum(txn, na.rm = TRUE) == 0) stop("Calculation error - please check validity of data",
                                         call. = FALSE)
  slogret <- s1[, "logret"] * posn

  s1$cond <- cond
  s1$posn <- posn
  s1$txn <- txn
  s1$slogret <- slogret
  s1$sret <- exp(slogret) - 1

  writeStrat(x = s1, strategy = strategy, dir = dir)

}

#' hasStrat
#'
#' A function for checking if an object contains a strategy.
#'
#' @param x an object.
#'
#' @return A logical value of TRUE if the 'strat' attribute of 'x' is set,
#'     otherwise FALSE.
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

