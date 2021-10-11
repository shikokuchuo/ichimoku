# Ichimoku - Strategy + ML Layer -----------------------------------------------

#' Automated Ichimoku Strategies
#'
#' Generate a list of the top performing ichimoku cloud strategies based on
#'     simple indicator conditions of the form 'c1 > c2' (level 1), complex
#'     combined strategies of the form 'c1 > c2 & c3 > c4' (level 2), or complex
#'     asymmetric strategies of the form 'c1 > c2 x c3 > c4' (level 3).
#'
#' @inheritParams strat
#' @param n [default 8] select top 'n' number of strategies to return.
#' @param level [default 1] to return simple strategies. For complex strategies,
#'     set level to 2 to return combined strategies of the form 's1 & s2' or
#'     level to 3 to return asymmetric strategies of the form 's1 x s2'
#'
#' @return Returned invisibly, a list of 'n' ichimoku objects containing strategies,
#'     with attributes 'logret' (a vector of cumulative log returns for all
#'     strategies) and 'summary' (a matrix of summaries for the top 'n'
#'     strategies). The strategy summaries are printed to the console.
#'
#' @details Ichimoku objects for each strategy are returned as a list. The
#'     cumulative log returns for all strategies as well as the summaries for
#'     the top 'n' strategies are saved as attributes to the list. This
#'     information may be retrieved by using \code{\link{look}} on the returned
#'     list.
#'
#'     Each individual ichimoku object may be accessed via its position in the
#'     list, e.g. [[1]] for the 1st item, or by using \code{\link{look}}
#'     specifying the parameter 'which'.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by calling:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#' strat <- look(stratlist, which = 1)
#' summary(strat)
#'
#' autostrat(cloud, n = 1, dir = "short", level = 2)
#' autostrat(cloud, n = 1, dir = "long", level = 3)
#'
#' @export
#'
autostrat <- function(x,
                      n = 8,
                      dir = c("long", "short"),
                      level = 1) {

  is.ichimoku(x) || stop("autostrat() only works on ichimoku objects", call. = FALSE)
  dir <- match.arg(dir)
  if (!level %in% 1:3) {
    warning("Specified 'level' invalid - reverting to default of 1", call. = FALSE)
    level <- 1
  }

  grid <- mlgrid(x, y = "logret", dir = dir, type = "boolean", unique = FALSE)

  if (level == 1) {
    matrix <- grid[, 1L] * grid[, -1L]
    logret <- (cs <- colSums(matrix))[order(cs, decreasing = TRUE)]
    returns <- logret[!logret == 0]
    args <- do.call(rbind, strsplit(names(returns[1:n]), "_", fixed = TRUE))
    list <- mapply(strat, c1 = args[, 1L], c2 = args[, 2L],
                   MoreArgs = list(x = x, dir = dir),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)

  } else if (level == 2) {
    lgrid <- grid[, -1L]
    w <- length(lgrid)
    pairs <- expand.grid(seq_len(w), seq_len(w), KEEP.OUT.ATTRS = FALSE)[-grid_dup(w), ]
    mgrid <- do.call(cbind,
                     mapply(function(a, b) lgrid[, a] * lgrid[, b],
                            a = pairs[, 1L], b = pairs[, 2L],
                            SIMPLIFY = FALSE, USE.NAMES = FALSE))
    dimnames(mgrid)[[2L]] <- do.call(c,
                                     mapply(function(a, b) paste0(names(lgrid)[a], "&", names(lgrid)[b]),
                                            a = pairs[, 1L], b = pairs[, 2L],
                                            SIMPLIFY = FALSE, USE.NAMES = FALSE))
    matrix <- grid[, 1L] * mgrid
    logret <- (cs <- colSums(matrix))[order(cs, decreasing = TRUE)]
    returns <- logret[!logret == 0]
    args <- do.call(rbind, strsplit(names(returns[1:n]), "&|_", perl = TRUE))
    list <- mapply(strat, c1 = args[, 1L], c2 = args[, 2L], c3 = args[, 3L], c4 = args[, 4L],
                   MoreArgs = list(x = x, dir = dir, type = 2),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)

  } else {
    lgrid <- grid[, -1L]
    w <- length(lgrid)
    pairs <- expand.grid(seq_len(w), seq_len(w), KEEP.OUT.ATTRS = FALSE)[-grid_dup(w, omit.id = TRUE), ]
    mgrid <- do.call(cbind,
                     mapply(function(a, b) {
                       xlen <- dim(lgrid)[1L]
                       s1posn <- lgrid[, a]
                       s1txn <- c(if (s1posn[1L] == 1) 1 else 0,
                                  diff(s1posn)[-1L],
                                  if (s1posn[xlen] == 1) -1 else 0)
                       s2posn <- lgrid[, b]
                       s2txn <- c(0,
                                  diff(s2posn)[-1L],
                                  if (s2posn[xlen] == 1) -1 else 0)
                       s1entry <- which(s1txn == 1)
                       s2exit <- which(s2txn == 1)
                       position <- integer(xlen)
                       while (length(s1entry) > 0 && length(s2exit) > 0) {
                         position[s1entry[1L]:(s2exit[1L] - 1L)] <- 1L
                         s1entry <- s1entry[s1entry > s2exit[1L]]
                         s2exit <- s2exit[s2exit > s1entry[1L]]
                       }
                       position
                     },
                     a = pairs[, 1L], b = pairs[, 2L],
                     SIMPLIFY = FALSE, USE.NAMES = FALSE))
    dimnames(mgrid)[[2L]] <- do.call(c,
                                     mapply(function(a, b) paste0(names(lgrid)[a], "x", names(lgrid)[b]),
                                            a = pairs[, 1L], b = pairs[, 2L],
                                            SIMPLIFY = FALSE, USE.NAMES = FALSE))
    matrix <- grid[, 1L] * mgrid
    logret <- (cs <- colSums(matrix))[order(cs, decreasing = TRUE)]
    returns <- logret[!logret == 0]
    args <- do.call(rbind, strsplit(names(returns[1:n]), "x|_", perl = TRUE))
    list <- mapply(strat, c1 = args[, 1L], c2 = args[, 2L], c3 = args[, 3L], c4 = args[, 4L],
                   MoreArgs = list(x = x, dir = dir, type = 3),
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)

  }

  attributes(list) <- list(logret = logret,
                           summary = print(do.call(cbind, lapply(list, attr, which = "strat"))),
                           autostrat = TRUE)
  invisible(list)

}

#' mlgrid Numeric Representation
#'
#' Create a grid of ichimoku indicator conditions and next period returns. The
#'     grid facilitates the comparison of strategy returns and provides a basis
#'     for use in machine learning applications. Translates the visual
#'     representation of the relationship between cloud chart elements into a
#'     numerical format for further analysis.
#'
#' @inheritParams strat
#' @param y [default 'logret'] choose target variable 'logret' (log returns) or
#'     'ret' (discrete returns).
#' @param type [default 'boolean'] either 'boolean' or 'numeric'. 'boolean'
#'     creates a grid of dummy variables for ichimoku indicator conditions of
#'     the form 1 if c1 > c2, 0 otherwise. 'numeric' creates a grid of the
#'     numeric difference c1 - c2.
#' @param unique [default TRUE] to return only unique combinations of c1 and c2.
#'     Set to FALSE to return both c1 > c2 and c2 > c1.
#'
#' @return A data.frame in a 'tidy' format with one observation per row and one
#'     feature per column with the target 'y' as the first column.
#'
#'     The 'y' parameter and trade direction are set as atrributes. To view these,
#'     use \code{\link{look}} on the returned object.
#'
#' @details The date-time index corresponds to when the indicator condition is
#'     met at the close for that period. The return is the single-period return
#'     achieved by transacting at the immediately following opening price until
#'     the next opening price.
#'
#'     Only valid combinations are included. This excludes any combination
#'     involving 'open' as it is in effect a lagged indicator and not
#'     contemporaneous. The following trivial or highly-collinear pairs are also
#'     excluded: (high, close), (low, close), (low, high), (cloudTop, Senkou A),
#'     (cloudBase, senkou A), (cloudTop, senkouB), (cloudBase, senkouB),
#'     (cloudBase, cloudTop).
#'
#' @section Further Details:
#'     mlgrid is used by \code{\link{autostrat}} to enumerate the returns for all
#'     valid strategy combinations.
#'
#'     Please refer to the strategies vignette by calling:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' grid <- mlgrid(cloud, y = "ret", dir = "short", type = "numeric")
#' str(grid)
#'
#' @export
#'
mlgrid <- function(x,
                   y = c("logret", "ret"),
                   dir = c("long", "short"),
                   type = c("boolean", "numeric"),
                   unique = TRUE) {

  is.ichimoku(x) || stop("mlgrid() only works on ichimoku objects", call. = FALSE)
  target <- match.arg(y)
  dir <- match.arg(dir)
  type <- match.arg(type)

  core <- coredata.ichimoku(x)
  xlen <- dim(core)[1L]
  p2 <- attr(x, "periods")[2L]
  y <- c(diff(log(core[, "open"]))[2:(xlen - 1L)], NA, NA)
  if (dir == "short") y <- -y
  if (target == "ret") y <- exp(y) - 1

  cols <- c("chikou", "close", "high", "low", "tenkan", "kijun", "senkouA", "senkouB", "cloudT", "cloudB")
  pairs <- list(character(37L), character(37L))
  ctr <- 0L
  for (i in 1:7) {
    colm <- cols[(i + 1L):10]
    for (j in seq_along(colm)) {
      colsi <- cols[i]
      colmj <- colm[j]
      if(colsi == "close" && (colmj == "high" || colmj == "low") ||
         colsi == "high" && colmj == "low") next
      ctr <- ctr + 1L
      pairs[[1]][ctr] <- colsi
      pairs[[2]][ctr] <- colmj
      if (colsi == "senkouA" && colmj == "senkouB") break
    }
  }
  veclist <- writeVectors(x = core, pairs = pairs, p2 = p2, xlen = xlen, type = type)

  if (!isTRUE(unique)) {
    pairs <- list(pairs[[2L]], pairs[[1L]])
    veclistf <- writeVectors(x = core, pairs = pairs, p2 = p2, xlen = xlen, type = type)
    veclist <- c(veclist, veclistf)
  }

  df <- c(list(y = y), veclist)
  cnames <- attr(df, "names")
  attributes(df) <- list(names = cnames,
                         class = "data.frame",
                         row.names = format.POSIXct(index.ichimoku(x)))
  grid <- df_trim(df)
  attributes(grid) <- list(names = cnames,
                           class = "data.frame",
                           row.names = attr(grid, "row.names"),
                           y = target,
                           direction = dir,
                           ticker = attr(x, "ticker"),
                           mlgrid = TRUE)
  grid

}

#' writeVectors
#'
#' Internal function used by mlgrid to create vectors of ichimoku representations.
#'
#' @param x an ichimoku object or coredata matrix of an ichimoku object.
#' @param pairs a 2-vector list of column name pairs.
#' @param p2 length of second ichimoku period.
#' @param xlen number of rows of 'x'.
#' @param type type of vectors to return, 'boolean' or 'numeric'.
#'
#' @return A named list of equal-length vectors.
#'
#' @noRd
#'
writeVectors <- function(x, pairs, p2, xlen, type) {

  setNames(mapply(function(c1, c2) {
    offset <- (p2 - 1L) * (c1 == "chikou" | c2 == "chikou")
    switch(type,
           boolean = as.integer(c(rep(NA, offset), (x[, c1] > x[, c2])[1:(xlen - offset)])),
           numeric = c(rep(NA, offset), (x[, c1] - x[, c2])[1:(xlen - offset)]))
  }, c1 = pairs[[1L]], c2 = pairs[[2L]], SIMPLIFY = FALSE, USE.NAMES = FALSE),
  do.call(c, mapply(function(c1, c2) paste0(c1, "_", c2),
                    c1 = pairs[[1L]], c2 = pairs[[2L]], SIMPLIFY = FALSE, USE.NAMES = FALSE)))

}

