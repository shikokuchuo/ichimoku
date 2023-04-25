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
#'     level to 3 to return asymmetric strategies of the form 's1 x s2'.
#' @param quietly (optional) if set to TRUE, will suppress printing of additional
#'     output to the console and return quietly.
#'
#' @return Returned invisibly, a list of 'n' ichimoku objects containing
#'     strategies, with attributes 'logret' (a vector of cumulative log returns
#'     for all strategies) and 'summary' (a matrix of summaries for the top 'n'
#'     strategies).
#'
#'     In addition, the strategy summaries are printed to the console.
#'
#' @details Ichimoku objects for each strategy are returned as a list. The
#'     cumulative log returns for all strategies as well as the summaries for
#'     the top 'n' strategies are saved as attributes to the list. This
#'     information may be retrieved by using \code{\link{look}} on the returned
#'     list.
#'
#'     Each individual ichimoku object may be accessed via its position in the
#'     list, e.g. [[1]] for the 1st item.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by calling:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' stratlist <- autostrat(cloud, n = 3, quietly = TRUE)
#' look(stratlist)
#' strat <- stratlist[[2]]
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
                      level = 1,
                      quietly) {

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
    pairs <- expand.grid(seq_len(w), seq_len(w),
                         KEEP.OUT.ATTRS = FALSE)[-grid_dup(w, omit.id = TRUE), ]
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
                           summary = do.call(cbind, lapply(list, attr, "strat")))
  if (missing(quietly) || !isTRUE(quietly)) print(attr(list, "summary"))
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
#' @param y [default 'logret'] choose target variable 'logret' (log returns),
#'     'ret' (discrete returns), or 'none'.
#' @param k [default 1L] number of periods time horizon over which to calculate
#'     target variable 'y'.
#' @param type [default 'boolean'] choose 'boolean', 'numeric' or 'z-score'.
#'     'boolean' creates a grid of dummy variables for ichimoku indicator
#'     conditions of the form 1 if c1 > c2, 0 otherwise. 'numeric' creates a
#'     grid of the numeric difference c1 - c2. 'z-score' standardises the numeric
#'     grid by the mean and standard deviation of each feature.
#' @param format [default 'dataframe'] select either 'dataframe' or 'matrix' for
#'     the format of returned object.
#' @param unique [default TRUE] to return only unique combinations of c1 and c2.
#'     Set to FALSE to return both c1 > c2 and c2 > c1.
#' @param expr [default list()] (for advanced use only) a named list of
#'     quoted language objects or expressions, which are evaluated internally
#'     within the function, referencing intermediate objects such as:
#'     'core', the coredata matrix of the ichimoku object, 'xlen' the number of
#'     observations, or any of the function parameters etc. Each evaluated
#'     expression must return a vector of length 'xlen' for inclusion in the grid.
#'
#' @return A data.frame or matrix in a 'tidy' format with one observation per
#'     row and one feature per column with the target 'y' as the first column
#'     (unless set to 'none').
#'
#'     The 'y' and 'k' parameters, trade direction and grid type are set as
#'     attributes, with 'means' and 'sdevs' also populated for type 'z-score' to
#'     return the mean and standard deviation for each column. To view these,
#'     use \code{\link{look}} on the returned object.
#'
#' @details The date-time index corresponds to when the indicator condition is
#'     met at the close for that period. The return is the k-period return
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
#' @seealso \code{\link{autostrat}} which uses \code{mlgrid()} to enumerate all
#'     valid return combinations.
#'
#'     \code{\link{relative}} which uses \code{mlgrid()} to relate the latest
#'     observed numeric representation to historical values.
#'
#' @section Further Details:
#'
#'     Please refer to the strategies vignette by calling:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' grid <- mlgrid(cloud, y = "ret", k = 2, dir = "short", type = "z-score")
#' str(grid)
#'
#' custom <- mlgrid(cloud, type = "numeric", expr = list(cd = quote(core[, "cd"])))
#' str(custom)
#'
#' @export
#'
mlgrid <- function(x,
                   y = c("logret", "ret", "none"),
                   k = 1L,
                   dir = c("long", "short"),
                   type = c("boolean", "numeric", "z-score"),
                   format = c("dataframe", "matrix"),
                   unique = TRUE,
                   expr = list()) {

  is.ichimoku(x) || stop("mlgrid() only works on ichimoku objects", call. = FALSE)
  y <- match.arg(y)
  if (!missing(k)) {
    if (is.numeric(k) && k >= 1L) {
      k <- as.integer(k)
    } else {
      warning("Specified value for 'k' invalid - reverting to default of 1L", call. = FALSE)
      k <- 1L
    }
  }
  dir <- match.arg(dir)
  type <- match.arg(type)
  format <- match.arg2(format, c("dataframe", "matrix"))
  core <- coredata.ichimoku(x)
  xlen <- dim(core)[1L]
  p2 <- attr(x, "periods")[2L]

  if (y != "none") {
    target <- c((log(core[(k + 1):xlen, "open"]) - log(core[1:(xlen - k), "open"]))[2:(xlen - k)], rep(NA, k + 1L))
    if (dir == "short") target <- -target
    if (y == "ret") target <- exp(target) - 1
  }

  pairs <- .mlgrid_pairs
  veclist <- writeVectors(x = core, pairs = pairs, p2 = p2, xlen = xlen, type = type)

  if (!missing(unique) && !isTRUE(unique)) {
    pairs <- list(pairs[[2L]], pairs[[1L]])
    veclistf <- writeVectors(x = core, pairs = pairs, p2 = p2, xlen = xlen, type = type)
    veclist <- c(veclist, veclistf)
  }

  if (length(expr)) {
    for (i in seq_along(expr)) {
      expr[[i]] <- eval(expr[[i]])
      length(expr[[i]]) == xlen ||
        stop(sprintf("expr %d produced output of incorrect length", i), call. = FALSE)
    }
    veclist <- c(veclist, expr)
  }

  veclist <- switch(y, none = veclist, c(list(y = target), veclist))
  grid <- do.call(cbind, veclist)
  dimnames(grid)[[1L]] <- format.POSIXct(index.ichimoku(x))
  grid <- .Call(ichimoku_naomit, grid)

  if (type == "z-score") {
    if (y != "none") {
      idx <- grid[, 1L, drop = FALSE]
      grid <- grid[, -1L]
    }
    dim1 <- dim(grid)[1L]
    dim2 <- dim(grid)[2L]
    means <- .colMeans(grid, dim1, dim2)
    sdevs <- unname(apply(grid, 2, sd))
    grid <- t((t(grid) - means) / sdevs)
    if (y != "none") grid <- cbind(idx, grid)
  } else {
    means <- sdevs <- NA
  }

  switch(format,
         `attributes<-`(matrix_df(grid),
                        list(names = attr(grid, "dimnames")[[2L]],
                             class = "data.frame",
                             row.names = attr(grid, "dimnames")[[1L]],
                             y = y,
                             k = k,
                             direction = dir,
                             type = type,
                             ticker = attr(x, "ticker"),
                             means = means,
                             sdevs = sdevs)),
         `attributes<-`(grid,
                        list(dim = attr(grid, "dim"),
                             dimnames = attr(grid, "dimnames"),
                             y = y,
                             k = k,
                             direction = dir,
                             type = type,
                             ticker = attr(x, "ticker"),
                             means = means,
                             sdevs = sdevs))
  )

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

  `names<-`(mapply(function(c1, c2) {
    offset <- (p2 - 1L) * (c1 == "chikou" || c2 == "chikou")
    switch(type,
           boolean = as.integer(c(rep(NA, offset), (x[, c1] > x[, c2])[1:(xlen - offset)])),
           c(rep(NA, offset), (x[, c1] - x[, c2])[1:(xlen - offset)]))
  }, c1 = pairs[[1L]], c2 = pairs[[2L]], SIMPLIFY = FALSE, USE.NAMES = FALSE),
  do.call(c, mapply(function(c1, c2) paste0(c1, "_", c2),
                    c1 = pairs[[1L]], c2 = pairs[[2L]], SIMPLIFY = FALSE, USE.NAMES = FALSE)))

}

#' Relative Numeric Representation
#'
#' Produce a statistical summary of the latest numeric representation of the
#'     ichimoku cloud chart relative to historical values. For determining
#'     whether current trading falls within or outside of normal ranges.
#'
#' @inheritParams autostrat
#' @param order [default FALSE] set to TRUE to order the results by the absolute
#'     'z-score'.
#' @param signif [default 0.2] set a significance threshold for which if 'p' is
#'     equal or lower, the element will be starred with a '*'.
#'
#' @return A data frame containing a statistical summary of the latest ichimoku
#'     cloud chart representation in relation to historical values.
#'
#'     In addition, the time index of the latest observed values and total
#'     number of datapoints are printed to the console.
#'
#' @details 'mean(X)' is the mean value for each element X, 'sd(X)' the
#'     standard deviation, and 'X[n]' the nth or latest observed values.
#'
#'     'res' is the residual X[n] - mean(X) and represents a centred measure of
#'     deviation for the latest observed value.
#'
#'     The 'z-score' (or standard score) is calculated as res / sd(X) and is a
#'     centred and scaled measure of deviation for the latest observed value.
#'
#'     'p >= |z|' represents the empirical probability of the latest observed
#'     absolute 'z-score' or greater.
#'
#'     'p*' will display a star if 'p >= |z|' is less than or equal to the value
#'     of the argument 'signif'.
#'
#'     'E(|res|)|p' represents the mean or expected absolute value of 'res',
#'     conditional upon the absolute 'z-score' being greater than equal to the
#'     latest observed absolute 'z-score'.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by calling:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' statistics <- relative(cloud, quietly = TRUE)
#' relative(cloud, signif = 0.4)
#' relative(cloud, order = TRUE, signif = 0.4)
#'
#' @export
#'
relative <- function(x, order = FALSE, signif = 0.2, quietly) {

  is.ichimoku(x) || stop("relative() only works on ichimoku objects", call. = FALSE)
  grid <- mlgrid(x, y = "none", type = "numeric")
  xlen <- dim(grid)[1L]
  xwid <- dim(grid)[2L]
  cnames <- attr(grid, "names")
  time <- attr(grid, "row.names")[xlen]

  xn <- as.numeric(grid[xlen, ])
  means <- unname(unlist(lapply(grid, mean)))
  sdevs <- unname(unlist(lapply(grid, sd)))
  res <- xn - means
  zscore <- res / sdevs

  expec <- pval <- numeric(xwid)
  for (i in seq_len(xwid)) {
    vec <- .subset2(grid, i)
    zvec <- (vec - means[i]) / sdevs[i]
    exceed <- abs(zvec) >= abs(zscore[i])
    pval[i] <- sum(exceed) / xlen
    expec[i] <- mean(abs(vec[exceed] - means[i]))
  }
  star <- character(xwid)
  star[pval <= signif] <- "*"

  df <- lapply(list(means, sdevs, xn, res, zscore, pval, star, expec),
               function(x) if (is.numeric(x)) round(x, digits = 2) else x)
  ordered <- !missing(order) && isTRUE(order)
  if (ordered) {
    reorder <- order(abs(zscore), decreasing = TRUE)
    df <- lapply(df, .subset, reorder)
  }

  attributes(df) <- list(names = c("mean(X)", "sd(X)", "X[n]", "res", "z-score", "p >= |z|", "p*", "E(|res|)|p"),
                         class = "data.frame",
                         row.names = if (ordered) cnames[reorder] else cnames,
                         latest = .POSIXct(as.POSIXct(time)),
                         periods = attr(x, "periods"),
                         periodicity = attr(x, "periodicity"),
                         ticker = attr(x, "ticker"))

  if (missing(quietly) || !isTRUE(quietly))
    cat("Latest:", time, "| n:", xlen, "\n", file = stdout())

  df

}
