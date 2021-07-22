# Ichimoku - ML Layer ----------------------------------------------------------

#' mlgrid Numeric Representation
#'
#' Create a grid of ichimoku indicator conditions and next period returns. The
#'     grid facilitates comparing strategy returns or as a basis for further
#'     processing in machine learning applications. Translates the visual
#'     representation of the relationship between cloud chart elements into a
#'     numerical format for further analysis.
#'
#' @param x an ichimoku object.
#' @param y [default 'logret'] choose target variable 'logret' (log returns) or
#'     'ret' (discrete returns).
#' @param dir [default 'long'] trade direction, either 'long' or 'short'.
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
#'     use the \code{\link{look}} function on the returned object.
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
#'     mlgrid is used by \code{\link{autostrat}} to enumerate the returns for all
#'     valid strategy combinations.
#'
#' @section Further Details:
#'     Please refer to the strategies vignette by running:
#'     \code{vignette("strategies", package = "ichimoku")}
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' grid <- mlgrid(cloud, y = "ret", dir = "short", type = "numeric")
#' look(grid)
#'
#' @export
#'
mlgrid <- function(x, y = c("logret", "ret"), dir = c("long", "short"),
                   type = c("boolean", "numeric"), unique = TRUE) {

  if (!is.ichimoku(x)) stop("mlgrid() only works on ichimoku objects", call. = FALSE)
  target <- match.arg(y)
  dir <- match.arg(dir)
  type <- match.arg(type)
  xlen <- dim(x)[1L]
  p2 <- attr(x, "periods")[2L]
  y <- c(diff(log(coredata(x$open)))[2:(xlen - 1)], NA, NA)
  if (dir == "short") y <- -y
  if (target == "ret") y <- exp(y) - 1
  cols <- c("chikou", "close", "high", "low", "tenkan", "kijun",
            "senkouA", "senkouB", "cloudTop", "cloudBase")
  comb <- as.matrix(expand.grid(cols, cols, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
                    )[-grid_dup(length(cols), omit.id = TRUE), ]
  pairs <- comb[-c(10L, 11L, 18L, 41:45), ]
  matrix <- writeMatrix(x, pairs = pairs, p2 = p2, xlen = xlen, type = type)
  if (!isTRUE(unique)) {
    pairs <- cbind(pairs[, 2L], pairs[, 1L])
    matrixf <- writeMatrix(x, pairs = pairs, p2 = p2, xlen = xlen, type = type)
    matrix <- cbind(matrix, matrixf)
  }
  mat <- unname(matrix)
  grid <- structure(c(list(y), lapply(seq_len(dim(mat)[2L]), function(i) mat[, i])),
                    class = "data.frame",
                    names = c("y", colnames(matrix)),
                    row.names = as.character(index(x)),
                    y = target,
                    direction = dir,
                    ticker = attr(x, "ticker"),
                    mlgrid = TRUE)
  df_trim(grid)
}

#' writeMatrix
#'
#' Internal function used by mlgrid to create matrices of ichimoku representations.
#'
#' @param x an ichimoku object.
#' @param pairs a 2-column matrix of pairs of column names.
#' @param p2 length of second ichimoku period.
#' @param xlen number of rows of 'x'.
#' @param type type of matrix to return, 'boolean' or 'numeric'.
#'
#' @return A matrix with column headings.
#'
#' @keywords internal
#'
writeMatrix <- function(x, pairs, p2, xlen, type) {
  matrix <- do.call(cbind, mapply(function(c1, c2) {
    offset <- p2 * (c1 == "chikou" | c2 == "chikou")
    switch(type,
           boolean = as.integer(c(rep(NA, offset), (x[, c1] > x[, c2])[1:(xlen - offset)])),
           numeric = c(rep(NA, offset), (x[, c1] - x[, c2])[1:(xlen - offset)]))
  }, c1 = pairs[, 2L], c2 = pairs[, 1L], SIMPLIFY = FALSE, USE.NAMES = FALSE))
  colnames(matrix) <- do.call(c, mapply(function(c1, c2) paste0(c1, "_", c2),
                                        c1 = pairs[, 2L], c2 = pairs[, 1L],
                                        SIMPLIFY = FALSE, USE.NAMES = FALSE))
  matrix
}

#' Look at Ichimoku Objects
#'
#' Inspect the informational attributes of objects created by the 'ichimoku'
#'     package. Can also be used to extract ichimoku objects from lists returned
#'     by \code{\link{autostrat}}.
#'
#' @param x an ichimoku object or an object returned by \code{\link{autostrat}},
#'     \code{\link{mlgrid}} or \code{\link{oanda}}.
#' @param which (optional) integer value of strategy to return from an autostrat
#'     list.
#'
#' @return List of attribute values, or if 'which' is specified on an autostrat
#'     list, an ichimoku object containing a strategy.
#'
#' @details Note: for a level 2 autostrat object, if the object fails to print
#'     correctly due to its length, please access the list items directly using
#'     \code{look(x)$summary} and \code{look(x)$logret}, possibly in conjunction
#'     with head() or by setting the 'max' argument in print().
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' look(cloud)
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#'
#' strat <- look(stratlist, which = 1)
#' look(strat)
#'
#' grid <- mlgrid(cloud)
#' look(grid)
#'
#' \dontrun{
#' # OANDA API key required to run this example
#' prices <- oanda("USD_JPY")
#' look(prices)
#' }
#'
#' @export
#'
look <- function(x, which) {
  name <- deparse(substitute(x))
  if (is.ichimoku(x)) {
    if (hasStrat(x)) list(periods = attr(x, "periods"), periodicity = attr(x, "periodicity"),
                          ticker = attr(x, "ticker"), strat = attr(x, "strat"))
    else list(periods = attr(x, "periods"), periodicity = attr(x, "periodicity"),
              ticker = attr(x, "ticker"))
  }
  else if (isTRUE(attr(x, "autostrat"))) {
    if (missing(which)) list(logret = attr(x, "logret"), summary = attr(x, "summary"))
    else tryCatch(x[[which]], error = function(e) {
      stop("Value '", which, "' for 'which' is invalid\n'which' must be an integer ",
           "specifying one of the strategies contained in '", name, "'", call. = FALSE)
      })
  } else if (isTRUE(attr(x, "mlgrid"))) {
    list(y = attr(x, "y"), direction = attr(x, "direction"), ticker = attr(x, "ticker"))
  } else if (isTRUE(attr(x, "oanda"))) {
    list(instrument = attr(x, "instrument"), price = attr(x, "price"),
         timestamp = attr(x, "timestamp"))
  } else stop("look() only works with certain object types created with the ichimoku package",
              call. = FALSE)
}

