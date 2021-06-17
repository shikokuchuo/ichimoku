# Ichimoku - ML Layer ----------------------------------------------------------

#' mlgrid Numeric Representation
#'
#' Create a grid of ichimoku rule conditions and next period returns. The grid
#'     facilitates comparing strategy returns or as a basis for further
#'     processing in machine learning applications. The purpose of this function
#'     is to translate the visual representation of the relationship between
#'     cloud chart elements into a numerical format for further analysis.
#'
#' @param x an ichimoku object.
#' @param y choose target variable 'logret' (log returns) or 'ret' (discrete
#'     returns), with a default of 'logret'.
#' @param dir trade direction either 'long' or 'short' with a default of 'long'.
#' @param type either 'boolean' or 'numeric' with a default of 'boolean'.
#'     'boolean' creates a grid of dummy variables for ichimoku indicator
#'     conditions of the form 1 if c1 > c2, 0 otherwise. 'numeric' creates a
#'     grid of the numeric difference c1 - c2.
#' @param unique defaults to TRUE to return only unique combinations of c1 and c2.
#'     Set to FALSE to return both c1 > c2 and c2 > c1.
#'
#' @return A data.frame in a 'tidy' format with one observation per row and one
#'     feature per column with the target 'y' as the first column.
#'
#'     The 'y' parameter and trade direction are set as atrributes. To view these,
#'     use the attributes() function on the returned object.
#'
#' @details The date-time index corresponds to when the condition is met at the
#'     close for that period. The return is the single-period return achieved by
#'     transacting at the immediately following opening price until the next
#'     opening price.
#'
#'     Only valid combinations are included. This excludes any combination
#'     involving 'open' as it is in effect a lagged indicator and not
#'     contemporaneous. The following trivial or highly-collinear pairs are also
#'     excluded: (high, close) ,(low, close), (low, high), (cloudTop, Senkou A),
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
#' attributes(grid)
#'
#' @export
#'
mlgrid <- function(x, y = c("logret", "ret"), dir = c("long", "short"),
                   type = c("boolean", "numeric"), unique = TRUE) {
  if(is.ichimoku(x)) {
    target <- match.arg(y)
    dir <- match.arg(dir)
    type <- match.arg(type)
    xlen <- dim(x)[1L]
    p2 <- attr(x, "periods")[2L]
    y <- c(diff(log(coredata(x$open)))[2:(xlen - 1)], NA, NA)
    if(dir == "short") y <- -y
    if(target == "ret") y <- exp(y) - 1
    sel <- c("chikou", "close", "high", "low", "tenkan", "kijun",
             "senkouA", "senkouB", "cloudTop", "cloudBase")
    comb <- as.matrix(expand.grid(sel, sel, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
                      )[-duplicate(length(sel), identical = TRUE), ]
    pairs <- comb[-c(10, 11, 18, 41:45), ]
    matrix <- writeMatrix(x, pairs = pairs, p2 = p2, xlen = xlen, type = type)
    if(!isTRUE(unique)) {
      pairs <- cbind(pairs[, 2], pairs[, 1])
      matrixf <- writeMatrix(x, pairs = pairs, p2 = p2, xlen = xlen, type = type)
      matrix <- cbind(matrix, matrixf)
    }
    grid <- data.frame(y, matrix)
    row.names(grid) <- index(x)
    grid <- trimdf(grid)
    attr(grid, "y") <- target
    attr(grid, "direction") <- dir
    attr(grid, "mlgrid") <- TRUE
    grid
  } else {
    message("ichimoku: mlgrid only works on ichimoku objects")
  }
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
  matrix <- do.call(cbind, lapply(seq_along(pairs[, 1L]), function(i) {
    c1 <- pairs[i, 2L]
    c2 <- pairs[i, 1L]
    offset <- p2 * (c1 == "chikou" | c2 == "chikou")
    if(type == "boolean") as.integer(c(rep(NA, offset), (x[, c1] > x[, c2])[1:(xlen - offset)]))
    else c(rep(NA, offset), (x[, c1] - x[, c2])[1:(xlen - offset)])
  }))
  colnames(matrix) <- vapply(seq_along(pairs[, 1L]),
                             function(i) paste0(pairs[i, 2L], "_", pairs[i, 1L]),
                             character(1L), USE.NAMES = FALSE)
  matrix
}

