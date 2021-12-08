# Ichimoku - Utility Functions -------------------------------------------------

#' Select Trading Days
#'
#' Used by \code{\link{ichimoku}} to subset a vector of dates to trading days.
#'
#' @param x a vector of POSIXct date objects.
#' @param holidays (optional) a vector, or function which outputs a vector, of
#'     dates defined as holidays. Set to NULL for a continuously-traded market.
#' @param ... other arguments not used by this function.
#'
#' @return A vector of logical values: TRUE if the corresponding element of 'x'
#'     is a weekday and not a holiday, FALSE otherwise.
#'
#'     Or, if the parameter 'holidays' is set to NULL, a vector of TRUE values
#'     of the same length as 'x'.
#'
#' @details New Year's Day (01-01) and Christmas Day (12-25) are defined as
#'     holidays by default if 'holidays' is not specified.
#'
#' @examples
#' dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
#' dates
#' tradingDays(dates)
#' tradingDays(dates, holidays = c("2020-01-02", "2020-01-03"))
#' tradingDays(dates, holidays = NULL)
#'
#' @export
#'
tradingDays <- function(x, holidays, ...) {
  if (missing(holidays)) {
    posixlt <- as.POSIXlt.POSIXct(x)
    vec <- .subset2(posixlt, "wday") %in% 1:5
    vec[(.subset2(posixlt, "mon") == 0L & .subset2(posixlt, "mday") == 1L) |
          (.subset2(posixlt, "mon") == 11L & .subset2(posixlt, "mday") == 25L)] <- FALSE
  } else if (is.null(holidays)) {
    return(rep(TRUE, length(x)))
  } else {
    posixlt <- as.POSIXlt.POSIXct(x)
    vec <- .subset2(posixlt, "wday") %in% 1:5
    holidays <- tryCatch(as.POSIXct(holidays), error = function(e) {
      warning("Specified holidays are invalid - reverting to defaults", call. = FALSE)
      vec[(.subset2(posixlt, "mon") == 0L & .subset2(posixlt, "mday") == 1L) |
            (.subset2(posixlt, "mon") == 11L & .subset2(posixlt, "mday") == 25L)] <- FALSE
      return(vec)
    })
    vec[x %in% holidays] <- FALSE
  }
  vec
}

#' Duplicates of expand.grid for 2 Variables
#'
#' Create a vector of element positions of duplicates in the output of expand.grid
#'     on 2 identical vectors. An efficient method of creating combinations for
#'     2 variables.
#'
#' @param n the length of vector passed to \code{expand.grid()}.
#' @param omit.id (optional) set to TRUE to also select the elements where the 2
#'     items are identical. The output of expand.grid, subset to remove
#'     duplicates with 'omit.id' set to TRUE would be the equivalent of
#'     \code{utils::combn(n, 2)}.
#'
#' @return A numeric vector.
#'
#' @examples
#' n <- 3
#' expand.grid(1:n, 1:n)
#' expand.grid(1:n, 1:n)[-grid_dup(n), ]
#' expand.grid(1:n, 1:n)[-grid_dup(n, omit.id = TRUE), ]
#'
#' @keywords internal
#' @export
#'
grid_dup <- function(n, omit.id) {
  vec <- vector(mode = "list", length = n - 1)
  for (i in seq_len(n - 1)) {
    vec[[i]] <- i * n + 1:i
  }
  vec <- unlist(vec)
  if (!missing(omit.id) && isTRUE(omit.id)) {
    vec <- c(vec, 1:n + n * (1:n - 1))
  }
  vec
}

#' Trim Dataframe Rows with NA Values
#'
#' Trim rows containing NA values from a 'data.frame' object. An efficient
#'     version of \code{stats::na.omit()} with no data validation or checking.
#'
#' @param x the data.frame to trim.
#'
#' @return The data.frame 'x' with rows containing NA values removed.
#'
#' @details Works only where the columns contain atomic (e.g. numeric) and not
#'     recursive types (e.g. lists).
#'
#' @examples
#' data <- data.frame(c(1:4, NA), c(NA, 2:5))
#' data
#' df_trim(data)
#'
#' @export
#'
df_trim <- function(x) {
  omit <- logical(dim(x)[1L])
  for (i in seq_len(length(x))) {
    omit <- omit | is.na(x[[i]])
  }
  x[!omit, , drop = FALSE]
}

#' Convert xts to data.frame
#'
#' An optimised 'xts' to 'data.frame' constructor.
#'
#' @param x an 'xts' object.
#' @param keep.attrs (optional) if set to TRUE, will preserve any custom
#'     attributes set on the original object.
#'
#' @return A 'data.frame' object. The 'xts' index is preserved as the first
#'     column with header 'index'.
#'
#' @details The optimised data.frame constructors are used internally within
#'     the package and made available as utilities. Please note that no data
#'     validation or checking is performed.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' df <- xts_df(cloud)
#' str(df)
#'
#' df2 <- xts_df(cloud, keep.attrs = TRUE)
#' str(df2)
#'
#' @export
#'
xts_df <- function(x, keep.attrs) {
  core <- coredata(x)
  dims <- dim(core)
  len <- dims[2L]
  df <- vector(mode = "list", length = len + 1L)
  df[[1L]] <- index(x)
  for (i in seq_len(len)) {
    df[[i + 1L]] <- core[, i]
  }
  attributes(df) <- c(list(names = c("index", dimnames(core)[[2L]]),
                           class = "data.frame",
                           row.names = .set_row_names(dims[1L])),
                      if (!missing(keep.attrs) && isTRUE(keep.attrs)) look(x))
  df
}

#' Convert matrix to data.frame
#'
#' An optimised 'matrix' to 'data.frame' constructor.
#'
#' @param x a matrix.
#' @param keep.attrs (optional) if set to TRUE, will preserve any custom
#'     attributes set on the original object.
#'
#' @return A 'data.frame' object. If the matrix has row names, these are
#'     retained by the dataframe.
#'
#' @details The optimised data.frame constructors are used internally within
#'     the package and made available as utilities. Please note that no data
#'     validation or checking is performed.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' mcloud <- as.matrix(cloud)
#' df <- matrix_df(mcloud)
#' str(df)
#' str(rownames(df))
#'
#' @export
#'
matrix_df <- function(x, keep.attrs) {
  dnames <- dimnames(x)
  mat <- unname(x)
  dims <- dim(mat)
  len <- dims[2L]
  df <- vector(mode = "list", length = len)
  for (i in seq_len(len)) {
    df[[i]] <- mat[, i]
  }
  attributes(df) <- c(list(names = dnames[[2L]],
                           class = "data.frame",
                           row.names = if (is.null(dnames[[1L]])) .set_row_names(dims[1L]) else dnames[[1L]]),
                      if (!missing(keep.attrs) && isTRUE(keep.attrs)) look(x))
  df
}

#' Merge Dataframes
#'
#' Full join on an arbitrary number of 'data.frame' objects passed as arguments,
#'     preserving all unique entries. Can be used to combine historical time
#'     series data where each observation is indexed by a unique timestamp and
#'     all periods are complete.
#'
#' @param ... data.frame objects to combine.
#'
#' @return A data.frame containing all unique entries in the objects passed as
#'     argument.
#'
#' @details Can be used to join price dataframes retrieved by \code{\link{oanda}}.
#'     The function is designed to join complete historical data. If the data to
#'     be merged contains data with incomplete periods, all entries are preserved
#'     rather than updated. If incomplete periods are detected within the data,
#'     a warning is issued, and the resulting dataframe should be manually checked
#'     in case it contains unwanted duplicates. Use \code{\link{df_append}} for
#'     updating dataframes with new values.
#'
#' @examples
#' data1 <- sample_ohlc_data[1:6, ]
#' data1
#' data2 <- sample_ohlc_data[4:10, ]
#' data2
#' df_merge(data1, data2)
#'
#' @export
#'
df_merge <- function(...) {
  dots <- list(...)
  merge <- Reduce(function(x, y) merge.data.frame(x, y, all = TRUE), dots)
  if (isTRUE(attr(dots[[1L]], "oanda"))) {
    attributes(merge) <- c(attributes(merge),
                           list(instrument = attr(dots[[1L]], "instrument"),
                                price = attr(dots[[1L]], "price"),
                                timestamp = .POSIXct(max(unlist(lapply(dots, attr, "timestamp")))),
                                oanda = TRUE))
    if (FALSE %in% .subset2(merge, "complete")) warning("Incomplete periods in merged dataframe - please check for possible duplicates", call. = FALSE)
  }
  merge
}

#' Append New Data to Dataframe
#'
#' Update a 'data.frame' object with new data. Can be used to append new updated
#'     time series data to an existing dataframe, where each observation is
#'     indexed by a unique timestamp/identifier in a key column.
#'
#' @param new data.frame object containing new data.
#' @param old data.frame object containing existing data.
#' @param key [default 'time'] column name used as key, provided as a character
#'     string.
#' @param keep.attr [default 'timestamp'] name of an attribute in 'new' to retain
#'     if it is present, provided as a character string.
#'
#' @return A data.frame of the existing data appended with the new data. If the
#'     data in 'new' contains data with the same value for the key column as 'old',
#'     the data in 'new' will overwrite the data in 'old'.
#'
#'     If the attribute specified by 'keep.attr' is present in 'new', this is
#'     retained. All other non-required attributes are dropped.
#'
#' @details Can be used to update price dataframes retrieved by \code{\link{oanda}}.
#'     The function is designed to update existing data with new values as they
#'     become available. As opposed to \code{\link{df_merge}}, the data in 'new'
#'     will overwrite the data in 'old' rather than create duplicates.
#'
#' @examples
#' data1 <- sample_ohlc_data[1:8, ]
#' data1
#' data2 <- sample_ohlc_data[7:10, ]
#' data2
#' df_append(data2, data1)
#'
#' @export
#'
df_append <- function(new, old, key = "time", keep.attr = "timestamp") {
  keep <- !.subset2(old, key) %in% .subset2(new, key)
  cnames <- attr(new, "names")
  len <- length(new)
  df <- vector(mode = "list", length = len)
  for (i in seq_len(len)) {
    df[[i]] <- c(.subset2(old, i)[keep], .subset2(new, i))
  }
  attributes(df) <- list(names = cnames,
                         class = "data.frame",
                         row.names = .set_row_names(length(df[[1L]])))
  attr(df, keep.attr) <- attr(new, keep.attr)
  df
}

#' Print More Rows of Ichimoku Objects
#'
#' After calling or invoking the default print method for ichimoku objects, the
#'     console output will display \code{# … with x more rows} if the entire
#'     data does not fit on-screen. Call \code{more()} to display more rows.
#'
#' @param n a parameter (optional) passed on to the argument 'n' of
#'     \code{\link[tibble]{print.tbl}} controlling the number of rows to show.
#'     Defaults to 100.
#'
#' @return The ichimoku object contained in \code{\link{.Last.value}} (invisibly)
#'     or else invisible NULL (if .Last.value is not an ichimoku object).
#'     The ichimoku object data is printed to the console.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' cloud
#' more()
#' more(20)
#'
#' @export
#'
more <- function(n) {

  is.ichimoku(lv <- .Last.value) || return(invisible())
  print(lv, plot = FALSE, n = if (missing(n)) 100 else n)

}

#' Look at Informational Attributes
#'
#' Inspect the informational attributes of objects.
#'
#' @param x an object (optional). If 'x' is not supplied, \code{\link{.Last.value}}
#'     will be used instead.
#'
#' @return For objects created by the ichimoku package, a list of attributes
#'     specific to that data type.
#'
#'     For other objects, a list of non-standard attributes for matrix /
#'     data.frame / xts classes, or else invisible NULL if none are present.
#'
#' @details Note: autostrat list attributes may be accessed directly using
#'     \code{look(x)$logret} and \code{look(x)$summary}.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' look(cloud)
#'
#' stratlist <- autostrat(cloud, n = 3)
#' look(stratlist)
#'
#' strat <- stratlist[[1]]
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
look <- function(x) {

  lk <- attributes(if (missing(x)) .Last.value else x)
  lk <- lk[!names(lk) %in% c("dim", "dimnames", "names", "row.names", "index", "class", "oanda")]
  if (length(lk)) lk else invisible()

}

#' is.ichimoku
#'
#' A function for checking if an object is an ichimoku object.
#'
#' @param x an object.
#'
#' @return A logical value of TRUE if 'x' is of class 'ichimoku', otherwise FALSE.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#'
#' # TRUE:
#' is.ichimoku(cloud)
#' # FALSE:
#' is.ichimoku(sample_ohlc_data)
#'
#' @export
#'
is.ichimoku <- function(x) inherits(x, "ichimoku")

