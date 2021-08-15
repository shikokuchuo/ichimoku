# Ichimoku - Utility Functions -------------------------------------------------

#' Select Trading Days
#'
#' Used by \code{\link{ichimoku}} to subset a vector of dates to trading days.
#'
#' @param x a vector of POSIXct date objects.
#' @param holidays (optional) a vector, or function which outputs a
#'     vector, of dates defined as holidays.
#' @param ... other arguments not used by this function.
#'
#' @return A vector of logical values: TRUE if the corresponding element of 'x'
#'     is a weekday and not a holiday, FALSE otherwise.
#'
#' @details New Year's Day (01/01) and Christmas Day (25/12) are defined as
#'     holidays by default regardless of the values supplied by 'holidays'.
#'
#' @examples
#' dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
#' dates
#' tradingDays(dates)
#' tradingDays(dates, holidays = c("2020-01-02", "2020-01-03"))
#'
#' @export
#'
tradingDays <- function(x, holidays, ...) {
  posixlt <- as.POSIXlt.POSIXct(x)
  vec <- posixlt$wday %in% 1:5
  vec[(posixlt$mon == 0 & posixlt$mday == 1) | (posixlt$mon == 11 & posixlt$mday == 25) ] <- FALSE
  if (!missing(holidays)) {
    holidays <- tryCatch(as.POSIXct(holidays),
                         error = function(e) {
                           warning("Specified holidays are invalid - disregarding", call. = FALSE)
                           return(vec)
                         })
    vec[x %in% holidays] <- FALSE
  }
  vec
}

#' Duplicates of expand.grid for 2 Variables
#'
#' Create a vector of element positions of duplicates in the output of expand.grid
#'     on 2 identical vectors. A faster method of creating combinations for 2
#'     variabes than \code{utils::combn()}.
#'
#' @param n the length of vector passed to \code{expand.grid()}.
#' @param omit.id [default FALSE] to not select the elements where the 2 items
#'     are identical. Set to TRUE to also select these. The output of expand.grid,
#'     subset to remove duplicates with 'omit.id' set to TRUE would be the
#'     equivalent of \code{utils::combn(n, 2)}.
#'
#' @return A numeric vector.
#'
#' @keywords internal
#'
grid_dup <- function(n, omit.id = FALSE) {
  vec <- do.call(c, lapply(seq_len(n - 1), function(x) x * n + 1:x))
  if (isTRUE(omit.id)) {
    vec <- c(vec, do.call(c, lapply(seq_len(n), function(x) x + n * (x - 1))))
  }
  vec
}

#' Trim Dataframe Rows with NA Values
#'
#' Trim rows containing NA values from a 'data.frame' object. A more performant
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
  for (i in 1:length(x)) {
    omit <- omit | is.na(x[[i]])
  }
  x[!omit, , drop = FALSE]
}

#' Convert xts to data.frame
#'
#' A performant 'xts' to 'data.frame' constructor with no data validation or
#'     checking.
#'
#' @param x an 'xts' object.
#' @param preserve.attrs (optional) if set to TRUE, will preserve any additional
#'     attributes set on the original object.
#'
#' @return A 'data.frame' object. The 'xts' index is preserved as the first
#'     column with header 'index'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' df <- xts_df(cloud)
#' str(df)
#'
#' @export
#'
xts_df <- function(x, preserve.attrs) {
  core <- coredata(x)
  dims <- dim(core)
  df <- structure(c(list(index(x)), lapply(seq_len(dims[2L]), function(i) core[, i])),
                  names = c("index", dimnames(core)[[2L]]),
                  class = "data.frame",
                  row.names = seq_len(dims[1L]))
  if (!missing(preserve.attrs) && isTRUE(preserve.attrs)) {
    lk <- look(x)
    attrs <- attributes(df)
    attributes(df) <- c(attrs, lk)
  }
  df
}

#' Convert matrix to data.frame
#'
#' A performant 'matrix' to 'data.frame' constructor with no data validation or
#'     checking.
#'
#' @param x a matrix.
#' @param preserve.attrs (optional) if set to TRUE, will preserve any additional
#'     attributes set on the original object.
#'
#' @return A 'data.frame' object. If the matrix has row names, these are retained
#'     in the dataframe, otherwise the row names of the dataframe will be an
#'     integer sequence.
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
matrix_df <- function(x, preserve.attrs) {
  dnames <- dimnames(x)
  mat <- unname(x)
  dims <- dim(mat)
  df <- structure(lapply(seq_len(dims[2L]), function(i) mat[, i]),
            names = dnames[[2L]],
            class = "data.frame",
            row.names = if (is.null(dnames[[1L]])) seq_len(dims[1L]) else dnames[[1L]])
  if (!missing(preserve.attrs) && isTRUE(preserve.attrs)) {
    lk <- look(x)
    attrs <- attributes(df)
    attributes(df) <- c(attrs, lk)
  }
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
#' @return A data.frame containing all unique entries in the objects passed is
#'     returned.
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
  if (isTRUE(attr(dots[[1]], "oanda"))) {
    merge <- structure(merge,
                       instrument = attr(dots[[1]], "instrument"),
                       price = attr(dots[[1]], "price"),
                       timestamp = do.call(max, lapply(dots, attr, "timestamp")),
                       oanda = TRUE)
    if (FALSE %in% merge$complete) {
      warning("Incomplete periods in merged dataframe, please check for possible duplicates",
              call. = FALSE)
    }
  }
  merge
}

#' Append New Data to Dataframe
#'
#' Update a 'data.frame' object with new data. Can be used to append new updated
#'     time series data to an existing dataframe, where each observation is indexed
#'     by a unique timestamp.
#'
#' @param new data.frame object containing new data.
#' @param old data.frame object containing existing data.
#'
#' @return A data.frame of the existing data appended with the new data. If the
#'     data in 'new' contains data with the same value for 'time' as 'old',
#'     the data in 'new' will overwrite the data in 'old'.
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
df_append <- function(new, old) {
  structure(rbind.data.frame(old[!old$time %in% new$time, ], new),
            timestamp = attr(new, "timestamp"))
}

