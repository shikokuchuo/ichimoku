# Ichimoku - Apache Arrow Translation Layer ------------------------------------

#' Write ichimoku objects to Arrow Archive
#'
#' Used to write ichimoku objects to archive storage in the Apache Arrow IPC
#'     file format.
#'
#' @param object an ichimoku object.
#' @param file A string file path, URI, or OutputStream, or path in a file
#'     system (SubTreeFileSystem).
#'
#' @return Invisible NULL. 'object' is written to 'file' as a side effect.
#'
#' @details If the file write operation has been successful, a confirmation will
#'     be printed to the console. Use \code{\link{ichimoku_read}} to read
#'     ichimoku objects archived using this function.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' filename <- tempfile()
#' ichimoku_write(cloud, filename)
#'
#' restored <- ichimoku_read(filename)
#' all.equal(cloud, restored, check.attributes = FALSE)
#' unlink(filename)
#'
#' @export
#'
ichimoku_write <- function(object, file) {

  if (requireNamespace("arrow", quietly = TRUE)) {

    objectname <- deparse(substitute(object))
    if (missing(object)) stop("No object specified for ichimoku_write()", call. = FALSE)
    if (missing(file)) stop("Argument 'file' must be specified", call. = FALSE)
    if (!is.ichimoku(object)) stop("ichimoku_write() only works with ichimoku objects", call. = FALSE)
    df <- xts_df(object)
    df <- structure(df,
                    ichimoku = TRUE,
                    periods = attr(object, "periods"),
                    periodicity = attr(object, "periodicity"),
                    ticker = attr(object, "ticker"),
                    strat = attr(object, "strat"))

    arrow::write_feather(df, file)
    message("ichimoku object '", objectname, "' written to: ", file)

  } else {
    message("Note: please install the 'arrow' package to enable archiving of ichimoku objects",
            "\nArchives utilise the Apache Arrow IPC file format")
  }
}

#' Read ichimoku objects from Arrow Archive
#'
#' Used to read ichimoku objects from archive storage in the Apache Arrow IPC
#'     file format.
#'
#' @param file A character file name or URI, raw vector, an Arrow input stream,
#'     or a FileSystem with path (SubTreeFileSystem). If a file name or URI, an
#'     Arrow InputStream will be opened and closed when finished. If an input
#'     stream is provided, it will be left open.
#'
#' @return The ichimoku object that was originally archived.
#'
#' @details Used to read ichimoku objects archived using the
#'     \code{\link{ichimoku_write}} function.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' filename <- tempfile()
#' ichimoku_write(cloud, filename)
#'
#' restored <- ichimoku_read(filename)
#' all.equal(cloud, restored, check.attributes = FALSE)
#' unlink(filename)
#'
#' @export
#'
ichimoku_read <- function(file) {

  if (requireNamespace("arrow", quietly = TRUE)) {

    if (missing(file)) stop("No file specified for ichimoku_read()", call. = FALSE)
    df <- arrow::read_feather(file)
    if (!isTRUE(attr(df, "ichimoku"))) stop("ichimoku_read() reads only ichimoku objects",
                                            call. = FALSE)

    structure(xts(df[, -1], order.by = df[, 1]),
              class = c("ichimoku", "xts", "zoo"),
              periods = attr(df, "periods"),
              periodicity = attr(df, "periodicity"),
              ticker = attr(df, "ticker"),
              strat = attr(df, "strat"))

  } else {
    message("Note: please install the 'arrow' package to read archived ichimoku objects",
            "\nArchives utilise the Apache Arrow IPC file format")
  }
}

