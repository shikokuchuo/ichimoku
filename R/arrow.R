# Ichimoku - Apache Arrow Translation Layer ------------------------------------

#' Read/write ichimoku objects <> Arrow Archive
#'
#' Used to read and write ichimoku objects to/from archival storage in the
#'     Apache Arrow IPC file format.
#'
#' @param ... additional parameters not used by this function.
#' @param object (for write operations) an ichimoku object.
#' @param filename string file path, URI, or OutputStream, or path in a file
#'     system (SubTreeFileSystem).
#'
#' @return For read operations: the ichimoku object originally archived.
#'
#'     For write operations: invisible NULL. 'object' is written to
#'     'filename' as a side effect.
#'
#' @details For read operations, please specify only 'filename'. 'filename' is
#'     read and the return value may be assigned to an object.
#'
#'     For write operations: please specify both 'object' and 'filename'.
#'     'object' will be written to 'filename'. Confirmation is printed to the
#'     console if the file write operation has been successful.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' filename <- tempfile()
#' archive(cloud, filename)
#'
#' restored <- archive(filename)
#' all.equal(cloud, restored)
#' unlink(filename)
#'
#' @export
#'
archive <- function(..., object, filename) {

  if (requireNamespace("arrow", quietly = TRUE)) {

    if (missing(object) && missing(filename)) {
      dots <- list(...)

      if (length(dots) == 1L) {
        filename <- dots[[1L]]
        readArchive(filename = filename)

      } else if (length(dots) == 2L) {
        object <- dots[[1L]]
        filename <- dots[[2L]]
        writeArchive(object = object, filename = filename)

      } else if (length(dots) > 2L) {
        stop("Too many arguments passed to archive()",
             "\nFor read operations please specify 'filename' only",
             "\nFor write operations please specify both 'object' and 'filename'", call. = FALSE)
      } else {
        stop("archive() is used to read/write ichimoku objects from/to Arrow archives",
             "\nFor read operations please specify 'filename' only",
             "\nFor write operations please specify both 'object' and 'filename'", call. = FALSE)
      }

    } else if (missing(object)) readArchive(filename = filename)

    else writeArchive(object = object, filename = filename)

  } else {
    message("Note: please install the 'arrow' package to enable archiving of ichimoku objects",
            "\nArchives utilise the Apache Arrow IPC file format")
  }
}

#' Write ichimoku objects to Arrow Archive
#'
#' Used to write ichimoku objects to archive storage in the Apache Arrow IPC
#'     file format.
#'
#' @param object an ichimoku object.
#' @param filename string file path, URI, or OutputStream, or path in a file
#'     system (SubTreeFileSystem).
#'
#' @return Invisible NULL. 'object' is written to 'file' as a side effect.
#'
#' @keywords internal
#'
writeArchive <- function(object, filename) {

  if (!is.character(filename)) {
    stop("in archive(object, filename): 'filename' must be supplied as a string. ",
         "\nDid you omit the surrounding quotes \"\"?", call. = FALSE)
  }
  if (!is.ichimoku(object)) {
    stop("object is of class '", class(object)[1L],
         "', archive() only writes ichimoku objects", call. = FALSE)
  }

  df <- xts_df(object)
  df <- structure(df,
                  ichimoku = TRUE,
                  periods = attr(object, "periods"),
                  periodicity = attr(object, "periodicity"),
                  ticker = attr(object, "ticker"),
                  strat = attr(object, "strat"))

  arrow::write_feather(df, filename)
  message("Archive created: ", filename)
}

#' Read ichimoku objects from Arrow Archive
#'
#' Used to read ichimoku objects from archive storage in the Apache Arrow IPC
#'     file format.
#'
#' @param filename string file path, URI, or OutputStream, or path in a file
#'     system (SubTreeFileSystem).
#'
#' @return The ichimoku object that was originally archived.
#'
#' @keywords internal
#'
readArchive <- function(filename) {

  if (!is.character(filename)) {
    stop("in archive(filename): 'filename' must be supplied as a string. ",
         "\nDid you omit the surrounding quotes \"\"?", call. = FALSE)
  }

  df <- arrow::read_feather(filename)
  if (!isTRUE(attr(df, "ichimoku"))) {
    stop("'", paste0(filename), "' is not an archived ichimoku object", call. = FALSE)
  }

  structure(xts(df[, -1], order.by = df[, 1]),
            class = c("ichimoku", "xts", "zoo"),
            periods = attr(df, "periods"),
            periodicity = attr(df, "periodicity"),
            ticker = attr(df, "ticker"),
            strat = attr(df, "strat"))
}

