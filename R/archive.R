# Ichimoku - Data Layer --------------------------------------------------------

#' Read/write ichimoku objects <> Arrow Archive
#'
#' Used to read and write ichimoku objects to/from archival storage in the
#'     Apache Arrow IPC file format.
#'
#' @param ... unnamed arguments will be parsed as 'filename' if there is only
#'     one argument, 'object' and 'filename' if there are two arguments.
#' @param object (for write operations) an ichimoku object.
#' @param filename string file path, URI, or OutputStream, or path in a file
#'     system (SubTreeFileSystem).
#'
#' @return For read operations: the ichimoku object originally archived.
#'
#'     For write operations: invisible NULL. 'object' is written to 'filename'
#'     as a side effect.
#'
#' @details For read operations, please specify only 'filename'. 'filename' is
#'     read and the return value may be assigned to an object.
#'
#'     For write operations: please specify both 'object' and 'filename'.
#'     'object' will be written to 'filename'. Confirmation is printed to the
#'     console if the file write operation has been successful.
#'
#' @section Further Details:
#'
#'     This function requires the 'arrow' package to be installed.
#'
#'     If the 'openssl' package is available, a sha256 hash of the original
#'     object is written to the archive. This allows the data integrity of the
#'     restored object to be verified when the archive is read back.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' filename <- tempfile()
#'
#' archive(cloud, filename)
#'
#' restored <- archive(filename)
#'
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

    } else if (!missing(filename)) {

      if (missing(object)) readArchive(filename = filename)
      else writeArchive(object = object, filename = filename)

    } else stop("in archive(object, filename): 'object' specified without 'filename'", call. = FALSE)

  } else {
    message("Please install the 'arrow' package to enable archiving of ichimoku objects",
            "\nArchives utilise the Apache Arrow IPC file format")
  }
}

#' Write ichimoku objects to Arrow Archive
#'
#' Internal function used to write ichimoku objects to archive storage in the
#'     Apache Arrow IPC file format.
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

  if (file.exists(filename)) {
    continue <- readline(prompt = paste0("The file '", filename,
                                         "' already exists. Overwrite? [y/N] "))
    if (!continue %in% c("y", "Y", "yes", "YES")) {
      message("Request cancelled")
      return(invisible())
    }
  }

  if (requireNamespace("openssl", quietly = TRUE)) {
    sha256 <- suppressWarnings(openssl::sha256(as.raw(object)))
  } else {
    message("sha256 hash not written to archive as 'openssl' package not available - ",
            "\nthis means the integrity of objects cannot be verified when read back")
    sha256 <- NULL
  }
  df <- xts_df(object)
  df <- structure(df,
                  periods = attr(object, "periods"),
                  periodicity = attr(object, "periodicity"),
                  ticker = attr(object, "ticker"),
                  strat = attr(object, "strat"),
                  ichimoku351 = TRUE,
                  sha256 = sha256)

  arrow::write_feather(df, filename)
  message("Archive written to '", filename, "'", sep = "")
  invisible()
}

#' Read ichimoku objects from Arrow Archive
#'
#' Internal function used to read ichimoku objects from archive storage in the
#'     Apache Arrow IPC file format.
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
  if (!isTRUE(attr(df, "ichimoku351"))) {
    stop("'", paste0(filename), "' is not an archived ichimoku object", call. = FALSE)
  }

  tzone <- if (is.null(attr(df$index, "tzone"))) "" else attr(df$index, "tzone")

  object <- structure(xts(df[, -1], order.by = df[, 1], tzone = tzone),
                      class = c("ichimoku", "xts", "zoo"),
                      periods = attr(df, "periods"),
                      periodicity = attr(df, "periodicity"),
                      ticker = attr(df, "ticker"),
                      strat = attr(df, "strat"))

  if (is.null(attr(df, "sha256"))) {
    message("Archive read from '", filename,
            "'\nIntegrity of restored object cannot be verified as archive does not contain a sha256 hash")

  } else if (requireNamespace("openssl", quietly = TRUE)) {
    sha256 <- suppressWarnings(openssl::sha256(as.raw(object)))
    if (identical(sha256, attr(df, "sha256"))) {
      message("Archive read from '", filename,
              "'\nData verified by sha256: ", sha256)
    } else {
      message("Archive read from '", filename, "'")
      warning("sha256 of restored object\n", sha256,
              " does not match the original\n", attr(df, "sha256"), call. = FALSE)
    }
  } else {
    message("Archive read from '", filename,
            "'\n'openssl' package required for integrity of restored objects to be verified")
  }

  object

}

