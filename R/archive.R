# Ichimoku - Data Layer --------------------------------------------------------

#' Read/write Objects <> Archive Files with Data Verification
#'
#' Read and write objects to/from archival storage in the native RData format,
#'     with verification of data integrity.
#'
#' @param ... unnamed arguments will be parsed as 'file' if there is only one
#'     argument, 'object' and 'file' if there are two arguments.
#' @param object (for write operations) an object.
#' @param file the name of the file or a connection where the object is saved to
#'     or read from.
#'
#' @return For read operations: the object originally archived.
#'
#'     For write operations: invisible NULL. 'object' is written to 'file'.
#'
#' @details For read operations: specify only 'file'. 'file' is read and the
#'     return value may be assigned to an object. A confirmation message is
#'     issued if the file read operation has been successful.
#'
#'     For write operations: specify both 'object' and 'file'. 'object' will be
#'     written to 'file'. A confirmation message is issued if the file write
#'     operation has been successful.
#'
#' @section Data Verification:
#'
#'     If the 'openssl' package is available, a sha256 hash of the original
#'     object is written to the archive. This allows the data integrity of the
#'     restored object to be verified when the archive is read back.
#'
#'     For write operations: confirmation of the sha256 hash is displayed if
#'     this has been successfully written to file.
#'
#'     For read operations: a 'data verified' message is issued if the sha256
#'     hash found within the data file has been authenticated.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' file <- tempfile()
#'
#' archive(cloud, file)
#'
#' restored <- archive(file)
#'
#' unlink(file)
#'
#' @export
#'
archive <- function(..., object, file) {

  if (missing(object) && missing(file)) {

    dots <- list(...)

    if (length(dots) == 1L) {
      file <- dots[[1L]]
      readArchive(file = file)

    } else if (length(dots) == 2L) {
      object <- dots[[1L]]
      file <- dots[[2L]]
      writeArchive(object = object, file = file)

    } else if (length(dots) > 2L) {
      stop("Too many arguments passed to archive()",
           "\nFor read operations specify 'file' only",
           "\nFor write operations specify both 'object' and 'file'", call. = FALSE)

    } else {
      stop("archive() is used to read/write objects to/from archive files",
           "\nFor read operations specify 'file' only",
           "\nFor write operations specify both 'object' and 'file'", call. = FALSE)
    }

  } else if (!missing(file)) {

    if (missing(object)) {
      readArchive(file = file)
    } else {
      writeArchive(object = object, file = file)
    }

  } else {
    stop("in archive(object, file): 'object' specified without 'file'", call. = FALSE)
  }

}

#' Write Objects to Archive
#'
#' Internal function used to write objects, along with their sha256 hash value,
#'     to archive files in the native RData format.
#'
#' @param object an object.
#' @param file the name of the file or a connection where the object is saved to
#'     or read from.
#'
#' @return Invisible NULL. 'object' is written to 'file'.
#'
#' @noRd
#'
writeArchive <- function(object, file) {

  if (!is.character(file)) {
    stop("in archive(object, file): 'file' must be supplied as a string. ",
         "\nDid you omit the surrounding quotes \"\"?", call. = FALSE)
  }

  if (file.exists(file)) {
    continue <- readline(prompt = paste0("The file '", file, "' already exists. Overwrite? [y/N] "))
    if (!continue %in% c("y", "Y", "yes", "YES")) {
      message("Request cancelled")
      return(invisible())
    }
  }

  x_archive_sha256 <- NA
  if (requireNamespace("openssl", quietly = TRUE)) {
    x_archive_sha256 <- openssl::sha256(serialize(object = object, connection = NULL))
  }

  save(object, x_archive_sha256, file = file, compress = TRUE)
  message("Archive written to '", file, "'\nsha256: ", x_archive_sha256,
          if (is.na(x_archive_sha256[1L])) " ['openssl' package not installed]")
  invisible()

}

#' Read Objects from Archive
#'
#' Internal function used to read objects from native RData files with stored
#'     sha256 hash values.
#'
#' @param file the name of the file or a connection where the object is saved to
#'     or read from.
#'
#' @return The object that was originally archived.
#'
#' @noRd
#'
readArchive <- function(file) {

  if (!is.character(file)) {
    stop("in archive(file): 'file' must be supplied as a string. ",
         "\nDid you omit the surrounding quotes \"\"?", call. = FALSE)
  }

  x_archive_names <- load(file)
  if (!length(x_archive_names) == 2L || !identical(x_archive_names[2L], "x_archive_sha256")) {
    stop("archive file was not created by archive()", call. = FALSE)
  }
  object <- get(x_archive_names[1L])
  x_archive_sha256 <- get(x_archive_names[2L])

  if (is.na(x_archive_sha256[1L])) {
    message("Archive read from '", file, "'\nData unverified: sha256 hash not present")

  } else if (requireNamespace("openssl", quietly = TRUE)) {

    sha256 <- openssl::sha256(serialize(object = object, connection = NULL))

    if (identical(sha256, x_archive_sha256)) {
      message("Archive read from '", file, "'\nData verified by sha256: ", sha256)
    } else {
      message("Archive read from '", file, "'")
      warning("sha256 of restored object\n", sha256,
              " does not match the original\n", x_archive_sha256, call. = FALSE)
    }
  } else {
    message("Archive read from '", file,
            "'\n'openssl' package required for authentication of restored objects")
  }

  object

}

