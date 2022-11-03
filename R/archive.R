# Copyright (C) 2021-2022 Hibiki AI Limited <info@hibiki-ai.com>
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
#'     For write operations: the filename supplied. 'object' is written to 'file'.
#'
#' @details For read operations: specify only 'file', or alternatively if no
#'     arguments are specified, a system dialog will be opened allowing a file
#'     to be chosen interactively. 'file' is read and the return value may be
#'     assigned to an object. A confirmation message is issued if the file read
#'     operation has been successful.
#'
#'     For write operations: specify both 'object' and 'file'. If only 'object'
#'     is specified and 'file' is left empty (see examples), a system dialog
#'     will be opened allowing the file save location to be chosen interactively.
#'     'object' will be written to 'file'. A confirmation message is issued if
#'     the file write operation has been successful.
#'
#' @section Data Verification:
#'
#'     A SHA256 hash of the original object is written to the archive. This
#'     allows the data integrity of the restored object to be verified when the
#'     archive is read back.
#'
#'     For write operations: confirmation of the SHA256 hash written to file is
#'     displayed.
#'
#'     For read operations: a 'data verified' message is issued if the SHA256
#'     hash found within the data file has been authenticated.
#'
#' @section Further Details:
#'     Please refer to the reference vignette by calling:
#'     \code{vignette("reference", package = "ichimoku")}
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
#' if (interactive()) {
#' # Only run examples in interactive R sessions
#' # Read file to 'object' using system dialog:
#' object <- archive()
#'
#' # Write 'cloud' to file using system dialog:
#' archive(cloud, )
#' }
#'
#' @export
#'
archive <- function(..., object, file) {

  if (missing(object) && missing(file)) {

    dots <- substitute(alist(...))
    dlen <- length(dots)

    if (dlen == 1L && interactive()) {
      readArchive(file = file.choose())

    } else if (dlen == 2L) {
      readArchive(file = ..1)

    } else if (dlen == 3L) {
      if (dots[[2L]] == "") {
        (dots[[3L]] == "") && {
          interactive() ||
            stop("Empty arguments for both 'object' and 'file' passed to archive()\nFor read operations specify 'file' only, write operations both 'object' and 'file'", call. = FALSE)
          return(.deconstruct(...))
        }
        readArchive(file =  ..2)

      } else {
        writeArchive(
          object = ..1,
          file = if (dots[[3L]] == "" && interactive()) file.choose(new = TRUE) else ..2
        )
      }

    } else {
      stop(sprintf("%d arguments passed to archive() which requires 1 or 2\nFor read operations specify 'file' only, write operations both 'object' and 'file'", dlen - 1L), call. = FALSE)
    }

  } else if (!missing(file)) {
    if (missing(object))
      readArchive(file = file) else
        writeArchive(object = object, file = file)

  } else {
    if (interactive())
      writeArchive(object = object, file = file.choose(new = TRUE)) else
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
#' @return The filename supplied. 'object' is written to 'file'.
#'
#' @noRd
#'
writeArchive <- function(object, file) {

  is.character(file) ||
    stop("in archive(object, file): 'file' must be supplied as a string.\nDid you omit the surrounding quotes \"\"?", call. = FALSE)

  if (file.exists(file) && interactive()) {
    continue <- readline(prompt = paste0("The file '", file, "' already exists. Overwrite? [y/N] "))
    continue %in% c("y", "Y", "yes", "YES") || {
      message("Request cancelled")
      return(invisible())
    }
  }

  x_archive_sha256 <- sha256(object)
  save(object, x_archive_sha256, file = file, compress = TRUE)
  message(sprintf("Archive written to '%s'\nSHA256: %s", file, x_archive_sha256))
  invisible(file)

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

  is.character(file) ||
    stop("in archive(file): 'file' must be supplied as a string.\nDid you omit the surrounding quotes \"\"?", call. = FALSE)

  object <- x_archive_sha256 <- NULL
  x_archive_names <- load(file)
  x_archive_names[2L] == "x_archive_sha256" && x_archive_names[1L] == "object" ||
    stop("archive file was not created by archive()", call. = FALSE)

  message("Archive read from '", file, "'")
  if (is.na(x_archive_sha256[1L])) {
    # for legacy compatibility with previous implementations of archive
    message("Data unverified: SHA256 hash not present")
  } else {
    sha256 <- sha256(object)
    if (identical(sha256, as.character(x_archive_sha256)))
      message("Data verified by SHA256: ", sha256) else
        warning(sprintf("SHA256 of restored object:   %s\ndoes not match the original: %s", sha256, x_archive_sha256), call. = FALSE)
  }

  object

}

