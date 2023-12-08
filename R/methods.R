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

# Ichimoku - Core Functions ----------------------------------------------------

#' Print Ichimoku Objects
#'
#' Default print method for ichimoku objects to enable automatic plotting of the
#'     ichimoku cloud chart.
#'
#' @param x an object of class 'ichimoku'.
#' @param plot [default TRUE] set to FALSE to prevent automatic plotting of
#'     the ichimoku cloud chart.
#' @param rows [default 26L] integer number of rows to print.
#' @param ... additional arguments passed along to the xts print and
#'     \code{\link{plot.ichimoku}} methods.
#'
#' @return The ichimoku object supplied (invisibly). The data is printed to the
#'     console. The ichimoku cloud chart is also output to the graphical device
#'     depending on the parameters set.
#'
#' @details This function is an S3 method for the generic function print() for
#'     class 'ichimoku'. It can be invoked by calling print(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#'
#' print(cloud)
#' print(cloud, plot = FALSE, rows = 20L)
#'
#' @method print ichimoku
#' @export
#'
print.ichimoku <- function(x, plot = TRUE, rows = 26L, ...) {

  cat(" ichimoku   [ more() to display more rows | look() to inspect attributes ]\n  object\n", file = stdout())
  dim <- attr(x, "dim")
  NextMethod(max = if (length(dim)) dim[2L] * rows, ...)
  if ((missing(plot) || isTRUE(plot)) && length(dim) && dim[1L] > 1L && dim[2L] >= 12L) plot.ichimoku(x, ...)
  invisible(x)

}

#' Display the Structure of Ichimoku Objects
#'
#' Compactly display the internal structure of ichimoku objects.
#'
#' @param object an object of class 'ichimoku'.
#' @param ... arguments passed to or from other methods.
#'
#' @return Invisible NULL. A compact display of the structure of the object is
#'     output to the console.
#'
#' @details This function is an S3 method for the generic function str()
#'     for class 'ichimoku'. It can be invoked by calling str(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' str(cloud)
#'
#' strat <- strat(cloud)
#' str(strat)
#'
#' @rdname str.ichimoku
#' @method str ichimoku
#' @export
#'
str.ichimoku <- function(object, ...) {

  dims <- attr(object, "dim")
  if (is.null(dims)) {
    xlen <- length(object)
    dates <- format.POSIXct(index.ichimoku(object, c(1L, xlen)))
    cat("ichimoku object with no dimensions\nVector <numeric> w/ length:", xlen, file = stdout())
  } else {
    dates <- format.POSIXct(index.ichimoku(object, c(1L, dims[1L])))
    cat("ichimoku object [", dates[1L], " / ", dates[2L], "] (",
        dims[1L], ", ", dims[2L], ")", if (hasStrat(object)) " w/ strat",
        "\n <double> $", file = stdout(), sep = "")
    cat(attr(object, "dimnames")[[2L]], file = stdout(), sep = " $")
  }
  cat("\n index: <POSIXct>", dates[1L], "...", dates[2L],
      "\n attributes:\n  periods:", attr(object, "periods"),
      "\n  periodicity:",
      if ((periodicity <- attr(object, "periodicity")) >= 86400) {
        paste0(round(periodicity / 86400, digits = 1), " days")
      } else if (periodicity >= 3600) {
        paste0(round(periodicity / 3600, digits = 1), " hours")
      } else if (periodicity >= 60) {
        paste0(round(periodicity / 60, digits = 1), " mins")
      } else {
        paste0(periodicity, " secs")
      },
      "\n  ticker:", attr(object, "ticker"), "\n", file = stdout())
  if (hasStrat(object))
    cat("  strat: [strategy: ", attr(object, "strat")["Strategy", ][[1L]],
        " w/ direction: ", attr(object, "strat")["Direction", ][[1L]], "... ]\n",
        file = stdout(), sep = "")

}

#' Summary of Ichimoku Objects and Strategies
#'
#' Display summary information for an ichimoku object or its strategy.
#'
#' @param object an object of class 'ichimoku'.
#' @param strat [default TRUE] to show the strategy summary if present. Set to
#'     FALSE to show the object summary instead.
#' @param ... arguments passed to or from other methods.
#'
#' @return A matrix containing the strategy summary, if present and 'strat' is
#'     set to TRUE, otherwise a character vector containing an abbreviated object
#'     summary (the full object summary is output to the console).
#'
#' @details This function is an S3 method for the generic function summary() for
#'     class 'ichimoku'. It can be invoked by calling summary(x) on an object 'x'
#'     of class 'ichimoku'.
#'
#'     Performs basic validation for an ichimoku object and will inform if an
#'     ichimoku object contains invalid information.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
#' summary(cloud)
#'
#' strat <- strat(cloud)
#' summary(strat)
#'
#' @method summary ichimoku
#' @export
#'
summary.ichimoku <- function(object, strat = TRUE, ...) {

  if (hasStrat(object) && (missing(strat) || isTRUE(strat))) {
    summary <- NULL
    tryCatch(attr(object, "strat")["Strategy", ], error = function(e)
      cat(summary <<- "ichimoku object with invalid strategy"), file = stdout())
    if (is.null(summary)) attr(object, "strat") else invisible(summary)

  } else {
    (!is.integer(periods <- attr(object, "periods")) || length(periods) != 3L ||
       !is.numeric(periodicity <- attr(object, "periodicity")) || length(periodicity) != 1L) && {
         cat(summary <- "ichimoku object with invalid attributes", file = stdout())
         return(invisible(summary))
       }
    dims <- attr(object, "dim")
    if (is.null(dims)) {
      cat(summary <- "ichimoku object with no dimensions", "\n", file = stdout())
    } else if (dims[2L] < 12L) {
      cat(summary <- "incomplete ichimoku object (partial or subset)", "\n", file = stdout())
    } else {
      cat(summary <- paste0("ichimoku object with dimensions (", dims[1L], ", ",
                            dims[2L], ")"), "\n", file = stdout())
      if (dims[1L] != 0L) {
        core <- coredata.ichimoku(object)
        end <- sum(!is.na(core[, "close"]))
        high <- which.max(core[1:end, "high"])
        low <- which.min(core[1:end, "low"])
        dates <- format.POSIXct(index.ichimoku(object, c(1L, high, low, end)))
        cat("\n            Max: ", dates[2L], " [", core[high, "high"],
            "]\nStart: ", dates[1L], " [", core[1L, "open"],
            "]   End: ", dates[4L], " [", core[end, "close"],
            "]\n            Min: ", dates[3L], " [", core[low, "low"], "]\n",
            file = stdout(), sep = "")
      }
    }

    cat("\nCloud periods:", periods, "\nPeriodicity:",
        if (periodicity >= 86400) {
          paste0(round(periodicity / 86400, digits = 1), " days")
        } else if (periodicity >= 3600) {
          paste0(round(periodicity / 3600, digits = 1), " hours")
        } else if (periodicity >= 60) {
          paste0(round(periodicity / 60, digits = 1), " mins")
        } else {
          paste0(periodicity, " secs")
        },
        "\nTicker:", attr(object, "ticker"), "\n", file = stdout())

    invisible(summary)

  }

}

#' Convert ichimoku to data.frame
#'
#' An optimised 'ichimoku' to 'data.frame' constructor.
#'
#' @param x an object of class 'ichimoku'.
#' @param row.names not used.
#' @param optional not used.
#' @param keep.attrs [default FALSE] if set to TRUE, will preserve any custom
#'     attributes set on the original object.
#' @param ... arguments passed to or from other methods.
#'
#' @return A 'data.frame' object. The ichimoku object index is preserved as the
#'     first column with header 'index'.
#'
#' @details This function is an S3 method for the generic function
#'     as.data.frame() for class 'ichimoku'. It can be invoked by calling
#'     as.data.frame(x) on an object 'x' of class 'ichimoku'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' df <- as.data.frame(cloud)
#' str(df)
#'
#' df2 <- as.data.frame(cloud, keep.attrs = TRUE)
#' str(df2)
#'
#' @method as.data.frame ichimoku
#' @export
#'
as.data.frame.ichimoku <- function(x, row.names, optional, keep.attrs = FALSE, ...)
  .Call(ichimoku_tbl, x, isTRUE(keep.attrs))

#' @name coredata
#' @rdname coredata.ichimoku
#' @export
NULL

#' Extract the Core Data of Ichimoku Objects
#'
#' Method for extracting the core data matrix of ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param fmt (optional) set to TRUE to retain the index as row names of the
#'     returned matrix, or a character string passed on to the 'format' argument
#'     of \code{format.POSIXct()} to format these values in a specific way.
#' @param ... arguments passed to or from other methods.
#'
#' @return A numeric matrix containing the ichimoku object data, stripped of the
#'     index unless 'fmt' is specified in which case the index will be retained
#'     as character values in the matrix row names.
#'
#' @details This function is an S3 method for the generic function coredata()
#'     for class 'ichimoku'. It can be invoked by calling coredata(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' coredata(cloud)[101:120, ]
#'
#' @rdname coredata.ichimoku
#' @method coredata ichimoku
#' @export
#'
coredata.ichimoku <- function(x, fmt, ...)
  if (missing(fmt))
    .Call(ichimoku_coredata, x) else if (is.null(attr(x, "dim")))
      `attributes<-`(
        x,
        list(names = if (is.character(fmt)) format.POSIXct(index.ichimoku(x), format = fmt) else
          format.POSIXct(index.ichimoku(x)))
      ) else
        `attributes<-`(
          x,
          list(dim = attr(x, "dim"),
               dimnames = list(if (is.character(fmt)) format.POSIXct(index.ichimoku(x), format = fmt) else
                 format.POSIXct(index.ichimoku(x)), attr(x, "dimnames")[[2L]]))
        )

#' @name index
#' @rdname index.ichimoku
#' @export
NULL

#' Extract the Index of Ichimoku Objects
#'
#' Method for extracting the date-time index of ichimoku objects.
#'
#' @param x an object of class 'ichimoku'.
#' @param subset an integer or logical value or vector by which to subset the index.
#' @param ... arguments passed to or from other methods.
#'
#' @return The date-time index of the ichimoku object as a vector of POSIXct
#'     values.
#'
#' @details This function is an S3 method for the generic function index()
#'     for class 'ichimoku'. It can be invoked by calling index(x) on an
#'     object 'x' of class 'ichimoku'.
#'
#'     Subsetting by specifying the 'subset' parameter subsets using the
#'     numerical values underlying the POSIXct times and results in a faster
#'     operation than usual subset operators such as '['.
#'
#' @examples
#' cloud <- ichimoku(sample_ohlc_data)
#' index(cloud)[101:110]
#' index(cloud, 101:110)
#'
#' @rdname index.ichimoku
#' @method index ichimoku
#' @export
#'
index.ichimoku <- function(x, subset, ...)
  if (missing(subset))
    .Call(ichimoku_index, x) else
      .Call(ichimoku_psxct, .subset(attr(x, "index"), subset))
