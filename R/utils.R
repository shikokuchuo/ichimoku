# Ichimoku ---------------------------------------------------------------------

#' Select Trading Days
#'
#' Used by \code{\link{ichimoku}} to subset a vector of dates to trading days.
#'
#' @param x a vector of POSIXct date objects.
#' @param holidays (optional) a vector, or function which outputs a
#'     vector, of dates defined as holidays. If not specified, New Year's and
#'     Christmas day are defined as holidays by default.
#' @param ... other arguments to be passed along.
#'
#' @return A vector of logical values: TRUE if the corresponding element of 'x'
#'     is a weekday and not a holiday, FALSE otherwise.
#'
#' @examples
#' dates <- seq(from = as.POSIXct("2020-01-01"), by = "1 day", length.out = 7)
#' tradingDays(dates)
#'
#' @export
#'
tradingDays <- function(x, holidays, ...) {
  baseyear <- as.POSIXlt(x[1L])$year + 1900
  if(missing(holidays)) {
    holidays <- c(as.POSIXct(paste0(baseyear, "-01-01")),
                  as.POSIXct(paste0(baseyear, "-12-25")),
                  as.POSIXct(paste0(baseyear + 1, "-01-01")))
  } else {
    holidays <- tryCatch(as.POSIXct(holidays),
                         error = function(e) {
                           warning("ichimoku: specified holidays are invalid - disregarding",
                                   call. = FALSE)
                           c(as.POSIXct(paste0(baseyear, "-01-01")),
                             as.POSIXct(paste0(baseyear, "-12-25")),
                             as.POSIXct(paste0(baseyear + 1, "-01-01")))
                         })
  }
  vapply(x, function(y, i = holidays) {
    if(!as.POSIXlt(y)$wday %in% 1:5) FALSE
    else if(y %in% i) FALSE
    else TRUE
  }, logical(1L), USE.NAMES = FALSE)
}

#' Trim NA Values
#'
#' Trim NA values from a data.frame. A streamlined version of na.omit.data.frame.
#'
#' @param x the data.frame to trim.
#'
#' @return The data.frame 'x' with NA values removed.
#'
#' @examples
#' data <- data.frame(c(1:4, NA), c(NA, 2:5))
#' trimdf(data)
#'
#' @export
#'
trimdf <- function(x) {
  if(is.data.frame(x)) {
    omit <- logical(dim(x)[1L])
    for (i in 1:length(x)) {
      y <- x[[i]]
      if (!is.atomic(y))
        next
      y <- is.na(y)
      d <- dim(y)
      if (is.null(d) || length(d) != 2L)
        omit <- omit | y
      else for (ii in 1L:d[2L]) omit <- omit | y[, ii]
    }
    trim <- x[!omit, , drop = FALSE]
    trim
  }
}

#' Duplicates of expand.grid for 2 Variables
#'
#' Create a vector of element positions of duplicates in the output of expand.grid
#'     on 2 identical vectors. A much faster method of creating combinations for
#'     2 variabes vs. the combn() function from the utils package.
#'
#' @param n the length of vector passed to expand.grid.
#' @param identical defaults to FALSE to not select the elements where the 2 items
#'     are identical. Set to TRUE to also include these. The output of
#'     expand.grid, subset to remove duplicates with 'identical' set to TRUE would
#'     be the equivalent of \code{utils::combn(n, 2)}.
#'
#' @return A numeric vector.
#'
#' @examples
#' n <- 3
#' expand.grid(1:n, 1:n)
#' expand.grid(1:n, 1:n)[-duplicate(n), ]
#' expand.grid(1:n, 1:n)[-duplicate(n, identical = TRUE), ]
#'
#' @export
#'
duplicate <- function(n, identical = FALSE) {
  vec <- do.call(c, lapply(seq_len(n - 1), function(x) x * n + 1:x))
  if(isTRUE(identical)) {
    vec <- c(vec, do.call(c, lapply(seq_len(n), function(x) x + n * (x -1))))
  }
  vec
}

