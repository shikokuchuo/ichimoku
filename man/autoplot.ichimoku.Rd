% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{autoplot}
\alias{autoplot}
\alias{autoplot.ichimoku}
\title{Plot Ichimoku Objects with ggplot2}
\usage{
\method{autoplot}{ichimoku}(
  object,
  window,
  ticker,
  subtitle,
  theme = c("classic", "dark", "mono", "noguchi", "okabe-ito", "solarized"),
  strat = TRUE,
  type = c("none", "r", "s", "bar", "line"),
  custom,
  ...
)
}
\arguments{
\item{object}{an object of class 'ichimoku'.}

\item{window}{(optional) a date-time window to subset the plot, in ISO-8601
compatible range strings of the format used for 'xts' objects, for example
"2020-02-15/2020-08-15" or "2020-02-15/", "/2020-08" or "2020-07".}

\item{ticker}{(optional) specify a ticker (or other text) to include in the
chart heading. If not set, the ticker saved within the ichimoku object
will be used.}

\item{subtitle}{(optional) specify a subtitle to display under the chart title.}

\item{theme}{[default 'classic'] with further choices of 'dark', 'mono',
'noguchi', 'okabe-ito' or 'solarized'. Alternatively, supply a vector
of 12 colour values (hex codes or names) as a user-defined theme.}

\item{strat}{[default TRUE] if the ichimoku object contains a strategy, the
periods for which the strategy results in a position will be shaded, and
the strategy printed as the chart subtitle (if not otherwise specified).
Set to FALSE to turn off this behaviour.}

\item{type}{[default 'none'] type of sub-plot to display beneath the ichimoku
cloud chart, with a choice of 'none', 'r' or 's' for the corresponding
oscillator type, and 'bar' or 'line' for custom plots.}

\item{custom}{(optional) character string (containing a regular expression)
matching the column name of the variable to be displayed as sub-plot.
Specify \code{type = 'bar'} or \code{type = 'line'}, otherwise other type
settings will take precedence.}

\item{...}{other arguments not used by this method.}
}
\value{
A ggplot2 object with S3 classes 'gg' and 'ggplot'.
}
\description{
Plot Ichimoku Kinko Hyo cloud charts from ichimoku objects.
}
\details{
This function is an S3 method for the generic function autoplot()
    for class 'ichimoku'. It can be invoked by calling autoplot(x) on an
    object 'x' of class 'ichimoku'.
}
\keyword{internal}
