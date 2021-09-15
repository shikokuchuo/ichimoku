% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/look.R
\name{look}
\alias{look}
\title{Look at Informational Attributes}
\usage{
look(x, which)
}
\arguments{
\item{x}{an object.}

\item{which}{(optional) integer value of strategy to return from an autostrat
list.}
}
\value{
For objects created by the ichimoku package, a list of attributes
    specific to that data type, or if 'which' is specified on an autostrat
    list, an ichimoku object containing a strategy.

    For other objects, a list of attributes that are non-standard for matrix /
    data.frame / xts objects, or else invisible NULL if none are present.
}
\description{
Inspect the informational attributes of objects, or extract ichimoku objects
    from lists returned by \code{\link{autostrat}}.
}
\details{
Note: for a level 2 autostrat object, if the attributes fail to print
    correctly due to their length, please access the them directly using
    \code{look(x)$summary} and \code{look(x)$logret}, possibly in conjunction
    with head() or by setting the 'max' argument in print().
}
\examples{
cloud <- ichimoku(sample_ohlc_data, ticker = "TKR")
look(cloud)

stratlist <- autostrat(cloud, n = 3)
look(stratlist)

strat <- look(stratlist, which = 1)
look(strat)

grid <- mlgrid(cloud)
look(grid)

\dontrun{
# OANDA API key required to run this example
prices <- oanda("USD_JPY")
look(prices)
}

}