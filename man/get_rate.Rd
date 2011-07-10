\name{.get_rate}
\alias{.get_rate}
\title{get an exchange rate series...}
\usage{.get_rate(ccy1, ccy2, env=.GlobalEnv)}
\description{get an exchange rate series}
\details{Try to find exchange rate data in an environment, inverting if necessary.}
\value{xts object with as many columns as practicable.}
\author{Garrett See}
\seealso{\code{\link{buildRatio}}
\code{\link{redenominate}}}
\arguments{\item{ccy1}{chr name of 1st currency}
\item{ccy2}{chr name of 2nd currency}
\item{env}{environment in which to look for data.}}
\examples{\dontrun{
EURUSD <- getSymbols("EURUSD=x",src='yahoo',auto.assign=FALSE)
USDEUR <- .get_rate("USD","EUR")
head(USDEUR)
head(EURUSD)
}}
