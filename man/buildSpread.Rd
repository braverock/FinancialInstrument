\name{buildSpread}
\alias{buildSpread}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
build price series for pre-defined multi-leg spread instrument
}
\description{
build price series for spreads, butterflies, or other synthetic instruments, using metadata of a previously defined synthetic instrument.
}
\usage{
buildSpread(spread_id, Dates = NULL, onelot=TRUE, prefer = NULL, auto.assign=TRUE, env=.GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spread_id}{
The name of the instrument that contains members and memberratio  
}
  \item{Dates}{
date range to subset on; currently not implemented
}
  \item{onelot}{
Should the series be divided by the first leg's ratio 
}
  \item{prefer}{
price column to use to build structure.
}
  \item{auto.assign}{assign the spread? If FALSE, the xts object will be returned}
  \item{env}{environment to assign spread into.}
}
\details{
The spread and all legs must be defined instruments.

This function can build multileg spreads such as calendars, butterflies, condors, etc. However, the returned series will be univariate. It does not build Bid Ask Mid data like fn_SpreadBuilder does.
}
\value{
If \code{auto.assign} is FALSE, a univariate xts object 
otherwise, the xts object will be assigned to \code{spread_id} and the \code{spread_id} will be returned.
}
\author{
Brian Peterson, Garrett See
}
\note{
This function requires that the spread to be build be defined using one of the twsInstrument functions. Specifically, synthetic.ratio from FinancialInstrument should not be used. synthetic.ratio stores memberratios deeper inside the instrument (instr$members$members instead of instr$members)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
fn_SpreadBuilder
}
\examples{
\dontrun{
currency("USD")
stock("SPY","USD",1)
stock("DIA","USD",1)
spread("SPYDIA", "USD", c("SPY","DIA"),c(1,-1))
getSymbols(c("SPY","DIA"))
buildSpread('SPYDIA')
head(SPYDIA)
}
}

