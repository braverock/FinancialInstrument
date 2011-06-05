\name{option_series.yahoo}
\alias{option_series.yahoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
constructor for series of options using yahoo data
}
\description{
Defines a chain or several chains of options by looking up necessary info from yahoo 
}
\usage{
option_series.yahoo(symbol, Exp, currency = "USD", multiplier = 100, first_traded = NULL, tick_size = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
ticker symbol of the underlying instrument (Currently, should only be stock tickers)
}
  \item{Exp}{
Expiration date or dates to be passed to getOptionChain
}
  \item{currency}{
currency of underlying and options
}
  \item{multiplier}{
contract multiplier. Usually 100 for stock options
}
  \item{first_traded}{
first date that contracts are tradeable. Probably not applicable if defining several chains.
}
  \item{tick_size}{
minimum price change of options. 
}
}
\details{
If \code{Exp} is missing it will define only the nearby options. 
If \code{Exp} is NULL it will define all options

If \code{first_traded} and/or \code{tick_size} should not be the same for all options being defined, they should be left NULL and defined outside of this function.
}
\value{
Called for side-effect. The instrument that is created and stored will inherit option_series, option, and instrument classes. 
}
\references{
Yahoo \url{http://finance.yahoo.com}
}
\author{
Garrett See
}
\note{
Has only been tested with stock options.
Does not support quanto options -- The options' currency should be the same as the underlying's.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
option_series, option, instrument, getOptionChain
}
\examples{
\dontrun{
option_series.yahoo('SPY') #only nearby calls and puts
option_series.yahoo('DIA', Exp=NULL) #all chains
ls(.instrument, all.names=TRUE)
}
}

