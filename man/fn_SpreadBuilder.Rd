\name{fn_SpreadBuilder}
\alias{fn_SpreadBuilder}
\title{Calculate prices of a spread from 2 instruments.}
\usage{fn_SpreadBuilder(prod1, prod2, ratio=1, from, to, session_times,
    unique_method=c("make.index.unique", "duplicated", "least.liq",
    "price.change"), ...)}
\description{Calculate prices of a spread from 2 instruments.}
\details{Before calling, both products must be defined as instruments.  

It will try to get data for \code{prod1} and \code{prod2} from .GlobalEnv.  
If it cannot find the data, it will get it with a call to getSymbols. 

Prices are multiplied by multipliers and exchange rates to get notional values in USD using the most recent exchange rate.
The second leg's new values are multiplied by the ratio. Then the difference is taken between the new values for leg1 and the new values for leg2.

\sQuote{make.index.unique} uses the xts function \code{make.index.unique} 
\sQuote{least.liq} subsets the spread time series, by using the timestamps of the leg that has the fewest rows.
\sQuote{duplicated} removes any duplicate indexes.
\sQuote{price.change} only return rows where there was a price change in the Bid, Mid or Ask Price of the spread.}
\value{an xts object with
Bid, Ask, Mid columns, 
or Open, Close, Adjusted columns, 
or Open, Close columns.
or Price column.}
\author{Lance Levenson, Brian Peterson, Garrett See}
\note{Currently, this doesn't really support multi-currency spreads.  
If an instrument is not denominated in USD, it will try to get data for that currency from 
the .GlobalEnv. However, it is unlikey that you would have data stored in an object called e.g. \sQuote{EUR}.
So, it probably won't find the data. This will soon be updated to look for exchange rates. 
Also, a parameter should be added for spread currency instead of requiring that the spread 
be denominated in USD.}
\seealso{\code{\link{buildSpread}}
\code{\link{synthetic.instrument}}
\code{\link{formatSpreadPrice}}}
\arguments{\item{prod1}{chr name of instrument that will be the 1st leg of a 2 leg spread}
\item{prod2}{chr name of instrument that will be the 2nd leg of a 2 leg spread}
\item{ratio}{hedge ratio.}
\item{from}{from Date to pass through to getSymbols if needed.}
\item{to}{to Date to pass through to getSymbols if needed.}
\item{session_times}{ISO-8601 time subset for the session time, in GMT, in the format 'T08:00/T14:59'}
\item{unique_method}{method for making the time series unique}
\item{\dots}{any other passthrough parameters}}
\examples{\dontrun{
currency("USD")
stock("SPY")
stock("DIA")
getSymbols(c("SPY","DIA"))
fSB <- fn_SpreadBuilder("SPY","DIA")
head(fSB)
}}
