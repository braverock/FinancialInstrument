\name{FinancialInstrument-package}
\alias{FinancialInstrument-package}
\alias{FinancialInstrument}
\docType{package}
\title{
Construct, manage and store contract specifications for trading
}
\description{
Transaction-oriented infrastructure for defining tradable instruments based on their contract specifications.  Construct and manage the definition of any asset class, including derivatives, exotics and currencies.  Potentially useful for portfolio accounting, backtesting, pre-trade pricing and other financial research.  Still in heavy development.
}
\details{
The FinancialInstrument package provides a construct for defining and storing meta-data for tradable contracts (referred to as instruments, e.g., stocks, futures, options, etc.).  It can be used to create any asset class and derivatives, across multiple currencies.  

FinancialInstrument was originally part of a companion package, blotter, that provides portfolio accounting functionality.  Blotter accumulates transactions accumulate into positions, then into portfolios and an account.  FinancialInstrument is used to contain the meta-data about an instrument, which blotter uses to calculate the notional value of positions and the resulting P&L.  FinancialInstrument, however, has plenty of utility beyond portfolio accounting despite it's small size, and was carved out so that others might take advantage of its functionality.

As used here, 'instruments' are S3 objects of type 'instrument' or a subclass thereof that define contract specifications for price series for a tradable contract, such as corn futures or IBM common stock.  When defined as instruments, these objects are extended to include descriptive information and contract specifications that help identify and value the contract.

The simplest example of an instrument is a common stock.  A tradable instrument can be defined in brief terms with an identifier (e.g., "IBM").  Beyond the primary identifier, additional identifiers may be added as well and will work as 'aliases'.  Any identifier will do -- Bloomberg, Reuters-RIC, CUSIP, etc. -- as long as it's unique to the workspace. In addition, a stock price will be denominated in a currency (e.g., "USD") and will have a specific tick size which is the minimum amount that the price can be quoted and transacted in (e.g., $0.01).  We also define a 'multiplier' that is used when calculating the notional value of a position or transaction using a quantity and price (sometimes called a contract multiplier).  For a stock it's usually '1'.  

More care is needed when dealing with complex instruments, like futures.  First, we have to define a future as a root contract.  This root is not tradable unto itself, but is used to generate a series of futures which are tradable and expire through time.  The root contract will provide an identifier (e.g., 'C' for the CME's corn contract), a denomination currency, a multiplier (one futures contract will cover multiple items) and a minimum tick size.  From that definition, a series of expiring contracts can be generated ("CH08", "CZ08", etc.) by specifying a suffix to be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year.  As you might expect, options are treated similarly.  The package also includes constructors for certain synthetic instruments, such as ratio spreads.

FinancialInstrument doesn't try to exhaust the possibilities of attributes, so it instead allows for flexibility.  If you wanted to add an attribute to tag the exchange the instrument is listed on, just add it when defining the instrument (e.g., \code{future('CL', multiplier=1000, currency="USD", tick_size=.01, exchange="CME", description="Crude Light futures")}).  Or, as you can see, we've found it useful to add a field with more slightly more detail, such as \code{description='IBM Common Stock'}.  

That flexibility also extends to creating instruments that the package doesn't define.  \code{@TODO: Create an example of an instrument not covered by the package, such as a corporate bond.}  We would greatly appreciate any contributions in this vein.

Defining instruments can be tedious, so we've also included a CSV loader in in the package too. See \code{\link{load.instruments}} for detail.  

Once you've defined all these instruments (we keep hundreds or thousands of them in our environments), and you keep a local store of all your historical market data, you may want to use \code{\link{setSymbolLookup.FI}} to define where and how they are stored so that \code{\link[quantmod]{getSymbols}} will work for you. 

FinancialInstrument's functions build and manipulate objects that are stored in an environment named ".instrument" rather than the global environment, \code{.GlobalEnv}.  Objects may be listed using \code{ls(envir=.instrument)}. See \code{\link{environment}} for more detail. 
We do that for two reasons.  First, it keeps the user's workspace less cluttered and lowers the probability of clobbering something.  Second, it allows the user to save and re-use the \code{.instrument} environment in other workspaces.  Objects created with FinancialInstrument may be directly manipulated as any other object, but in our use so far we've found that it's relatively rare to do so.  Use the \code{\link{getInstrument}} function to query the contract specs of a particular instrument from the environment.

}
\author{
Peter Carl
Brian Peterson

Maintainer: Brian Peterson <brian@braverock.com>
}
\keyword{ package }
\seealso{
\code{\link[quantmod:quantmod-package]{quantmod}}
\code{\link[blotter:blotter-package]{blotter}}
\code{\link[xts:xts-package]{xts}}
\code{\link[PerformanceAnalytics:PerformanceAnalytics-package]{PerformanceAnalytics}}
}
\examples{
# Construct instruments for several different asset classes
## The next line is here to deal with frame issues in R CMD check
## and ARE NOT NECESSARY to run by hand in your own environment.
if(!exists(".instrument")) .instrument <<- new.env()

# Define a currency and some stocks
    require(FinancialInstrument)
    currency("USD")
    symbols = c("IBM","F","MMM")
    for(symbol in symbols){ # establish tradable instruments
        stock(symbol, currency="USD",multiplier=1)
    }

}
