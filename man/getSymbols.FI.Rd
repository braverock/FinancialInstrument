\name{getSymbols.FI}
\alias{getSymbols.FI}
\title{getSymbols method for loading data from split files...}
\usage{getSymbols.FI(Symbols, from="2010-01-01", to=Sys.Date(), ..., env,
    dir="", return.class="xts", extension="rda",
    date_format="\%Y.\%m.\%d")}
\description{getSymbols method for loading data from split files}
\details{This function should probably get folded back into getSymbols.rda in quantmod.

Meant to be called internally by \code{\link[quantmod]{getSymbols}} .

The symbol lookup table will most likely be loaded by \code{\link{setSymbolLookup.FI}}}
\seealso{\code{\link{setSymbolLookup.FI}}
\code{\link{load.instruments}}
\code{\link[quantmod]{getSymbols}}}
\arguments{\item{Symbols}{a character vector specifying the names of each symbol to be loaded}
\item{from}{Retrieve data no earlier than this date. Default '2010-01-01'.}
\item{to}{Retrieve data through this date. Default Sys.Date().}
\item{...}{any other passthru parameters}
\item{env}{where to create objects. Default .GlobalEnv}
\item{dir}{if not specified in getSymbolLookup, directory string to use.  default ""}
\item{return.class}{only "xts" is currently supported}
\item{extension}{file extension, default "rda"}
\item{date_format}{format as per the \code{\link{strptime}}, default '\%Y.\%m.\%d'}}
