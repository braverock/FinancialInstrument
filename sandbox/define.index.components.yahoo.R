
#' Define an Index and it's components using yahoo
#'
#' Get the components of an index and define instruments
#' @param Symbol character yahoo ticker symbol for a stock index (e.g. "^DJI" or "^GDAX")
#' @param currency
#' @return called for side-effect, but it will return a list with 2 components:
#' \item{synthetic}{name of the \code{\link{synthetic}} instrument that was defined to hold the metadata of the index.} 
#' \item{stock}{name of the component \code{\link{stock}}s that were defined}
#' @note Depends on XML package
#' @author Garrett See
#' @examples
#' \dontrun{
#' define_components.yahoo('^STOXX50E', 'EUR')
#' define_components.yahoo('^DJI', 'USD')
#' }
define_components.yahoo <- function(Symbol, currency) {
    require(FinancialInstrument)
    require(XML)
    ccy <- currency(currency) #make sure it's defined
    x <- readHTMLTable(paste("http://finance.yahoo.com/q/cp?s=",Symbol,"+Components", sep=""))
    mdata <- x[which.max(sapply(x, NROW))][[1]][,1:2]
    new.Symbol <- synthetic(Symbol, ccy, members=paste(mdata[,1]), src=list(src='yahoo',name=Symbol), identifiers=list(yahoo=Symbol))
    if (!identical(integer(0), grep("There is no Components data", mdata[,1]))) stop("No Components Data Available for ", Symbol)
    for (i in 1:NROW(mdata)) {
        stock(paste(mdata[i,1]), currency=ccy, Name=paste(mdata[i,2]), member.of=new.Symbol)
    }
    list(synthetic=new.Symbol, stock=paste(mdata[,1]))
}

#define_components.yahoo('^STOXX50E','EUR')
#Symbol <- '^STOXX50E'
#instr <- getInstrument("^STOXX50E")
#instr
#memb5 <- getInstrument(instr$members[5])
#memb5
#getInstrument(memb5$member.of)

