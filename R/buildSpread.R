#' construct a price/level series for a spread
#' 
#' this function should provide a generic spread series builder.  
#' 
#' 
#' @param spread_id string descrining the primary_id of an instrument of type 'spread 
#' @param ... any other passthru parameters
#' @param Dates date range to subset on, will be used for \code{\link[quantmod]{getSymbols}} if the instrument is not available via \code{\link{get}}
#' @param prefer 
#' @seealso 
#' \code{\link{spread}} for instructions on defining the spread
#' @author bpeterson
#' @export
buildSpread<- function(spread_id, ..., Dates = NULL, prefer='Mid.Price', onelot=FALSE, method=c('Close','Midpoint','BA','BB')) {
    #TODO subset using Dates arg?  or let the +/- operators deal with it?
    #TODO FIXME put some intelligence in the subsetting and intersection, maybe up front or in a checkData style
    spread_instr<-try(getInstrument(spread_id))
    if(inherits(spread_instr,"try-error") | !is.instrument(spread_instr)){
        stop(paste("Instrument",spread_instr," not found, please create it first."))
    } 
    if(!inherits(spread_instr,"spread")) stop (paste("Instrument", spread_id, " is not a spread, please use the symbol of a spread instrument."))

    spread_currency<-spread_instr$currency
    stopifnot(is.currency(spread_currency)) #TODO add assumption of Currency multiplier of 1?
    
    times <- .parseISO8601(Dates)
    from  <- times$first.time
    to    <- times$to.time
    
    # now build each spread factor and add them up
    spreadseries<-NULL
    for(i in 1:length(spread_instr$memberlist$members)) {
        instr<-try(getInstrument(as.character(spread_instr$memberlist$members[i])))
        if(inherits(instr,"try-error") | !is.instrument(instr)){
            stop(paste("Instrument",instr," not found, please create it first."))
        } else {
            #TODO check to see if instrument is a 'root symbol' instrument like a future or option
            instr_currency<-instr$currency
            if(i==1) primary_currency=instr_currency
            stopifnot(is.currency(instr_currency))
            if(!all.equal(primary_currency,instr_currency)){
                instr_currency<-instr$currency
                stopifnot(is.currency(instr_currency))
                exchange_rate<-try(get( paste(primary_currency,instr_currency,sep='')))
                if(inherits(exchange_rate,"try-error")){
                    exchange_rate<-try(get( paste(instr_currency,primary_currency,sep='')))
                    if(inherits(exchange_rate,"try-error")){
                        stop(paste("Exchange Rate", paste(primary_currency, instr_currency, sep=''), "not found."))    
                    } else {
                        exchange_rate <- 1/exchange_rate
                    }   
                }
            } else {
                #currencies of both instruments are the same
                exchange_rate=1
            }
            instr_mult<-instr$multiplier
            instr_ratio<-spread_instr$memberlist$memberratio[i]
            #TODO get both bid and ask?
            #instr_prices<-getPrice(get(as.character(spread_instr$memberlist$members[i])),prefer=prefer)
            instr_prices<-try(get(as.character(spread_instr$memberlist$members[i])))
            if(inherits(instr_prices,"try-error")){
                instr_prices<-getSymbols(as.character(spread_instr$memberlist$members[i]),from=from,to=to)
            }
        }        
        instr_norm<-instr_prices*instr_mult*instr_ratio*exchange_rate
        colnames(instr_norm)<-paste(as.character(spread_instr$memberlist$members[i]),prefer,sep='.')
        if(is.null(spreadseries)) spreadseries<-instr_norm else spreadseries=merge(spreadseries,instr_norm)
    }

    # Fill in merged time stamps.  This is correct for Bid and Ask, but may produce bad results with close.
    for(col in 1:ncol(spreadseries)) {
        spreadseries[,col]<-na.locf(spreadseries[,col])
    }    
    spreadseries<-na.omit(spreadseries)
    
    if(onelot) spreadlevel = spreadlevel/spread_instr$memberlist$memberratio[1]

    return(spreadlevel)
}

#' spread builder
#' @param prod1 product 1 identifier for use by getSymbols and getInstrument
#' @param prod2 product 1 identifier for use by getSymbols and getInstrument
#' @param from date string in ISO format YYYY-MM-DD
#' @param to   date string in ISO format YYYY-MM-DD
#' @param ratio ratio to calculate
#' @param session_times ISO-8601 time subset for the session time, in GMT, in the format 'T08:00/T14:59'
#' @param unique_method method for making the time series unique, see Details
#' @author Lance Levenson, Brian Peterson
#' @export
fn_SpreadBuilder <- function(prod1, prod2, from, to, ratio, session_times=NULL, unique_method=c('make.index.unique','duplicated','least.liq','price.change'))
{
    #print(paste(date," ",prod1,".",prod2,sep=""))
    
    unique_method<-unique_method[1]
    
    Data.1 <- NULL
    Data.2 <- NULL
    
    # put the instrument data into this temporary environment in the function
    tmpenv<-new.env()
    
    
    getSymbols(prod1,from=from,to=to,env=tmpenv)
    Data.1 <- get(prod1,env=tmpenv)        
    getSymbols(prod2,from=from,to=to,env=tmpenv)
    Data.2 <- get(prod2,env=tmpenv) 
    
    prod1.instr <- getInstrument(prod1)
    prod2.instr <- getInstrument(prod2)
    
    Mult.1 <- prod1.instr$multiplier 
    Mult.2 <- prod2.instr$multiplier 
    
    #TODO FIXME we probably need to setSymbolLookup to oanda, and look up the cross rate.
    if (prod1.instr$currency != 'USD'){
        Cur.1 <- get(prod1.instr$currency)
        Cur.1 <- as.numeric(last(Cur.1[to]))
    } else { Cur.1 <- 1 }
    
    if (prod2.instr$currency != 'USD'){
        Cur.2 <- get(prod2.instr$currency)
        Cur.2 <- as.numeric(last(Cur.2[to]))
    } else { Cur.2 <- 1 }
    
    
    M <- merge(Data.1[,c("Bid.Price","Ask.Price")],Data.2[,c("Bid.Price","Ask.Price")])
    names(M) <- c("Bid.Price.1","Ask.Price.1","Bid.Price.2","Ask.Price.2")
    
    fn_split <- function(DF)
    {   
        DF.split <- split(DF,"days")
        ret <- NULL
        
        for(d in 1:length(DF.split))
        {
            tmp <- na.locf(DF.split[[d]])
            tmp <- na.omit(tmp)
            ret <- rbind(ret,tmp)   
        }
        #attr(attr(ret,"index"),"tzone") <- "GMT" # no longer needed?
        #attr(ret,".indexTZ") <- "GMT" # no longer needed?
        ret
    }
    
    M<- fn_split(M)
    
    #can't subset times until after the merge
    if(!is.null(session_times)){
        #Data.1 <- Data.1[time.sub.GMT]
        #Data.2 <- Data.2[time.sub.GMT]
        M <- M[session_times]
    }
    
    M$Bid.Price.1 <- M$Bid.Price.1 * Mult.1 * Cur.1 
    M$Ask.Price.1 <- M$Ask.Price.1 * Mult.1 * Cur.1
    M$Bid.Price.2 <- M$Bid.Price.2 * Mult.2 * Cur.2
    M$Ask.Price.2 <- M$Ask.Price.2 * Mult.2 * Cur.2
    
    bid <- M$Bid.Price.1 - ratio * M$Ask.Price.2
    ask <- M$Ask.Price.1 - ratio * M$Bid.Price.2
    
    Spread <- cbind(bid,ask)
    names(Spread) <- c("Bid.Price","Ask.Price")
    Spread$Mid.Price <- (Spread$Bid.Price + Spread$Ask.Price) / 2
    
    switch(unique_method,
            make.index.unique = {Spread<-make.index.unique(Spread)},
            least.liq = {
                #determine the least liquid
                idx1 <- index(na.omit(getPrice(Data.1)))
                idx2 <- index(na.omit(getPrice(Data.2)))
                if(length(idx1)<length(idx2)) idx<-idx1 else idx <- idx2
                
                #subset the Spread
                Spread <- Spread[idx]
            },
            duplicated = {
                Spread <- Spread[!duplicated(index(Spread))]  #this may still be useful for instrument with huge numders of observations 
            },
            price.change = {
                Spread <- Spread[which(diff(Spread$Mid.Price)!=0 | 
                                        diff(Spread$Bid.Price)!=0 | 
                                        diff(Spread$Ask.Price)!=0) ,]
                
            }
    )
    
    return(Spread)  
}

###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
