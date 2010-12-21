#' construct a price/level series for a spread
#' 
#' this function should provide a generic spread series builder.  
#' 
#' 
#' @param spread_id string descrining the primary_id of an instrument of type 'spread 
#' @param ... any other passthru parameters
#' @param Dates date range to subset on, currently not implemented
#' @param onelot TRUE/FALSE, if TRUE, will divide by the number of units of the front leg to get a 'onelot'
#' @seealso 
#' \code{\link{spread}} for instructions on defining the spread
#' @author bpeterson
#' @export
buildSpread<- function(spread_id, ..., Dates = NULL, onelot=FALSE) {
    #TODO subset using Dates arg?  or let the +/- operators deal with it?
    #TODO FIXME put some intelligence in the subsetting and intersection, maybe up front or in a checkData style
    spread_instr<-try(getInstrument(spread_id))
    if(inherits(spread_instr,"try-error") | !is.instrument(spread_instr)){
        stop(paste("Instrument",spread_instr," not found, please create it first."))
    } 
    if(!inherits(spread_instr,"spread")) stop (paste("Instrument", spread_id, " is not a spread, please use the symbol of a spread instrument."))

    spread_currency<-spread_instr$currency
    stopifnot(is.currency(spread_currency)) #TODO add assumption of Currency multiplier of 1?
    
    # now build each spread factor and add them up
    spreadlevel<-NULL
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
                instr_currency<-instr_instr$currency
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
            
            instr_prices<-Cl(get(as.character(spread_instr$memberlist$members[i])))
        }        
        instr_norm<-instr_prices*instr_mult*instr_ratio*exchange_rate
        if(is.null(spreadlevel)) spreadlevel<-instr_norm else spreadlevel=spreadlevel+instr_norm
    }

    if(onelot) spreadlevel = spreadlevel/spread_instr$memberlist$memberratio[1]

    return(spreadlevel)
}


###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
