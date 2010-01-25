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
#    primary_instr<-getInstrument(spread_instr$memberlist$members[1])
#    if(inherits(primary_instr,"try-error") | !is.instrument(primary_instr)){
#        stop(paste("Instrument",primary_instr," not found, please create it first."))
#    } else {
#        primary_currency<-primary_instr$currency
#        stopifnot(is.currency(primary_currency))
#        primary_mult<-primary_instr$multiplier
#        primary_ratio<-spread_instr$memberlist$memberratios[1]
#        primary_prices<-Cl(get(spread_instr$memberlist$members[1]))
#    }
#    secondary_instr<-getInstrument(spread_instr$memberlist[1])
#    if(inherits(secondary_instr,"try-error") | !is.instrument(secondary_instr)){
#        stop(paste("Instrument", secondary_instr, " not found, please create it first."))
#    } else {
#        if(!all.equal(primary_currency,secondary_currency)){
#            secondary_currency<-secondary_instr$currency
#            stopifnot(is.currency(secondary_currency))
#            exchange_rate<-try(get( paste(primary_currency,secondary_currency,sep='')))
#            if(inherits(exchange_rate,"try-error")){
#                exchange_rate<-try(get( paste(secondary_currency,primary_currency,sep='')))
#                if(inherits(exchange_rate,"try-error")){
#                    stop(paste("Exchange Rate", paste(primary_currency, secondary_currency, sep=''), "not found."))    
#                } else {
#                    exchange_rate <- 1/exchange_rate
#                }   
#            }
#        } else {
#            #currencies of both instruments are the same
#            exchange_rate=1
#        }
#        secondary_mult<-secondary_instr$multiplier
#        secondary_ratio<-spread_instr$memberlist$memberratios[2]
#        secondary_prices<-get(spread_instr$memberlist$members[2])
#    }
#    
#    spreadlevel<- (primary_prices*primary_mult*primary_ratio)-(secondary_prices*secondary_mult*secondary_ratio*exchange_rate)
    if(onelot) spreadlevel = spreadlevel/spread_instr$memberlist$memberratios[1]
    if(!all.equal(spread_currency,primary_currency)){
        #convert to the currency of the spread
        spr_exch_rate <- try(get(paste(spread_currency,primary_currency,sep='')))
        if(inherits(spr_exch_rate,"try-error")){
            stop(paste("Required Exchange Rate", paste(spread_currency, primary_currency,sep=''),"not found."))    
        } else {
            spreadlevel<-spreadlevel*exchange_rate
        }
    }
    return(spreadlevel)
}


###############################################################################
# Blotter: Tools for transaction-oriented trading systems development
# for R (see http://r-project.org/) 
# Copyright (c) 2008-2010 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
