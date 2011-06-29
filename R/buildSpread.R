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
buildSpread <- function(spread_id, Dates = NULL, onelot=TRUE, prefer = NULL, auto.assign=TRUE, env=.GlobalEnv) #overwrite=FALSE
{
##TODO: test something with a different currency    
    spread_instr <- try(getInstrument(spread_id))
    if (inherits(spread_instr, "try-error") | !is.instrument(spread_instr)) {
        stop(paste("Instrument", spread_instr, " not found, please create it first."))
    }
    if (!inherits(spread_instr, "synthetic")) 
        stop(paste("Instrument", spread_id, " is not a synthetic instrument, please use the symbol of a synthetic."))
    #if (!inherits(try(get(spread_id),silent=TRUE), "try-error") && overwrite==FALSE) #Doesn't work..returns vector of FALSE
	#stop(paste(spread_instr,' price series already exists. Try again with overwrite=TRUE if you wish to replace it.')) 

    spread_currency <- spread_instr$currency
    stopifnot(is.currency(spread_currency))
    
    spread_mult <- as.numeric(spread_instr$multiplier)
    if (is.null(spread_mult) || spread_mult == 0) spread_mult <- 1
    spread_tick <- spread_instr$tick_size
  
    if (!is.null(Dates)) {
      times <- .parseISO8601(Dates)
      from <- times$first.time
      to <- times$last.time
    }
    
    spreadseries <- NULL
    for (i in 1:length(spread_instr$members)) {
        instr <- try(getInstrument(as.character(spread_instr$members[i])))
        if (inherits(instr, "try-error") | !is.instrument(instr)) {
            stop(paste("Instrument", instr, " not found, please create it first."))
        }
        else {
            instr_currency <- instr$currency
	        if (i == 1) {
        		primary_currency = instr_currency
            }
	        stopifnot(is.currency(instr_currency))
            if (!all.equal(primary_currency, instr_currency)) {
                instr_currency <- instr$currency
                stopifnot(is.currency(instr_currency))
                exchange_rate <- try(get(paste(instr_currency, primary_currency, sep = "")))
                if (inherits(exchange_rate, "try-error")) {
                    exchange_rate <- try(get(paste(primary_currency, instr_currency, sep = "")))
                    if (inherits(exchange_rate, "try-error")) {
                        stop(paste("Exchange Rate", paste(primary_currency, instr_currency, sep = ""), "not found."))
                    }
                    else {
                        exchange_rate <- 1/exchange_rate
                    }
                }
            }
            else {
                exchange_rate = 1
            }
            instr_mult <- as.numeric(instr$multiplier)
            instr_ratio <- spread_instr$memberratio[i]
            instr_prices <- try(get(as.character(spread_instr$members[i],envir=.GlobalEnv)),silent=TRUE)
	        # If we were able to find instr_prices in .GlobalEnv, check to make sure there is data between from and to.
	        #if we couldn't find it in .GlobalEnv or there's no data between from and to, getSymbols
	        if (inherits(instr_prices, "try-error") || (!is.null(Dates) && length(instr_prices[Dates]) == 0)) {
                if (is.null(Dates)) {
                    warning(paste(spread_instr$members[i],"not found in .GlobalEnv, and no Dates supplied. Trying getSymbols defaults.") )
                    instr_prices <- getSymbols(as.character(spread_instr$members[i]),auto.assign=FALSE)
                    from <- first(index(instr_prices))
                    to <- last(index(instr_prices))
                } else {
                    warning(paste('Requested data for', spread_instr$members[i], 'not found in .GlobalEnv. Trying getSymbols.'))
                    instr_prices <- getSymbols(as.character(spread_instr$members[i]), from = from, to = to, auto.assign=FALSE)
                }
            }
	        if (is.null(Dates)) {
	            from <- first(index(instr_prices))
	            to <- last(index(instr_prices))
	        }
	        instr_prices <- instr_prices[paste(from,to,sep="::")]
            ##TODO: if length(prefer > 1), use the first value that exists in colnames(instr_prices)
            ##	i.e. treat prefer as an ordered vector of preferences.
	        if (is.null(prefer)) { 
	          if (is.HLC(instr_prices)) { 
		        pref='Close'
	          } else
	          if (has.Mid(instr_prices)) {
		        pref='Mid'
	          } else
	          if (has.Trade(instr_prices)) {
		        pref='Trade'
	          } else
	          if (has.Price(instr_prices)) {
		        pref='Price'
	          } else pref=colnames(instr_prices)[1]
	        } else pref=prefer
	        if (ncol(instr_prices > 1)) instr_prices <- getPrice(instr_prices,prefer=pref)
        }
        instr_norm <- instr_prices * instr_mult * instr_ratio * exchange_rate
        colnames(instr_norm) <- paste(as.character(spread_instr$members[i]), 
            prefer, sep = ".")
        if (is.null(spreadseries)) 
            spreadseries <- instr_norm
        else spreadseries = merge(spreadseries, instr_norm)
    }
    spreadseries <- na.locf(spreadseries,na.rm=TRUE)
    spreadlevel = xts(rowSums(spreadseries),order.by=index(spreadseries)) #assumes negative memberratio values for shorts in 'memberratio'
    if (onelot) 
        spreadlevel = spreadlevel/abs(spread_instr$memberratio[1]) #abs() takes care of things like a crack spread which is -3:2:1.
    colnames(spreadlevel) <- paste(spread_id,pref,sep='.')
    #Divide by multiplier and round according to tick_size of spread_instr
    if (is.null(spread_tick) || spread_tick == 0) ret <- spreadlevel/spread_mult
    else ret <- round((spreadlevel / spread_mult) / spread_tick, spread_tick) * spread_tick
    if (auto.assign) {
        assign(spread_id, ret, pos=env)
        ret <- spread_id
    } else 
    ret
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
fn_SpreadBuilder <- function(prod1, prod2, ratio=1, from=NULL, to=NULL, session_times=NULL, 
    unique_method=c('make.index.unique','duplicated','least.liq','price.change'), ...)
{
##TODO: don't require from and to to be passed in...use getSymbol defaults.
##TODO: allow for different methods for calculating Bid and Ask 
##TODO: Currently we're expecting ratio to be a univariate vector
    #print(paste(date," ",prod1,".",prod2,sep=""))
    
    unique_method<-unique_method[1]
    
    prod1.instr <- try(getInstrument(prod1))
    prod2.instr <- try(getInstrument(prod2))

    if (inherits(prod1.instr,'try-error') || 
        inherits(prod2.instr,'try-error') ||
        !is.instrument(prod1.instr) ||
        !is.instrument(prod2.instr) ) stop("both products must be defined as instruments first.")

    Data.1 <- NULL
    Data.2 <- NULL
    
    Data.1 <- try(get(as.character(prod1),envir=.GlobalEnv),silent=TRUE) 
    Data.2 <- try(get(as.character(prod2),envir=.GlobalEnv),silent=TRUE)
    if (inherits(Data.1, "try-error")) Data.1 <- getSymbols(prod1,auto.assign=FALSE,...) #the dots are for from and to    
    if (inherits(Data.2, "try-error")) Data.2 <- getSymbols(prod2,auto.assign=FALSE,...)
    
    if ( (is.OHLC(Data.1) && !is.OHLC(Data.2)) || 
	(is.BBO(Data.1) && !is.BBO(Data.2)) ||
	(!is.OHLC(Data.1) && is.OHLC(Data.2)) ||
	(!is.BBO(Data.1) && is.BBO(Data.2)) ) stop('prod1 and prod2 must be the same types of data (BBO,OHLC,etc.)')
    
    if (is.null(from)) from <- max(index(first(Data.1)),index(first(Data.2)))
    if (is.null(to)) to <- min(index(last(Data.1)),index(last(Data.2))) 
    Data.1 <- Data.1[paste(from,to,sep="::")]
    Data.2 <- Data.2[paste(from,to,sep="::")]
    
    Mult.1 <- as.numeric(prod1.instr$multiplier) 
    Mult.2 <- as.numeric(prod2.instr$multiplier) 
    
    #TODO FIXME we probably need to setSymbolLookup to oanda, and look up the cross rate.
    #if src is already set, don't reset it
    if (prod1.instr$currency != 'USD'){
        Cur.1 <- get(prod1.instr$currency)
        if (!is.null(to)) {
            Cur.1 <- as.numeric(last(Cur.1[to]))
        } else Cur.1 <- as.numeric(last(Cur.1))
    } else { Cur.1 <- 1 }
    
    if (prod2.instr$currency != 'USD'){
        Cur.2 <- get(prod2.instr$currency)
        if (!is.null(to)) {
            Cur.2 <- as.numeric(last(Cur.2[to]))
        } else Cur.2 <- as.numeric(last(Cur.2)) 
   } else { Cur.2 <- 1 }

    #Determine what type of data it is
    if (is.OHLC(Data.1) && has.Ad(Data.1)) {
      	M <- merge(Op(Data.1)[,1],Cl(Data.1)[,1],Ad(Data.1)[,1],Op(Data.2)[,1],Cl(Data.2)[,1],Ad(Data.2)[,1])
	colnames(M) <- c("Open.Price.1","Close.Price.1","Adjusted.Price.1","Open.Price.2","Close.Price.2","Adjusted.Price.2")
    } else if(is.OHLC(Data.1)) {
	M <- merge(Op(Data.1)[,1],Cl(Data.1)[,1],Op(Data.2)[,1],Cl(Data.2)[,1])
	colnames(M) <- c("Open.Price.1","Close.Price.1","Open.Price.2","Close.Price.2")
    } else if (is.BBO(Data.1)) {
	M <- merge(Data.1[,c( grep('Bid',colnames(Data.1),ignore.case=TRUE)[1], 
			grep('Ask',colnames(Data.1),ignore.case=TRUE)[1])],
		Data.2[,c(grep('Bid',colnames(Data.1),ignore.case=TRUE)[1],
			grep('Ask',colnames(Data.2),ignore.case=TRUE)[1])] )
      colnames(M) <- c("Bid.Price.1","Ask.Price.1","Bid.Price.2","Ask.Price.2")
    } else M <- merge(Data.1,Data.2)

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
	colnames(ret) <- colnames(DF)
        ret
    }
    
    M <- fn_split(M)
	
    #can't subset times until after the merge
    if(!is.null(session_times)){
        #Data.1 <- Data.1[time.sub.GMT]
        #Data.2 <- Data.2[time.sub.GMT]
        M <- M[session_times]
    }
    
    if( is.OHLC(Data.1) ) {
      M$Open.Price.1 <- M$Open.Price.1 * Mult.1 * Cur.1 
      M$Close.Price.1 <- M$Close.Price.1 * Mult.1 * Cur.1
      M$Open.Price.2 <- M$Open.Price.2 * Mult.2 * Cur.2
      M$Close.Price.2 <- M$Close.Price.2 * Mult.2 * Cur.2
      
      open <- M$Open.Price.1 - M$Open.Price.2
      close <- M$Close.Price.1 - M$Close.Price.2

      Spread <- cbind(open,close)
      colnames(Spread) <- c('Open.Price','Close.Price')
      if (has.Ad(Data.1)) {
	M$Adjusted.Price.1 <- M$Adjusted.Price.1 * Mult.1 * Cur.1
	M$Adjusted.Price.2 <- M$Adjusted.Price.2 * Mult.2 * Cur.2
	Spread$Adjusted.Price <- M$Adjusted.Price.1 - M$Adjusted.Price.2
      }
      #Spread$Mid.Price <- (Spread$Open.Price + Spread$Close.Price) / 2
    } else
    if (is.BBO(Data.1) ) {
      M$Bid.Price.1 <- M$Bid.Price.1 * Mult.1 * Cur.1 
      M$Ask.Price.1 <- M$Ask.Price.1 * Mult.1 * Cur.1
      M$Bid.Price.2 <- M$Bid.Price.2 * Mult.2 * Cur.2
      M$Ask.Price.2 <- M$Ask.Price.2 * Mult.2 * Cur.2
      ##TODO: Expand this to work with multiple legs
      bid <- M$Bid.Price.1 - ratio * M$Ask.Price.2
      ask <- M$Ask.Price.1 - ratio * M$Bid.Price.2
      
      Spread <- cbind(bid,ask)
      names(Spread) <- c("Bid.Price","Ask.Price")
      Spread$Mid.Price <- (Spread$Bid.Price + Spread$Ask.Price) / 2
    } else {
    #univariate spread.  Call buildSpread2?
      if (ncol(M) > 2) stop('Unrecognized column names.')
      Spread <- M[,1] - ratio * M[,2]
      colnames(Spread) <- 'Price'
    }
##TODO: Test with symbols where each symbol has data on a day that the other one doesn't 
##TODO: Add a method that merges Data.1 and Data.2 with all=FALSE and use that index to subset
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
##TODO: look to see if there is an instrument defined for this spread (using whatever is the spread naming convention).
## If it is not defined, then define it, adding fn_SpreadBuilder to the type, or indicating in some other way that it was
## auto-defined.

    Spread  
}

formatSpreadPrice <- function(x,multiplier=1,tick_size=0.01) {
  x <- x / multiplier
  round( x / tick_size) * tick_size
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
