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

.onLoad <- function(lib, pkg) {
    if(!exists('.instrument'))
        .instrument <<- new.env()
}

## we should probably assign instruments into a special namespace and create get* functions.  Jeff?

#' class test for object supposedly of type 'instrument'
#' @param x object to test for type
#' @export
is.instrument <- function( x ) {
  inherits( x, "instrument" )
}

#' instrument class constructors
#' 
#' All 'currency' instruments must be defined before instruments of other types may be defined
#' 
#' @param primary_id string describing the unique ID for the instrument
#' @param ... any other passthru parameters 
#' @param currency string describing the currency ID of an object of type \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument currency to get to notional value
#' @param tick_size the tick increment of the instrument price in it's trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers character vector of any other identifiers that should also be stored for this instrument
#' @param type instrument type to be appended to the class definition
#' @param underlying_id for derivatives, the identifier of the instrument that this one is derived from, may be NULL for cash settled instruments
#' @aliases 
#' stock
#' bond
#' future
#' option
#' currency
#' instrument
#' @seealso 
#' \code{\link{exchange_rate}}
#' \code{\link{option_series}}
#' \code{\link{future_series}}
#' @export
instrument<-function(primary_id , ..., currency , multiplier , tick_size=NULL, identifiers = NULL, type=NULL ){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")

  # not sure this is correct, maybe should store the primary_id for the currency instead.  Why doesn't R have pointers?
  if(!is.currency(currency)) stop("currency must be an object of type 'currency'")

  if(!hasArg(identifiers)) identifiers = list()

  ## note that multiplier could be a time series, probably add code here to check
  if(!is.numeric(multiplier) | length(multiplier) > 1) stop("multiplier must be a single number")
  if(!is.numeric(tick_size) | length(tick_size) > 1) stop("tick_size must be a single number")
  
  if(is.null(type)) tclass="instrument" else tclass = c(type,"instrument")

  ## now structure and return
  return(structure( list(primary_id = primary_id,
                         type = type,
                         currency = currency,
                         multiplier = multiplier,
						 tick_size=tick_size,
                         identifiers = identifiers
                        ),
                    class = tclass
                  ) # end structure
          )
}

#' @export
stock <- function(primary_id , currency=NULL , multiplier=1 , tick_size=.01, identifiers = NULL, ...){
	stock_temp=  instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="stock")
	assign(primary_id, stock_temp, envir=as.environment(.instrument) )
}

#' @export
future <- function(primary_id , currency , multiplier , tick_size=NULL, identifiers = NULL, ..., underlying_id=NULL){
  future_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ... , type="future" )

  if(is.null(underlying_id)) {
      warning("underlying_id should only be NULL for cash-settled futures")
  } else {
      if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  }
  ## now structure and return
  assign(primary_id, structure( list(primary_id = future_temp$primary_id,
                         currency = future_temp$currency,
                         multiplier = future_temp$multiplier,
						 tick_size=future_temp$tick_size,
						 identifiers = future_temp$identifiers,
                         underlying_id = future_temp$underlying_id
                        ),
                    class=c("future","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

#' constructors for series contracts on instruments such as options and futures
#' @param primary_id string describing the unique ID for the instrument
#' @param suffix_id string suffix that should be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year
#' @param first_traded string coercible to Date for first trading day
#' @param expires string coercible to Date for expiration date
#' @param identifiers character vector of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @aliases 
#' option_series
#' future_series
#' @export
future_series <- function(primary_id , suffix_id, first_traded=NULL, expires=NULL, identifiers = NULL, ...){
  contract<-try(getInstrument(primary_id))
  if(!inherits(contract,"future")) stop("futures contract spec must be defined first")

  # TODO add check for Date equivalent in first_traded and expires

  ## with futures series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years)
  ## and then add the first_traded and expires to the time series
  temp_series<-try(getInstrument(paste(primary_id, suffix_id,sep="_")),silent=TRUE)
  if(inherits(temp_series,"future_series")) {
      message("updating existing first_traded and expires")
      temp_series$first_traded<-c(temp_series$first_traded,first_traded)
    temp_series$expires<-c(temp_series$expires,expires)
  } else {
    temp_series = structure( list(primary_id = contract$primary_id,
                         suffix_id = suffix_id,
                         currency = contract$currency,
                         multiplier = contract$multiplier,
						 tick_size=contract$tick_size,
						 first_traded = first_traded,
                         expires = expires,
                         identifiers = identifiers
                        ),
                    class=c("future_series", "future", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id, sep="_"), temp_series, envir=as.environment(.instrument))
}

#' @export
option <- function(primary_id , currency , multiplier , tick_size=NULL, identifiers = NULL, ..., underlying_id=NULL){
  option_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="option")

  if(is.null(underlying_id)) {
      warning("underlying_id should only be NULL for cash-settled options")
  } else {
      if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  }
  ## now structure and return
  assign(primary_id, structure( list(primary_id = option_temp$primary_id,
                         currency = option_temp$currency,
                         multiplier = option_temp$multiplier,
						 tick_size = option_temp$tick_size,
                         identifiers = option_temp$identifiers,
                         underlying_id = option_temp$underlying_id
                        ),
                    class=c("option","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

#' @export
option_series <- function(primary_id , suffix_id, first_traded=NULL, expires=NULL, callput=c("call","put"), identifiers = NULL, ...){
  contract<-try(getInstrument(primary_id))
  if(!inherits(contract,"option")) stop("options contract spec must be defined first")
  ## with options series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years)
  ## and then add the first_traded and expires to the time series
  if(length(callput)==2) stop("value of callput must be specified as 'call' or 'put'")
  temp_series<-try(getInstrument(paste(primary_id, suffix_id,sep="_")),silent=TRUE)
  if(inherits(temp_series,"option_series")) {
    message("updating existing first_traded and expires")
    temp_series$first_traded<-c(temp_series$first_traded,first_traded)
    temp_series$expires<-c(temp_series$expires,expires)
  } else {
    temp_series = structure( list(primary_id = contract$primary_id,
                         suffix_id = suffix_id,
                         first_traded = first_traded,
                         currency = contract$currency,
                         multiplier = contract$multiplier,
						 tick_size=contract$tick_size,
                         expires = expires,
                         callput = callput,
                         identifiers = identifiers
                        ),
                    class=c("option_series", "option", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id,sep="_"), temp_series, envir=as.environment(.instrument))
}

#' @export
currency <- function(primary_id , currency=NULL , multiplier=1 , identifiers = NULL, ...){
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "currency",
                         currency = primary_id,
                         multiplier = 1,
                         tick_size=.01,
                         identifiers = identifiers
                        ),
                    class=c("currency","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

#' class test for object supposedly of type 'currency'
#' @param x object to test for type
#' @export
is.currency <- function( x ) {
  x<-getInstrument(x)
  inherits( x, "currency" )
}

#' constructor for spot exchange rate instruments
#' @param primary_id string identifier, usually expressed as a currency pair 'USDYEN' or 'EURGBP'
#' @param currency string identifying front currency
#' @param second_currency string identifying second currency
#' @param identifiers character vector of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @export
exchange_rate <- function (primary_id , currency , second_currency, identifiers = NULL, ...){
  # exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , tick_size=.01, identifiers = identifiers, ..., type="exchange_rate")

  if(!exists(currency, where=.instrument,inherits=TRUE)) warning("currency not found") # assumes that we know where to look
  if(!exists(second_currency, where=.instrument,inherits=TRUE)) warning("second_currency not found") # assumes that we know where to look

  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         currency = currency,
                         multiplier=1,
                         tick_size=.01,
                         second_currency = second_currency,
                         identifiers = identifiers
                        ),
                    class=c("exchange_rate","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

#@TODO: government bond
#@TODO  auction dates, coupons, etc for govmt. bonds
bond <- function(primary_id , currency , multiplier, tick_size=NULL , identifiers = NULL, ...){
    bond_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="bond" )
    assign( primary_id, bond_temp, envir=as.environment(.instrument) )
}


#' primare accessor function for getting objects of type 'instrument'
#' @param x string identifier of instrument to retrieve
#' @param Dates date range to retrieve 'as of', may not currently be implemented
#' @export
getInstrument <- function(x, Dates=NULL){
    tmp_instr<-get(x,pos=.instrument) #removed inherits=TRUE
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
        warning(paste("Instrument",x," not found, please create it first."))
        return(FALSE)
    } else{
        return(tmp_instr)
    }
    #TODO add Date support to instrument, to get the proper value given a specific date
}
