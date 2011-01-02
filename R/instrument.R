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

.onLoad <- function(lib, pkg) {
    if(!exists('.instrument'))
        .instrument <<- new.env(hash=TRUE)
}

#' class test for object supposedly of type 'instrument'
#' @param x object or primary_id of instrument to test for type
#' @export
is.instrument <- function( x ) {
  inherits( x, "instrument" )
}

#' instrument class constructors
#' 
#' All 'currency' instruments must be defined before instruments of other types may be defined.
#' 
#' In \dots you may pass any other arbitrary instrument fields that will be used to create 'custom' fields.  
#' S3 classes in \R are basically lists with a class attribute.
#' We use this to our advantage to allow us to set arbitrary fields.
#' 
#' \code{identifiers} should be a named list to specify other identifiers beyond the \code{primary_id}.
#' Please note that whenever possible, these should still be unique.  Perhaps Bloomberg, Reuters-X.RIC, CUSIP, etc.  
#' The code will return the first (and only the first) match that it finds, starting with the primary_id, and then searching all instruments in the list alphabetically by primary_id.  
#' This is robust enough if you take some care, though a more robust patch would be welcomed.
#' 
#' The \code{primary_id} will be coerced within reason to a valid \R variable name by 
#' using \code{\link{make.names}}. We also remove any leading '1' digit (a simple workaround to account for issues with the Reuters API).  
#' Please use some care to choose your primary identifiers so that R won't complain.
#' If you have better regular expression code, we'd be happy to include it.   
#' 
#' Identifiers will also try to be discovered as regular named arguments passed in via \code{...}.  
#' We currently match any of the following: \code{"CUSIP","SEDOL","ISIN","OSI","Bloomberg","Reuters","X.RIC","CQG","TT","Yahoo","Google"}
#' Others mat be specified using a named list of identifiers, as described above.
#' 
#' \code{assign_i} will use \code{\link{assign}} to place the constructed 
#' instrument class object into the \code{.instrument} environment.  Most of the 
#' special type-specific constructors will use \code{assign_i=TRUE} internally. 
#' Calling with \code{assign_i=FALSE}, or not specifying it, will return an object and
#' will \emph{not} store it.  Use this option ether to wrap calls to \code{instrument}
#' prior to further processing (and presumably assignment) or to test your parameters
#' before assignment.
#' 
#' @param primary_id string describing the unique ID for the instrument
#' @param ... any other passthru parameters 
#' @param currency string describing the currency ID of an object of type \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument currency to get to notional value
#' @param tick_size the tick increment of the instrument price in it's trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param type instrument type to be appended to the class definition, typically not set by user
#' @param underlying_id for derivatives, the identifier of the instrument that this one is derived from, may be NULL for cash settled instruments
#' @param assign_i TRUE/FALSE if TRUE, assign the instrument to the .instrument environment, default FALSE
#' @aliases 
#' stock
#' bond
#' future
#' option
#' currency
#' instrument
#' fund
#' @seealso 
#' \code{\link{exchange_rate}}
#' \code{\link{option_series}}
#' \code{\link{future_series}}
#' \code{\link{load.instruments}}
#' @export
instrument<-function(primary_id , ..., currency , multiplier , tick_size=NULL, identifiers = NULL, type=NULL, assign_i=FALSE ){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")
  
  #deal with leading digits or illegal characters
  if(substr(primary_id,1,1)==1) primary_id <- substr(primary_id,2,nchar(primary_id))
  primary_id<-make.names(primary_id)
  
  if(!is.currency(currency)) stop("currency ",currency," must be an object of type 'currency'")

  if(!hasArg(identifiers) || is.null(identifiers)) identifiers = list()
  if(!is.list(identifiers)) {
      warning("identifiers",identifiers,"do not appear to be a named list")
  } 

  arg<-list(...)
  if(is.list(arg[['...']])){
      if(length(arg)==1) arg <- arg[['...']]
      else {
          targ<-arg[['...']]
          arg[['...']]<-NULL
          arg<-c(arg,targ)
      }
  } 
  #check for identifiers we recognize 
  ident_str<-c("X.RIC","RIC","CUSIP","SEDOL","OSI","Bloomberg","Reuters","ISIN","CQG","TT","Yahoo","Google")
  for(i_s in ident_str ){
      if(any(grepl(i_s,names(arg),ignore.case=TRUE))) {
          pos<-first(grep(i_s,names(arg),ignore.case=TRUE))
          identifiers<-c(identifiers,arg[[pos]])
          names(identifiers)[length(identifiers)]<-names(arg)[pos]
          arg[[pos]]<-NULL
      }
  }
  
  
  ## TODO note that multiplier could be a time series, probably add code here to check
  if(!is.numeric(multiplier) | length(multiplier) > 1) stop("multiplier must be a single number")
  if(!is.numeric(tick_size) | length(tick_size) > 1) stop("tick_size must be a single number")
  
  if(is.null(type)) tclass="instrument" else tclass = c(type,"instrument")

  tmpinstr <- list(primary_id = primary_id,
                   currency = currency,
                   multiplier = multiplier,
				   tick_size=tick_size,
                   identifiers = identifiers,
                   type = type
                   )
  if(length(arg)>=1) {
      tmpinstr <- c(tmpinstr,arg)   
  }
  class(tmpinstr)<-tclass
  
  if(assign_i)  assign(primary_id, tmpinstr, envir=as.environment(.instrument) )
  else return(tmpinstr) 
}

#' @export
stock <- function(primary_id , currency=NULL , multiplier=1 , tick_size=.01, identifiers = NULL, ...){
	stock_temp=  instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="stock", assign_i=TRUE)
}

#' @export
fund <- function(primary_id , currency=NULL , multiplier=1 , tick_size=.01, identifiers = NULL, ...){
    fund_temp =  instrument(primary_id = primary_id, currency = currency, multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, ..., type="fund", assign_i=TRUE)
}

#' @export
future <- function(primary_id , currency , multiplier , tick_size=NULL, identifiers = NULL, ..., underlying_id=NULL){
    if(is.null(underlying_id)) {
        warning("underlying_id should only be NULL for cash-settled futures")
    } else {
        if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
    }

    future_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ... , type="future", underlying_id=underlying_id, assign_i=TRUE )
}

#' constructors for series contracts on instruments such as options and futures
#' 
#' In custom parameters for these series contracts, we have often found it
#' useful to store attributes such as local roll-on and roll-off dates
#' (rolling not on the \code{first_listed} or \code{expires}
#' @param primary_id string describing the unique ID for the instrument
#' @param suffix_id string suffix that should be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year
#' @param first_traded string coercible to Date for first trading day
#' @param expires string coercible to Date for expiration date
#' @param maturity string coercible to Date for maturity date of bond series
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @aliases 
#' option_series
#' future_series
#' bond_series
#' @export
future_series <- function(primary_id , suffix_id, first_traded=NULL, expires=NULL, identifiers = NULL, ...){
  contract<-try(getInstrument(primary_id))
  if(!inherits(contract,"future")) stop("futures contract spec must be defined first")

  # TODO add check for Date equivalent in first_traded and expires

  ## with futures series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years or months)
  ## and then add the first_traded and expires to the time series bu splicing
  id<-paste(primary_id, suffix_id,sep="_")
  temp_series<-try(getInstrument(id),silent=TRUE)
  if(inherits(temp_series,"future_series")) {
      message("updating existing first_traded and expires for ",id)
      temp_series$first_traded<-c(temp_series$first_traded,first_traded)
      temp_series$expires<-c(temp_series$expires,expires)
      assign(id, temp_series, envir=as.environment(.instrument))
  } else {
      dargs<-list(...)
      dargs$currency=NULL
      dargs$multiplier=NULL
      dargs$type=NULL
      temp_series = instrument( primary_id = id,
                                 suffix_id=suffix_id,
                                 currency = contract$currency,
                                 multiplier = contract$multiplier,
        						 tick_size=contract$tick_size,
        						 first_traded = first_traded,
                                 expires = expires,
                                 identifiers = identifiers,
                                 type=c("future_series", "future"),
                                 ...=dargs,
                                 assign_i=TRUE
                              ) 
  }
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
  option_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ... , type="option", underlying_id=underlying_id, assign_i=TRUE )
}

#' @export
option_series <- function(primary_id , suffix_id, first_traded=NULL, expires=NULL, callput=c("call","put"), identifiers = NULL, ...){
    contract<-try(getInstrument(primary_id))
    if(!inherits(contract,"option")) stop("options contract spec must be defined first")
    ## with options series we probably need to be more sophisticated,
    ## and find the existing series from prior periods (probably years)
    ## and then add the first_traded and expires to the time series
    if(length(callput)==2) stop("value of callput must be specified as 'call' or 'put'")
    id<-paste(primary_id, suffix_id,sep="_")
    temp_series<-try(getInstrument(id),silent=TRUE)
    if(inherits(temp_series,"option_series")) {
        message("updating existing first_traded and expires for ", id)
        temp_series$first_traded<-c(temp_series$first_traded,first_traded)
        temp_series$expires<-c(temp_series$expires,expires)
        assign(id, temp_series, envir=as.environment(.instrument))
    } else {
        temp_series = instrument( primary_id = id,
                                    suffix_id = suffix_id,
                                    currency = contract$currency,
                                    multiplier = contract$multiplier,
                                    tick_size=contract$tick_size,
                                    first_traded = first_traded,
                                    expires = expires,
                                    identifiers = identifiers,
                                    ...,
                                    type=c("option_series", "option"),
                                    assign_i=TRUE
                                ) 
    }
}

#' @export
currency <- function(primary_id , currency=NULL , multiplier=1 , identifiers = NULL, ...){
  ## now structure and return
  currency_temp <- list(primary_id = primary_id,
          currency = primary_id,
          multiplier = 1,
          tick_size= .01,
          identifiers = identifiers
  )
  currency_temp <- c(currency_temp,list(...))   
  
  class(currency_temp)<-c("currency","instrument")
  assign(primary_id, currency_temp, envir=as.environment(.instrument) )
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
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @export
exchange_rate <- function (primary_id , currency , second_currency, identifiers = NULL, ...){
  # exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , tick_size=.01, identifiers = identifiers, ..., type="exchange_rate")

  if(!exists(currency, where=.instrument,inherits=TRUE)) warning("currency not found") # assumes that we know where to look
  if(!exists(second_currency, where=.instrument,inherits=TRUE)) warning("second_currency not found") # assumes that we know where to look

  ## now structure and return
  exrate_temp=  instrument(primary_id=primary_id , currency=currency , multiplier=1 , tick_size=.01, identifiers = identifiers, ..., secon_currency=second_currency, type=c("exchange_rate","currency"), assign_i=TRUE)
}

#TODO  auction dates, coupons, etc for govmt. bonds
#' @export
bond <- function(primary_id , currency , multiplier, tick_size=NULL , identifiers = NULL, ...){
    bond_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="bond", assign_i=TRUE )
}

#' @export
bond_series <- function(primary_id , suffix_id, ..., first_traded=NULL, maturity=NULL, identifiers = NULL, payment_schedule=NULL){
    contract<-try(getInstrument(primary_id))
    if(!inherits(contract,"bond")) stop("bonds contract spec must be defined first")
    
    # TODO add check for Date equivalent in first_traded and expires
    
    ## with bond series we probably need to be more sophisticated,
    ## and find the existing series from prior periods (probably years or months)
    ## and then add the first_traded and expires to the time series by splicing
    id<-paste(primary_id, suffix_id,sep="_")
    temp_series<-try(getInstrument(id),silent=TRUE)
    if(inherits(temp_series,"bond_series")) {
        message("updating existing first_traded and maturity for ",id)
        temp_series$first_traded<-c(temp_series$first_traded,first_traded)
        temp_series$expires<-c(temp_series$expires,expires)
        assign(id, temp_series, envir=as.environment(.instrument))
    } else {
        dargs<-list(...)
        dargs$currency=NULL
        dargs$multiplier=NULL
        dargs$type=NULL
        temp_series = instrument( primary_id = id,
                suffix_id=suffix_id,
                currency = contract$currency,
                multiplier = contract$multiplier,
                tick_size=contract$tick_size,
                first_traded = first_traded,
                maturity = maturity,
                identifiers = identifiers,
                type=c("bond_series", "bond"),
                ...=dargs,
                assign_i=TRUE
        ) 
    }
}
    
#' primary accessor function for getting objects of type 'instrument'
#' @param x string identifier of instrument to retrieve
#' @param Dates date range to retrieve 'as of', may not currently be implemented
#' @export
getInstrument <- function(x, Dates=NULL, silent=FALSE){
    tmp_instr<-try(get(x,pos=.instrument),silent=TRUE) #removed inherits=TRUE
    if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
        #first search
        instr_list<-ls(pos=.instrument)
        for (instr in instr_list){
            tmp_instr<-try(get(instr,pos=.instrument),silent=TRUE)
            if(is.instrument(tmp_instr) && length(grep(x,tmp_instr$identifiers))) {
                return(tmp_instr)
            }
        }
        if(!silent) warning(paste("Instrument",x," not found, please create it first."))
        return(FALSE)
    } else{
        return(tmp_instr)
    }
    #TODO add Date support to instrument, to get the proper value given a specific date
}
