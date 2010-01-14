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

is.instrument <- function( x ) {
  x <- get(x,pos=.instrument) #removed inherits=TRUE
  inherits( x, "instrument" )
}

instrument<-function(primary_id , currency , multiplier , identifiers = NULL, ...,type=NULL ){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")

  # not sure this is correct, maybe should store the primary_id for the currency instead.  Why doesn't R have pointers?
  if(!is.currency(currency)) stop("currency must be an object of type 'currency'")

  if(!hasArg(identifiers)) identifiers = list()

  ## note that multiplier could be a time series, probably add code here to check
  if(!is.numeric(multiplier) | length(multiplier) > 1) stop("multiplier must be a single number")

  if(is.null(type)) tclass="instrument" else tclass = c(type,"instrument")

  ## now structure and return
  return(structure( list(primary_id = primary_id,
                         type = type,
                         currency = currency,
                         multiplier = multiplier,
                         identifiers = identifiers
                        ),
                    class = tclass
                  ) # end structure
          )
}

stock <- function(primary_id , currency , multiplier, identifiers = NULL, ...){
  stock_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ..., type="stock" )
  ## now structure and return
  assign(primary_id, structure( list(primary_id = stock_tmp$primary_id,
                         currency = stock_tmp$currency,
                         multiplier = stock_tmp$multiplier,
                         identifiers = stock_tmp$identifiers
                        ),
                    class=c("stock","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

future <- function(primary_id , currency , multiplier , identifiers = NULL, ..., underlying_id=NULL){
  future_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ... , type="future" )

  if(is.null(underlying_id)) {
      warning("underlying_id should only be NULL for cash-settled futures")
  } else {
      if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  }
  ## now structure and return
  assign(primary_id, structure( list(primary_id = future_temp$primary_id,
                         currency = future_temp$currency,
                         multiplier = future_temp$multiplier,
                         identifiers = future_temp$identifiers,
                         underlying_id = future_temp$underlying_id
                        ),
                    class=c("future","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

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
                         first_traded = first_traded,
                         expires = expires,
                         identifiers = identifiers
                        ),
                    class=c("future_series", "future", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id, sep="_"), temp_series, envir=as.environment(.instrument))
}

option <- function(primary_id , currency , multiplier , identifiers = NULL, ..., underlying_id=NULL){
  option_temp = instrument(primary_id , currency , multiplier, identifiers = identifiers, ..., type="option")

  if(is.null(underlying_id)) {
      warning("underlying_id should only be NULL for cash-settled options")
  } else {
      if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
  }
  ## now structure and return
  assign(primary_id, structure( list(primary_id = option_temp$primary_id,
                         currency = option_temp$currency,
                         multiplier = option_temp$multiplier,
                         identifiers = option_temp$identifiers,
                         underlying_id = option_temp$underlying_id
                        ),
                    class=c("option","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

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
                         expires = expires,
                         callput = callput,
                         identifiers = identifiers
                        ),
                    class=c("option_series", "option", "instrument")
             ) # end structure
  }

  assign(paste(primary_id, suffix_id,sep="_"), temp_series, envir=as.environment(.instrument))
}

currency <- function(primary_id , currency=NULL , multiplier=1 , identifiers = NULL, ...){
  ## now structure and return
  assign(primary_id, structure( list(primary_id = primary_id,
                         type = "currency",
                         currency = primary_id,
                         multiplier = 1,
                         identifiers = identifiers
                        ),
                    class=c("currency","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

is.currency <- function( x ) {
  x <- get(x,pos=.instrument) # REMOVED ,inherits=TRUE
  inherits( x, "currency" )
}

exchange_rate <- function (primary_id , currency , second_currency, identifiers = NULL, ...){
  exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , identifiers = identifiers, ..., type="exchange_rate")

  if(!exists(currency, where=.instrument,inherits=TRUE)) warning("currency not found") # assumes that we know where to look
  if(!exists(second_currency, where=.instrument,inherits=TRUE)) warning("second_currency not found") # assumes that we know where to look

  ## now structure and return
  assign(primary_id, structure( list(primary_id = exchange_rate_temp$primary_id,
                         currency = exchange_rate_temp$currency,
                         second_currency = exchange_rate_temp$second_currency,
                         identifiers = exchange_rate_temp$identifiers
                        ),
                    class=c("exchange_rate","instrument")
                  ), # end structure
         envir=as.environment(.instrument)
  )
}

#@TODO: government bond
#@TODO  auction dates, coupons, etc for govmt. bonds
bond <- function(primary_id , currency , multiplier, identifiers = NULL, ...){
    bond_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ..., type="bond" )
    ## now structure and return
    assign(primary_id, structure( list(primary_id = bond_temp$primary_id,
                            currency = bond_temp$currency,
                            multiplier = bond_temp$multiplier,
                            identifiers = bond_temp$identifiers
                    ),
                    class=c("bond","instrument")
            ), # end structure
            envir=as.environment(.instrument)
    )
}



getInstrument <- function(x){
  get(x,pos=.instrument) #removed inherits=TRUE
  #TODO add Date support to instrument, to get the proper value given a specific date
}