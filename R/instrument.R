###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, 
# Joshua Ulrich, Brian G. Peterson, and Garrett See
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
#' \code{future} and \code{option} are used to define the contract specs of a series of instruments.  The
#' \code{primary_id} for these can begin with 1 or 2 dots if you need to avoid overwriting another instrument.
#' For example, if you have a \code{stock} with \sQuote{SPY} as the \code{primary_id}, you could use 
#' \sQuote{.SPY} as the \code{primary_id} of the \code{option} specs, and \sQuote{..SPY} as the 
#' \code{primary_id} of the single stock \code{future} specs. (or vice versa)
#'
#' You can (optionally) provide a \code{src} argument in which case, it will be used in a call to setSymbolLookup.
#' @param primary_id string describing the unique ID for the instrument. Most of the wrappers allow this to be a vector.
#' @param ... any other passthru parameters, including 
#' @param underlying_id for derivatives, the identifier of the instrument that this one is derived from, may be NULL for cash settled instruments
#' @param currency string describing the currency ID of an object of type \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument currency to get to notional value
#' @param tick_size the tick increment of the instrument price in it's trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param type instrument type to be appended to the class definition, typically not set by user
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
#' \code{\link{spread}}
#' \code{\link{load.instruments}}
#' @export
instrument<-function(primary_id , ..., currency , multiplier , tick_size=NULL, identifiers = NULL, type=NULL, assign_i=FALSE ){
  if(is.null(primary_id)) stop("you must specify a primary_id for the instrument")
  
  #deal with leading digits or illegal characters
  if(substr(primary_id,1,1)==1) primary_id <- substr(primary_id,2,nchar(primary_id))
  primary_id<-make.names(primary_id)
  
  if(missing(currency) || (!missing(currency) && !is.currency(currency)))
    stop("currency ",currency," must be an object of type 'currency'")

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
  if (!is.null(arg$src)) {
      sarg <- list()
      sarg[[primary_id]] <- arg$src
      setSymbolLookup(sarg)
      #arg[["src"]]<-NULL
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
  if(!is.null(tick_size) && (!is.numeric(tick_size) | length(tick_size) > 1)) stop("tick_size must be NULL or a single number")
  
  if(is.null(type)) tclass="instrument" else tclass = unique(c(type,"instrument"))

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
  
  if(assign_i)  {
      assign(primary_id, tmpinstr, envir=as.environment(.instrument) )
      return(primary_id)  
  } else return(tmpinstr) 
}

#' @export
#' @rdname instrument
stock <- function(primary_id , currency=NULL , multiplier=1 , tick_size=.01, identifiers = NULL, ...){
    if (is.null(currency)) stop ("'currency' is a required argument")
    if (length(primary_id) > 1) return(unname(sapply(primary_id, stock, 
        currency=currency, multiplier=multiplier, tick_size=tick_size, identifiers=identifiers, ...=...)))
    instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="stock", assign_i=TRUE)
}

#' @export
#' @rdname instrument
fund <- function(primary_id , currency=NULL , multiplier=1 , tick_size=.01, identifiers = NULL, ...){
    if (is.null(currency)) stop ("'currency' is a required argument")
    if (length(primary_id) > 1) return(unname(sapply(primary_id, fund,
        currency=currency, multiplier=multiplier, tick_size=tick_size, identifiers=identifiers, ...=...)))
    instrument(primary_id = primary_id, currency = currency, multiplier = multiplier, tick_size = tick_size, identifiers = identifiers, ..., type="fund", assign_i=TRUE)
}

#' @export
#' @rdname instrument
future <- function(primary_id , currency , multiplier , tick_size=NULL, identifiers = NULL, ..., underlying_id=NULL){
    if(missing(primary_id)) primary_id <- paste("..",underlying_id,sep="")
    if (length(primary_id) > 1) stop('primary_id must be of length 1')
    if (missing(currency) && !is.null(underlying_id)) {
        uinstr <- getInstrument(underlying_id,silent=TRUE)
        if (is.instrument(uinstr)) {
            currency <- uinstr$currency
        } else stop("'currency' is a required argument")
    }
    if(is.null(underlying_id)) {
        warning("underlying_id should only be NULL for cash-settled futures")
    } else {
        if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
        if (primary_id == underlying_id) {
            primary_id <- paste("..",primary_id,sep="")
            warning(paste('primary_id is the same as underlying_id,',
                'the instrument will be given a primary_id of', primary_id))
        }  
    }

    instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ... , type="future", underlying_id=underlying_id, assign_i=TRUE )
}

getRoot <- function(root_id, type=c('future','option')) {
  .Deprecated(new='getInstrument')
}

#' constructors for series contracts on instruments such as options and futures
#' 
#' constructors for series contracts on instruments such as options and futures
#'
#' In custom parameters for these series contracts, we have often found it
#' useful to store attributes such as local roll-on and roll-off dates
#' (rolling not on the \code{first_listed} or \code{expires}.  
#'
#' For \code{future_series} and \code{option_series} you may either provide a \code{primary_id} (or vector of \code{primary_id}s), 
#' OR both a \code{root_id} and \code{suffix_id}.
#' @param primary_id string describing the unique ID for the instrument. May be a vector for \code{future_series} and \code{option_series}
#' @param root_id string product code or underlying_id, usually something like 'ES' or 'CL' for futures, 
#' or the underlying stock symbol (maybe preceded with a dot) for equity options.
#' @param suffix_id string suffix that should be associated with the series, usually something like 'Z9' or 'Mar10' denoting expiration and year
#' @param first_traded string coercible to Date for first trading day
#' @param expires string coercible to Date for expiration date
#' @param maturity string coercible to Date for maturity date of bond series
#' @param callput right of option; call or put
#' @param strike strike price of option
#' @param payment_schedule not currently being implemented
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @aliases 
#' option_series
#' future_series
#' bond_series
#' @examples
#' \dontrun{
#' currency("USD")
#' future("ES","USD",multiplier=50, tick_size=0.25)
#' future_series('ES_U1')
#' future_series(root_id='ES',suffix_id='Z11')
#' stock('SPY','USD')
#' option('.SPY','USD',multiplier=100,underlying_id='SPY')
#' #can use either .SPY or SPY for the root_id. 
#' #it will find the one that is option specs.
#' option_series('SPY_110917C125', expires='2011-09-16')
#' option_series(root_id='SPY',suffix_id='111022P125')
#' option_series(root_id='.SPY',suffix_id='111119C130')
#' #multiple series instruments at once.
#' future_series(c("ES_H12","ES_M12"))
#' option_series(c("SPY_110917C115","SPY_110917P115"))
#' }
#' @export
#' @rdname series_instrument
future_series <- function(primary_id, root_id=NULL, suffix_id=NULL, first_traded=NULL, expires=NULL, identifiers = NULL, ...){
  if (missing(primary_id)) {
      if (all(is.null(c(root_id,suffix_id)))) {
          stop('must provide either a primary_id or both a root_id and a suffix_id')
      } else {
          if (is.null(suffix_id)) {
              sdate <- gsub("-","",expires)
              if (is.null(expires) || nchar(sdate) < 6) stop("must provide either 'expires' or 'suffix_id'")
              suffix_id <- paste(M2C()[as.numeric(substr(sdate,5,6))], substr(sdate,3,4),sep="")              
          }
          primary_id <- paste(root_id, suffix_id, sep="_")
      }
  } else if (length(primary_id) > 1) {
      if (!is.null(expires) || !is.null(first_traded)) 
          stop("'first_traded' and 'expires' must be NULL if calling with multiple primary_ids")
      return(unname(sapply(primary_id, future_series,
          root_id=root_id, suffix_id=suffix_id, first_traded=first_traded, 
          expires=expires, identifiers = identifiers, ...=...)))
  } else if (is.null(root_id) && !is.null(suffix_id) && parse_id(primary_id)$type == 'root') {
      #if we have primary_id, but primary_id looks like a root_id, and we have suffix_id and don't have root_id
      #then primary_id is really root_id and we need to replace primary_id
      root_id <- primary_id
      primary_id <- paste(root_id, suffix_id, sep="_")
  } else if (is.null(suffix_id) && parse_id(primary_id)$type == 'root') {
      #primary_id is actually a root_id, and suffix_id is NULL. we need to build suffix_id
      #using expires so that we can build a new primary_id.  Call recursively to handle this.
      return(future_series(root_id=primary_id, first_traded=first_traded, expires=expires, 
                        identifiers=identifiers, ...=...))
  }    

  pid <- parse_id(primary_id)
  if (is.null(root_id)) root_id <- pid$root
  if (is.null(suffix_id)) suffix_id <- pid$suffix
  if (is.null(expires)) {
    expires <- paste(pid$year, sprintf("%02d",match(pid$month, toupper(month.abb))),sep='-') 
    #if expires now has an NA in it, set it back to NULL
    if (!identical(integer(0), grep("NA",expires))) expires <- NULL
  }

  contract<-getInstrument(root_id,type='future')
  
  # TODO add check for Date equivalent in first_traded and expires

  ## with futures series we probably need to be more sophisticated,
  ## and find the existing series from prior periods (probably years or months)
  ## and then add the first_traded and expires to the time series bu splicing
  temp_series<-try(getInstrument(primary_id, silent=TRUE),silent=TRUE)
  if(inherits(temp_series,"future_series")) {
      message("updating existing first_traded and expires for ",primary_id)
      temp_series$first_traded<-unique(c(temp_series$first_traded,first_traded))
      temp_series$expires<-unique(c(temp_series$expires,expires))
      assign(primary_id, temp_series, envir=as.environment(.instrument))
      primary_id
  } else {
      dargs<-list(...)
      dargs$currency=NULL
      dargs$multiplier=NULL
      dargs$type=NULL
      if (is.null(dargs$src) && !is.null(contract$src)){
          dargs$src <- contract$src
      }
      instrument( primary_id = primary_id,
                 root_id = root_id,
                 suffix_id=suffix_id,
                 currency = contract$currency,
                 multiplier = contract$multiplier,
				 tick_size=contract$tick_size,
				 first_traded = first_traded,
                 expires = expires,
                 identifiers = identifiers,
                 type=c("future_series", "future"),
                 underlying_id = contract$underlying_id,
                 ...=dargs,
                 assign_i=TRUE
                ) 
  }
}

#' @export
#' @rdname instrument
option <- function(primary_id , currency , multiplier , tick_size=NULL, identifiers = NULL, ..., underlying_id=NULL){
  if (missing(primary_id)) primary_id <- paste(".",underlying_id,sep="")
  if (length(primary_id) > 1) stop("'primary_id' must be of length 1")
  if (missing(currency) && !is.null(underlying_id)) {
        uinstr <- getInstrument(underlying_id,silent=TRUE)
        if (is.instrument(uinstr)) {
            currency <- uinstr$currency
        } else stop("'currency' is a required argument")
    }
  if(is.null(underlying_id)) {
      warning("underlying_id should only be NULL for cash-settled options")
  } else {
      if(!exists(underlying_id, where=.instrument,inherits=TRUE)) warning("underlying_id not found") # assumes that we know where to look
      if (primary_id == underlying_id) {
          primary_id <- paste(".",primary_id,sep="")
          warning(paste('primary_id is the same as underlying_id,',
                'the instrument will be given a primary_id of', primary_id))
      }  
  }
  ## now structure and return
  instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ... , type="option", underlying_id=underlying_id, assign_i=TRUE )
}

#' @export
#' @rdname series_instrument
option_series <- function(primary_id , root_id = NULL, suffix_id = NULL, first_traded=NULL, 
                            expires=NULL, callput=c("call","put"), strike=NULL, identifiers = NULL, ...){
    if (missing(primary_id) ) {
        if (all(is.null(c(root_id,suffix_id)))) 
            stop('must provide either a primary_id or both a root_id and a suffix_id')
        else { #if you give it only a root_id it will make the suffix_id using expires, callput, and strike
            if (is.null(suffix_id)) {
                sdate <- try(as.Date(expires),silent=TRUE)
                if (inherits(sdate,'try-error')) stop("expires is missing or of incorrect format")
                sright <- try(switch(callput, C=,c=,call="C", P=,p=,put="P"),silent=TRUE)
                if (inherits(sright,'try-error')) 
                    stop("must provide 'callput' or a 'suffix_id' from which 'callput' can be inferred.")
                if (is.null(strike)) 
                    stop("must provide 'strike' or a 'suffix_id' from which 'strike' can be inferred.")
                suffix_id <- paste(format(sdate,'%y%m%d'), sright, strike, sep="")
            }
            primary_id <- paste(root_id, suffix_id, sep="_")
        }
    } else if (length(primary_id) > 1) {
      if (!is.null(expires) || !is.null(first_traded)) 
          stop("'first_traded' and 'expires' must be NULL if calling with multiple primary_ids")         
      return(unname(sapply(primary_id, option_series, 
          root_id=root_id, suffix_id=suffix_id, first_traded=first_traded,
          expires=expires, callput=callput, strike=strike, identifiers=identifiers, ...=...)))
    } else if (is.null(root_id) && !is.null(suffix_id) && parse_id(primary_id)$type == 'root') {
          #if we have primary_id, but primary_id looks like a root_id, and we have suffix_id and don't have root_id
          #then primary_id is really root_id and we need to replace primary_id
          root_id <- primary_id
          primary_id <- paste(root_id, suffix_id, sep="_")
    } else if (is.null(suffix_id) && parse_id(primary_id)$type == 'root') {
        #primary_id is actually a root_id, and suffix_id is NULL. we need to build suffix_id so that
        #we can build a new primary_id.  Call recursively to handle this.
        return(option_series(root_id=primary_id, first_traded=first_traded, expires=expires, 
                            callput=callput, strike=strike, identifiers=identifiers, ...=...))
    }    
    pid <- parse_id(primary_id)
    if (is.null(root_id)) root_id <- pid$root
    if (is.null(suffix_id)) suffix_id <- pid$suffix
    if (is.null(strike)) {
        if (is.na(pid$strike)) stop('strike must be provided.')
        strike <- pid$strike
    }
    if (is.null(expires)) {
        expires <- paste(pid$year, sprintf("%02d",match(pid$month, toupper(month.abb))),sep='-') 
        if (!identical(integer(0), grep("NA",expires))) 
            stop("must provide 'expires' formatted '%Y-%m-%d', or a 'suffix_id' from which to infer 'expires'")
    }
    contract<-getInstrument(root_id, type='option')
    if (!hasArg(src) && !is.null(contract$src)){
        src <- contract$src
    } else src <- NULL
    ## with options series we probably need to be more sophisticated,
    ## and find the existing series from prior periods (probably years)
    ## and then add the first_traded and expires to the time series
    if(length(callput)==2) callput <- switch(pid$right, C='call', P='put')
    if (is.null(callput)) stop("value of callput must be specified as 'call' or 'put'")
    temp_series<-try(getInstrument(primary_id, silent=TRUE),silent=TRUE)
    if(inherits(temp_series,"option_series")) {
        message("updating existing first_traded and expires for ", primary_id)
        temp_series$first_traded<-unique(c(temp_series$first_traded,first_traded))
        temp_series$expires<-unique(c(temp_series$expires,expires))
        assign(primary_id, temp_series, envir=as.environment(.instrument))
        primary_id
    } else {
        instrument( primary_id = primary_id,
                    root_id = root_id,
                    suffix_id = suffix_id,
                    currency = contract$currency,
                    multiplier = contract$multiplier,
                    tick_size=contract$tick_size,
                    first_traded = first_traded,
                    expires = expires,
                    identifiers = identifiers,
                    callput = callput,
                    strike = strike,
                    underlying_id = contract$underlying_id,
                    if (!is.null(src)) src=src,
                    ...,
                    type=c("option_series", "option"),
                    assign_i=TRUE
                  ) 
    }
}

#' constructor for series of options using yahoo data
#'
#' Defines a chain or several chains of options by looking up necessary info from yahoo 
#' If \code{Exp} is missing it will define only the nearby options. 
#' If \code{Exp} is NULL it will define all options
#' 
#' If \code{first_traded} and/or \code{tick_size} should not be the same for all options being defined, they should be left NULL and defined outside of this function.
#' @param symbol character vector of ticker symbols of the underlying instruments (Currently, should only be stock tickers)
#' @param Exp Expiration date or dates to be passed to getOptionChain
#' @param currency currency of underlying and options
#' @param multiplier contract multiplier. Usually 100 for stock options
#' @param first_traded first date that contracts are tradeable. Probably not applicable if defining several chains.
#' @param tick_size minimum price change of options.
#' @return Called for side-effect. The instrument that is created and stored will inherit option_series, option, and instrument classes. 
#' @references Yahoo \url{http://finance.yahoo.com}
#' @author Garrett See
#' @note Has only been tested with stock options.
#' The options' currency should be the same as the underlying's.
#' @seealso \code{\link{option_series}}, \code{\link{instrument}}, quantmod:::getOptionChain
#' @examples
#'
#' \dontrun{
#' option_series.yahoo('SPY') #only nearby calls and puts
#' option_series.yahoo('DIA', Exp=NULL) #all chains
#' ls(.instrument, all.names=TRUE)
#' }
#' @export
option_series.yahoo <- function(symbol, Exp, currency="USD", multiplier=100, first_traded=NULL, tick_size=NULL) {
    #FIXME: identifiers?
    
    if (!("package:quantmod" %in% search() || require("quantmod",quietly=TRUE))) {
        stop("Please install quantmod before using this function.")
    }    

    opts <- getOptionChain(Symbol=symbol,Exp=Exp, src="yahoo")
	
	locals <- function(x) c(rownames(x$calls),rownames(x$puts))
	if (is.null(opts$calls)) { #if is.null(Exp) we'll get back all chains
		led <- (lapply(opts, locals))  
		optnames <- unname(do.call(c, led))	#FIXME: Is this a reasonable way to get rownames?		
	} else 	optnames <- locals(opts) #c(rownames(opts$calls),rownames(opts$puts))
	
    idout <- NULL
    for (r in optnames) {
        root_id <- symbol
        si <- gsub(symbol,"",r) #suffix_id
        expiry <- substr(si,1,6)
        right <- substr(si,7,7)
        strike <- as.numeric(substr(si,8,15))/1000
#        local <- paste(symbol, si, sep="   ")      
		clean.si <- paste(expiry,right,strike,sep="")		
		primary_id <- paste(symbol, "_", clean.si, sep="")

		#create currency if it doesn't exist	#?? Any reason not to ??	
		tmpccy <- try(getInstrument(currency,silent=TRUE),silent=TRUE)
		if (!inherits(tmpccy, "currency")) {
			warning(paste("Created currency", currency, "because it did not exist."))			
			currency(currency) #create it
		}
		#create option spec if we need to.
		tmpInstr <- try(getInstrument(paste('.',symbol,sep=""),silent=TRUE),silent=TRUE)
		if (!inherits(tmpInstr, "option")) {
			warning(paste('Created option specs for root',paste('.',symbol,sep="")))
			option(primary_id=paste('.',symbol,sep=""), currency=currency,
					multiplier=multiplier, tick_size=tick_size, 
					underlying_id=symbol)		
		}
		#create specific option
        tempseries = instrument( primary_id=primary_id, 
				                    suffix_id=clean.si, 
				                    first_traded=first_traded, 
				                    currency=currency, 
				                    multiplier=multiplier, 
				                    tick_size=tick_size, 
				                    expires=as.Date(paste(paste('20', substr(expiry,1,2),sep=""), 
                                                    substr(expiry,3,4), 
                                                    substr(expiry,5,6),sep="-")), 
				                    callput=switch(right, C="call", P="put"), #to be consistent with the other option_series function	
				                    strike=strike, 
				                    underlying_id=symbol, 
				                    type = c("option_series","option"), 
				                    defined.by='yahoo', assign_i=TRUE
                                )    
#		option_series(primary_id=primary_id, suffix_id=si, exires=expiry, currency=currency,
#                        callput = switch(right,C='call',P='put'))   
        idout <- c(idout, primary_id)    
    }
    idout
}

#' @export
#' @rdname instrument
currency <- function(primary_id , currency=NULL , multiplier=1 , identifiers = NULL, ...){
  if (length(primary_id) > 1) return(unname(sapply(primary_id, currency, identifiers=identifiers, ...=...)))
  currency_temp <- list(primary_id = primary_id,
          currency = primary_id,
          multiplier = 1,
          tick_size= .01,
          identifiers = identifiers,
          type = "currency"
  )
  currency_temp <- c(currency_temp,list(...))   
  class(currency_temp)<-c("currency","instrument")
  assign(primary_id, currency_temp, envir=as.environment(.instrument) )
  primary_id
}

#' class test for object supposedly of type 'currency'
#' @param x object to test for type
#' @export
is.currency <- function( x ) {
#FIXME: This should not get instrument, but it will break everyone's code if I change it. -Garrett
  x<-getInstrument(x, silent=TRUE)
  inherits( x, "currency" )
}

#' constructor for spot exchange rate instruments
#' 
#' Currency symbols (like any symbol) may be any combination of alphanumeric characters, 
#' but the FX market has a convention that says that the first currency in a 
#' currency pair is the 'target'  and the second currency in the symbol pair 
#' is the currency the rate ticks in.  So 'EURUSD' can be read as 'USD per 1 EUR'.
#' 
#' In \code{FinancialInstrument} the \code{currency} of the instrument should 
#' be the currency that the spot rate ticks in, so it will typically be the second
#' currency listed in the symbol. 
#' 
#' Thanks to Garrett See for helping sort out the inconsistencies in different naming and calculating conventions. 
#' @param primary_id string identifier, usually expressed as a currency pair 'USDYEN' or 'EURGBP'
#' @param currency string identifying the currency the exchange rate ticks in
#' @param counter_currency string identifying the currency which the rate uses as the base 'per 1' multiplier
#' @param identifiers named list of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters
#' @references http://financial-dictionary.thefreedictionary.com/Base+Currency
#' @export
exchange_rate <- function (primary_id = NULL, currency = NULL, counter_currency = NULL, identifiers = NULL, ...){
  # exchange_rate_temp = instrument(primary_id , currency , multiplier=1 , tick_size=.01, identifiers = identifiers, ..., type="exchange_rate")
  if (is.null(primary_id) && !is.null(currency) && !is.null(counter_currency)) {
    primary_id <- c(outer(counter_currency,currency,paste,sep=""))
    same.same <- function(x) substr(x,1,3) == substr(x,4,6)
    primary_id <- primary_id[!same.same(primary_id)]
  } else if (is.null(primary_id) && (is.null(currency) || is.null(counter_currency))) 
    stop("Must provide either 'primary_id' or both 'currency' and 'counter_currency'")
  if (length(primary_id) > 1) return(unname(sapply(primary_id, exchange_rate, identifiers=identifiers, ...=...)))
  
  if (is.null(currency)) currency <- substr(primary_id,4,6)
  if (is.null(counter_currency)) counter_currency <- substr(primary_id,1,3)
  if(!exists(currency, where=.instrument,inherits=TRUE)) warning(paste("currency",currency,"not found")) # assumes that we know where to look
  if(!exists(counter_currency, where=.instrument,inherits=TRUE)) warning(paste("counter_currency",counter_currency,"not found")) # assumes that we know where to look

  ## now structure and return
  instrument(primary_id=primary_id , currency=currency , multiplier=1 , tick_size=.01, identifiers = identifiers, ..., counter_currency=counter_currency, type=c("exchange_rate","currency"), assign_i=TRUE)
}

#TODO  auction dates, coupons, etc for govmt. bonds
#' @export
#' @rdname instrument
bond <- function(primary_id , currency , multiplier, tick_size=NULL , identifiers = NULL, ...){
    if (missing(currency)) stop ("'currency' is a required argument")
    if (length(primary_id) > 1) stop("'primary_id' must be of length 1 for this function")
    instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , tick_size=tick_size, identifiers = identifiers, ..., type="bond", assign_i=TRUE )
}

#' @export
#' @rdname series_instrument
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
        temp_series$maturity<-c(temp_series$maturity,maturity)
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

#' Create an instrument based on name alone
#'
#' Given a name, this function will attempt to create
#' an instrument of the appropriate type.
#'
#' If \code{currency} is not already defined, it will be defined (unless it is not 3 uppercase characters).
#' The default value for \code{currency} is \dQuote{USD}.  If you do not provide a value for \code{currency}, 
#' \dQuote{USD} will be defined and used to create the instrument.  
#'
#' If \code{primary_id} is 6 uppercase letters and \code{default_type} is not provided, 
#' it will be assumed that it is the primary_id of an \code{\link{exchange_rate}}, in which case, 
#' the 1st and 2nd half of \code{primary_id} will be defined as \code{\link{currency}}s if not 
#' the names of already defined \code{\link{instrument}}s.
#' If the \code{primary_id} begins with a \dQuote{^} it will be assumed that it is a yahoo symbol and
#' that the instrument is an index (synthetic), and the \sQuote{src} will be set to \dQuote{yahoo}. 
#' (see \code{\link{setSymbolLookup}})
#'
#' If it is not clear from the \code{primary_id} what type of instrument to create, an instrument of type 
#' \code{default_type} will be created (which is 'NULL' by default).
#' This will happen when \code{primary_id} is that of a \code{\link{stock}} \code{\link{future}} 
#' \code{\link{option}} or \code{\link{bond}}.  This may also happen if \code{primary_id} is that of a 
#' \code{\link{future_series}} or \code{\link{option_series}} but the corresponding \code{future} or 
#' \code{option} cannot be found.  In this case, the instrument type would be \code{default_type}, but a lot
#' of things would be filled in as if it were a valid series instrument (e.g. \sQuote{expires}, \sQuote{strike}, 
#' \sQuote{suffix_id}, etc.)
#' @param primary_id charater primary identifier of instrument to be created
#' @param currency character name of currency that instrument will be denominated it. Default=\dQuote{USD}
#' @param multiplier numeric product multiplier
#' @param silent TRUE/FALSE. silence warnings?
#' @param default_type What type of instrument to make if it is not clear from the primary_id. ("stock", "future", etc.) Default is NULL.
#' @param ... other passthrough parameters
#' @return Primarily called for its side-effect, but will return the name of the instrument that was created
#' @note This is not intended to be used to create instruments of type \code{stock}, \code{future}, \code{option},
#' or \code{bond} although it may be updated in the future.
#' @author Garrett See
#' @examples
#' \dontrun{
#' instrument.auto("CL_H1.U1")
#' getInstrument("CL_H1.U1") #guaranteed_spread
#' 
#' instrument.auto("ES_H1.YM_H1")
#' getInstrument("ES_H1.YM_H1") #synthetic
#' 
#' currency(c("USD","EUR"))
#' instrument.auto("EURUSD")
#' getInstrument("EURUSD") #made an exchange_rate
#' 
#' instrument.auto("VX_H11") #no root future defined yet!
#' getInstrument("VX_H11") #couldn't find future, didnt make future_series
#' future("VX","USD",1000,underlying_id=synthetic("SPX","USD")) #make the root 
#' instrument.auto("VX_H11") #and try again
#' getInstrument("VX_H11") #made a future_series
#' }
#' @export
instrument.auto <- function(primary_id, currency='USD', multiplier=1, silent=FALSE, default_type='NULL', ...) {
##TODO: check formals against dots and remove duplicates from dots before calling constructors to avoid
# 'formal argument "multiplier" matched by multiple actual arguments'
    if (!is.currency(currency)) {
        if (nchar(currency) != 3 || currency != toupper(currency))
            stop(paste(currency, "is not defined,",
                "and it will not be auto defined because it does not appear to be valid."))
        currency(currency)    
        if (!silent) cat(paste('Created currency', currency,'because it was not defined.\n'))
    }
    warned <- FALSE
    dargs <- list(...)    
    pid <- parse_id(primary_id)
    type <- NULL
    if (any(pid$type == 'calendar')) {
        return(guaranteed_spread(primary_id, currency=currency, defined.by='auto', ...))
    } 
    if (any(pid$type == 'butterfly')) {
        return(butterfly(primary_id, currency=currency, defined.by='auto', ...))
    }
    if (any(pid$type == 'future')) {
        root <- getInstrument(pid$root,silent=TRUE,type='future')
        if (is.instrument(root) && !inherits(root, 'future_series')) {
            return(future_series(primary_id,defined.by='auto',...))
        } else {
            if (!silent) {
                warning(paste(primary_id," appears to be a future_series, ", 
                        "but its root cannot be found. ", 
                        "Creating _", default_type, "_ instrument instead.", sep=""))
                warned <- TRUE
            }
            dargs$root_id <- pid$root
            dargs$suffix_id <- pid$suffix
            dargs$expires <- paste(pid$year, sprintf("%02d", month_cycle2numeric(pid$month)), sep="-")
        }
    }
    if (any(pid$type == 'option')) {
        root <- getInstrument(pid$root,silent=TRUE,type='option')
        if (is.instrument(root) && !inherits(root, 'option_series')) {
            return(option_series(primary_id, defined.by='auto', ...))
        } else {
            if (!silent) {
                warning(paste(primary_id," appears to be an option_series, ", 
                    "but its root cannot be found. ", 
                    "Creating _", default_type, "_ instrument instead.", sep=""))
                warned <- TRUE
            }
            dargs$root_id <- pid$root
            dargs$suffix_id <- pid$suffix
            dargs$expires <- if(pid$format == 'opt2') {
                    as.Date(substr(pid$suffix,1,6),format='%y%m%d')
                } else if (pid$format == 'opt4') {
                    as.Date(substr(pid$suffix,1,8),format='%Y%m%d')
                } else paste(pid$year, sprintf("%02d", month_cycle2numeric(pid$month)), sep="-")
            dargs$multiplier=100
            dargs$callput <- switch(pid$right, C='call', P='put')
            dargs$strike <- pid$strike
        }
    } 
    if (any(pid$type == 'exchange_rate'))
        return(exchange_rate(primary_id, defined.by='auto', ...))
    #if we weren't given a default_type, then if it's 6 uppercase letters, make an exchange rate    
    if (default_type == 'NULL' && nchar(primary_id) == 6 && sum(attr(gregexpr("[A-Z]",primary_id)[[1]],"match.length")) == 6) {
        if (!is.instrument(getInstrument(substr(primary_id,1,3), silent=TRUE))) {
            ccy.st <- currency(substr(primary_id,1,3), defined.by='auto') 
            if (!silent) cat("Created currency", ccy.st, "because it was not defined.\n") 
        }
        if (!is.instrument(getInstrument(substr(primary_id,4,6), silent=TRUE))) { 
            ccy.st <- currency(substr(primary_id,4,6), defined.by='auto') 
            if (!silent) cat("Created currency", ccy.st, "because it was not defined.\n")
        }
        return(exchange_rate(primary_id, defined.by='auto', ...))
    }
    if (any(pid$type == 'synthetic')) {
        if (!is.na(pid$format) && pid$format == 'yahooIndex') {
            return(synthetic(gsub("\\^","",primary_id), currency=currency, src=list(src='yahoo',name=primary_id), defined_by='auto', ...))
        } else return(synthetic(members=strsplit(primary_id,"\\.")[[1]], currency=currency, defined.by='auto', ...) )
    } 
    dargs$primary_id <- primary_id
    dargs$currency <- currency
    dargs$multiplier <- multiplier
    dargs$defined.by='auto'
    if(is.function(try(match.fun(default_type),silent=TRUE))) {
        if (!silent && !warned) 
            warning('Creating a _', default_type, '_ instrument because ', 
                    primary_id, ' is of an ambiguous format.') 
         return(do.call(default_type, dargs))
    }
    if (!silent && !warned) 
        warning(paste(primary_id, 'is not of an unambiguous format.', 
                'Creating basic instrument with multiplier 1.'))
    dargs$assign_i <- TRUE
    do.call(instrument, dargs)
}
 
   
#' primary accessor function for getting objects of type 'instrument'
#' 
#' This function will search the \code{.instrument} environment for objects of
#' type \code{type}, using first the \code{primary_id} and then any \code{identifiers} 
#' to locate the instrument.  Finally, it will try adding 1 and then 2 dots to the
#' \code{primary_id} to see if an instrument was stored there to avoid naming conflicts.
#' 
#' \code{\link{future}} and \code{\link{option}} objects may have a primary_id that 
#' begins with 1 or 2 dots (in order to avoid naming conflics).  For example, the root specs
#' for options (or futures) on the stock with ticker "SPY" may be stored with a primary_id 
#' of "SPY", ".SPY", or "..SPY".  \code{getInstrument} will try using each possible \code{primary_id}
#' until it finds an instrument of the appropriate \code{type}
#' @param x string identifier of instrument to retrieve
#' @param Dates date range to retrieve 'as of', may not currently be implemented
#' @param silent if TRUE, will not warn on failure, default FALSE
#' @param type type of object to look for. See Details
#' @examples 
#' \dontrun{
#' option('..VX', multiplier=100, 
#'   underlying_id=future('.VX',multiplier=1000, 
#'     underlying_id=synthetic('VIX', currency("USD"))))
#'
#' getInstrument("VIX")
#' getInstrument('VX') #returns the future
#' getInstrument("VX",type='option')
#' getInstrument('..VX') #finds the option
#' }
#' @export
#' @rdname getInstrument
getInstrument <- function(x, Dates=NULL, silent=FALSE, type='instrument'){
    tmp_instr<-try(get(x,pos=.instrument),silent=TRUE) #removed inherits=TRUE
    if(inherits(tmp_instr,"try-error") | !inherits(tmp_instr, type)){
        #first search
        instr_list<-ls(pos=.instrument)
        for (instr in instr_list){
            tmp_instr<-try(get(instr,pos=.instrument),silent=TRUE)
            if(inherits(tmp_instr, type) && length(grep(x,tmp_instr$identifiers))) {
                return(tmp_instr)
            }
        }
        #If not found, see if it begins with dots (future or option root)
        #strip out the dots and add them back 1 at a time to the beginning of id
        x <- gsub("\\.", "", x) 
        tmp_instr<-try(get(x,pos=.instrument),silent=TRUE)
        if(!inherits(tmp_instr,type)) {
            tmp_instr<-try(get(paste(".",x,sep=""),pos=.instrument),silent=TRUE)
            if(!inherits(tmp_instr,type)) {
                tmp_instr<-try(get(paste("..",x,sep=""),pos=.instrument),silent=TRUE)
            }
        }
        if (!inherits(tmp_instr,'try-error') && inherits(tmp_instr, type)) return(tmp_instr)
        if(!silent) warning(paste(type,x,"not found, please create it first."))
        return(FALSE)
    } else{
        return(tmp_instr)
    }
    #TODO add Date support to instrument, to get the proper value given a specific date
}


#' add or change an attribute of an instrument
#' 
#' This function will add or overwrite the data stored in the specified slot of the specified instrument.
#'
#' If the \code{attr} you are trying to change is the \dQuote{primary_id,} the instrument will be renamed.
#' (A copy of the instrument will be stored by the name of \code{value} and the old instrument will be removed.)
#' If the \code{attr} you are changing is \dQuote{type}, the instrument will be reclassed with that type.
#' If \code{attr} is \dQuote{src}, \code{value} will be used in a call to \code{setSymbolLookup}.
#' Other checks are in place to make sure that \dQuote{currency} remains a \code{\link{currency}} object and that
#' \dQuote{multiplier} and \dQuote{tick_size} can only be changed to reasonable values.
#' @param primary_id primary_id of the instrument that will be updated
#' @param attr name of the slot that will be added or changed
#' @param value what to assign to the \code{attr} slot of the \code{primary_id} instrument
#' @return called for side-effect
#' @note you can remove an attribute/slot from an instrument by calling this function with \code{value=NULL}
#' @examples
#' \dontrun{
#' currency("USD")
#' stock("SPY","USD")
#' instrument_attr("USD","description","U.S. Dollar")
#' instrument_attr("SPY", "description", "An ETF")
#' getInstrument("USD")
#' getInstrument("SPY")
#' 
#' #Call with value=NULL to remove an attribute
#' instrument_attr("SPY", "description", NULL)
#' getInstrument("SPY")
#'
#' instrument_attr("SPY","primary_id","SPX") #move/rename it
#' instrument_attr("SPX","type","synthetic") #re-class
#' instrument_attr("SPX","src",list(src='yahoo',name='^GSPC')) #setSymbolLookup
#' getSymbols("SPX") #knows where to look because the last line setSymbolLookup
#' getInstrument("SPX")
#' }
#' @export
instrument_attr <- function(primary_id, attr, value) {
    instr <- try(getInstrument(primary_id, silent=TRUE))
    if (inherits(instr, 'try-error') || !is.instrument(instr))
        stop(paste('instrument ',primary_id,' must be defined first.',sep=''))
    instr[[attr]] <- value
    if (attr == 'primary_id') rm(list = primary_id, pos = .instrument)
    if (attr == 'currency' && !is.instrument(getInstrument(value,type='currency',silent=TRUE)))
        stop("currency ", value, " must be an object of type 'currency'")
    if (attr == 'multiplier' && (!is.numeric(value) || length(value) > 1))
        stop("multiplier must be a single number")
    if (attr == 'tick_size' && (!is.null(value) && (!is.numeric(value) || length(value) > 1)))
        stop("tick_size must be NULL or a single number")
    if (attr == 'type') {
        tclass <- unique(c(value, "instrument"))
        class(instr) <- tclass
    }
    if (attr == 'src') {
        sarg <- list()
        sarg[[instr$primary_id]] <- value
        setSymbolLookup(sarg)
    }
    assign(instr$primary_id, instr, pos=.instrument)
}

#' instrument class print method
#' 
#' @method print instrument
#' @S3method print instrument
#' @author Joshua Ulrich, Garrett See
#' @keywords internal
print.instrument <- function(x, ...) {
  str(unclass(x), comp.str="", no.list=TRUE, give.head=FALSE,
    give.length=FALSE, give.attr=FALSE, nest.lev=-1, indent.str="")
  invisible(x)
}

