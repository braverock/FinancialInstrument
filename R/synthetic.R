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

#' @export
#' @rdname synthetic.instrument
synthetic <- function(primary_id , currency , multiplier=1, identifiers = NULL, ..., members=NULL, type=c("synthetic", "instrument"))
{
    synthetic_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , identifiers = identifiers, ...=..., type=type, members=members, assign_i=TRUE )    
}

#' constructors for synthetic instruments
#' 
#' THIS FUNCTION HAS BEEN DEPRECATED. Use synthetic.instrument instead.
#' Simple derivatives like \code{\link{option}} or \code{\link{future}} contracts typically have one underlying instrument.  
#' While properties like strike and expiration vary for these derivative contracts or series, the underlying is well understood.
#' 
#' More complex derivatives are typically modeled as baskets of underlying products, and are typically traded over-the-counter or as proprietary in-house products.
#' 
#' The general \code{synthetic} function is intended to be extended to support these arbitrary baskets of assets.  
#' 
#' Currently implemented examples are in ratio based spreads in \code{spread} and in exchange-guaranteed spread products in \code{guaranteed_spread}.
#' 
#' We welcome assistance from others to model more complex OTC derivatives such as swap products.
#' 
#' @param primary_id string describing the unique ID for the instrument
#' @param currency string describing the currency ID of an object of type \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument currency to get to notional value
#' @param identifiers character vector of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters such as tick_size -- the tick increment of the instrument price in it's trading venue, as numeric quantity (e.g. 1/8 is .125)
#' @param type class string, should not be set by users
#' @param members character vector of instrument identifiers that make up the synthetic
#' @param memberratio numeric vector of ratio relationships between members, e.g. c(4,3) for a 4:3 spread
#' @note DEPRECATED
#' @export
synthetic.ratio <- function(primary_id , currency ,  members, memberratio, ..., multiplier=1, identifiers = NULL, type=c("synthetic.ratio","synthetic","instrument"))
{
    #TODO make sure that with options/futures or other  instruments that we have you use the base contract
    if(!is.list(members)){
        if(length(members)!=length(memberratio) | length(members)<2){
            stop("length of members and memberratio must be equal, and contain two or more instruments")
        } else {
            memberlist<-list(members=members,memberratio=memberratio,currencies=vector(),memberpositions=NULL)   
        }
        for(member in members) {
            tmp_symbol<-member
            tmp_instr<-try(getInstrument(member))
            if(inherits(tmp_instr,"try-error") | !is.instrument(tmp_instr)){
                message(paste("Instrument",tmp_symbol," not found, using currency of",currency))
                memberlist$currencies[member]<-currency
            } else {
                memberlist$currencies[member]<-tmp_instr$currency                
            }
        }
        names(memberlist$members)<-memberlist$members
        names(memberlist$memberratio)<- memberlist$members
        names(memberlist$currencies)<- memberlist$members
    } else {
        # TODO do some sanity checking here on the list elements
        warning("passing in members as a list not fully tested")
        memberlist=members
    }
    if(is.null(currency)) currency<- as.character(memberlist$currencies[1]) #use the currency of the front leg

    if (hasArg(tick_size)){
        tick_size<-match.call(expand.dots=TRUE)$tick_size  
    } else tick_size<-0 

    synthetic(primary_id=primary_id , currency=currency , multiplier=multiplier , identifiers = identifiers, members=memberlist , memberratio=memberratio, ...=... ,type=type, tick_size=tick_size)
}

#' synthetic instrument constructors
#' 
#' define spreads, guaranteed_spreads, butterflies, and other synthetic instruments
#'
#' Simple derivatives like \code{\link{option}} or \code{\link{future}} contracts typically have one underlying instrument.  
#' While properties like strike and expiration vary for these derivative contracts or series, the underlying is well understood.
#'
#' More complex derivatives are typically modeled as baskets of underlying products, and are typically traded over-the-counter or as proprietary in-house products.
#'
#' The general \code{synthetic} function is intended to be extended to support these arbitrary baskets of assets.  
#' 
#' \code{spread} \code{guaranteed_spread} and \code{butterfly} are wrappers for \code{synthetic.instrument}. \code{synthetic.instrument} will make a call to synthetic to create the final instrument.
#' 
#' The \code{suffix_id} parameter of wrapper functions such as  \code{guaranteed_spread} is presumed to 
#' be a string describing the \code{members}. 
#' It will be \code{\link{strsplit}} using the regex "[-;:_,\\.]" to create the \code{members} vector,
#' and potentially combined with a \code{root_id}.
#' 
#' We welcome assistance from others to model more complex OTC derivatives such as swap products.
#'
#' @aliases synthetic.instrument synthetic spread guaranteed_spread butterfly
#' @param primary_id chr string of primary identifier of instrument to be defined.
#' @param currency chr string name of currency denomination
#' @param members vector of primary_ids of member instruments
#' @param memberratio vector of weights for each leg. negative numbers for selling.
#' @param \dots any other passthrough parameters
#' @param multiplier multiplier of the spread
#' @param tick_size minimum price change of the spread
#' @param identifiers identifiers
#' @param type type of instrument; wrappers do not require this.
#' @param root_id instrument identifier for the root contract, default NULL
#' @param suffix_id identifiers for the member contract suffixes, default NULL, will be split as \code{members}, see Details
#' @return called for side effect. stores an instrument in .instrument environment
#' @author author Brian Peterson, Garrett See
#' @seealso instrument, future, option_series.yahoo
#' @examples
#'
#' \dontrun{
#' stock('SPY','USD',1)
#' stock('DIA','USD',1)
#' spread('SPYDIA','USD',c('SPY','DIA'),c(1,-1))
#' }
#' @export
synthetic.instrument <- function (primary_id, currency, members, memberratio, ..., multiplier = 1, tick_size=NULL, 
    identifiers = NULL, type = c("synthetic.instrument", "synthetic", "instrument")) 
{
    if (!is.list(members)) {
        if (length(members) != length(memberratio) | length(members) < 
            2) {
            stop("length of members and memberratio must be equal, and contain two or more instruments")
        }
        else {
            memberlist <- list(members = members, memberratio = memberratio, 
                currencies = vector(), memberpositions = NULL)
        }
        for (member in members) {
            tmp_symbol <- member
            tmp_instr <- try(getInstrument(member))
            if (inherits(tmp_instr, "try-error") | !is.instrument(tmp_instr)) {
                message(paste("Instrument", tmp_symbol, " not found, using currency of", 
                  currency))
                memberlist$currencies[member] <- currency
            }
            else {
                memberlist$currencies[member] <- tmp_instr$currency
            }
        }
        names(memberlist$members) <- memberlist$members
        names(memberlist$memberratio) <- memberlist$members
        names(memberlist$currencies) <- memberlist$members
    }
    else {
        warning("passing in members as a list not fully tested")
        memberlist = members
    }
    if (is.null(currency)) 
        currency <- as.character(memberlist$currencies[1])
	
    synthetic(primary_id = primary_id, currency = currency, multiplier = multiplier, 
        identifiers = identifiers, memberlist = memberlist, memberratio = memberratio, tick_size=tick_size,
        ... = ..., members = members, type = type)
}


#' @export
#' @rdname synthetic.instrument
spread <- function (primary_id, currency = NULL, members, memberratio, tick_size=NULL,
    ..., multiplier = 1, identifiers = NULL) 
{
    synthetic.instrument(primary_id = primary_id, currency = currency, 
      members = members, memberratio = memberratio, ...=..., tick_size=tick_size,
      multiplier = multiplier, identifiers = identifiers, 
      type = c("spread", "synthetic.instrument", "synthetic", "instrument"))
}


#' @export
#' @rdname synthetic.instrument
butterfly <- function(primary_id, currency=NULL, members,tick_size=NULL, identifiers=NULL, ...)
{
##TODO: butterfly can refer to expirations (futures) or strikes (options)
##TODO: A butterfly could either have 3 members that are outrights, or 2 members that are spreads
  if (length(members) == 3) {
    synthetic.instrument(primary_id=primary_id,currency=currency,members=members,
	    memberratio=c(1,-2,1), multiplier=1, tick_size=tick_size,
	    identifiers=NULL, ...=..., type = c('butterfly','spread','synthetic.instrument',
	    'synthetic','instrument'))
  } else if (length(members) == 2) {
      stop('butterfly currently only supports 3 leg spreads (i.e. no spread of spreads yet.)')

  } else stop('A butterfly must either have 3 outright legs or 2 spread legs') 
  
}


#' @export
#' @rdname synthetic.instrument
guaranteed_spread <- calendar_spread <- function (primary_id, currency=NULL, root_id=NULL, suffix_id=NULL, members = NULL, memberratio = c(1,-1), ..., 
    multiplier = NULL, identifiers = NULL, tick_size=NULL)
{

	if (hasArg(suffix_id)) {
		if(hasArg(root_id)) {
			id<- paste(root_id,suffix_id,sep="_")
		} else {
			id <- paste(primary_id, suffix_id, sep = "_")
		}
    } else id <- primary_id
    
    id<-make.names(id) #force syntactically valid primary_id
    
	if (is.null(members) && hasArg(suffix_id)) {
		#construct members from suffix_id and either primary_id or root_id
        members <- unlist(strsplit(suffix_id, "[-;:_,\\.]"))
		if(hasArg(root_id)) {
			members <- paste(root_id,members, sep ="_")
		} else {
			members <- paste(primary_id, members, sep = "_")
		}		
	}
	
	if(hasArg(root_id)) {
		# go get other instrument quantities from the root contract
		root_contract<-getInstrument(root_id)
		if(is.instrument(root_contract)){
			if(is.null(currency)) currency <- root_contract$currency
			if(is.null(multiplier)) multiplier <- root_contract$multiplier
			if(is.null(tick_size)) tick_size <-  root_contract$tick_size
		}
	} 
	
    synthetic.instrument(primary_id = id, currency = currency, members = members, 
	memberratio = memberratio, multiplier = multiplier, identifiers = NULL, 
	tick_size=tick_size, ... = ..., type = c("guaranteed_spread", "spread", 
	"synthetic.instrument", "synthetic", "instrument"))
}
