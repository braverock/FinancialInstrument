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

#' @export
synthetic <- function(primary_id , currency , multiplier=1, identifiers = NULL, ..., members=NULL, type=c("synthetic", "instrument"))
{
    synthetic_temp = instrument(primary_id=primary_id , currency=currency , multiplier=multiplier , identifiers = identifiers, ...=..., type=type, members=members, assign_i=TRUE )    
}

#' constructors for synthetic instruments
#' 
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
#' @param ... any other passthru parameters 
#' @param type class string, should not be set by users
#' @param members character vector of instrument identifiers that make up the synthetic
#' @param memberratio numeric vector of ratio relationships between members, e.g. c(4,3) for a 4:3 spread
#' @aliases
#' synthetic
#' spread 
#' synthetic.ratio
#' guaranteed_spread
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
    synthetic(primary_id=primary_id , currency=currency , multiplier=multiplier , identifiers = identifiers, members=memberlist , memberratio=memberratio, ...=... ,type=type)
}

#' @export
spread <- function(primary_id , currency , members=NULL, memberratio, ..., multiplier=1, identifiers = NULL)
{
    synthetic.ratio(primary_id , currency , members=members, memberratio=memberratio, multiplier=multiplier, identifiers = identifiers, ...=..., type=c("spread","synthetic.ratio","synthetic","instrument"))
}

#' @export
guaranteed_spread <- function(primary_id , currency , members=NULL, memberratio=c(1,1), ..., multiplier=1, identifiers = NULL)
{
    if (hasArg(suffix_id)){
        suffix_id<-match.call(expand.dots=TRUE)$suffix_id  
        id<-paste(primary_id, suffix_id,sep="_")
    } else id<-primary_id 

    
    if(is.null(members) && hasArg(suffix_id)){
        #make.names uses a dot to replace illegal chars like the '-', 
        members<-unlist(strsplit(suffix_id,"[-;:_,\\.]")) # clean up the list to something we can use  
        members<-paste(primary_id,members,sep='_') # construct a member vector appropriate for a guaranteed spread
    }
    synthetic.ratio(primary_id=id , currency=currency , members=members, memberratio=memberratio, multiplier=multiplier, identifiers = NULL, ...=..., type=c("guaranteed_spread","spread","synthetic.ratio","synthetic","instrument"))
}
