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
synthetic <- function(primary_id , currency , multiplier=1, identifiers = NULL, ..., members=NULL, cl=c("synthetic", "instrument"))
{

    synthetic_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ..., type=NULL )
    
    ## now structure and return
    return(structure( list(primary_id = synthetic_temp$primary_id,
                            currency = synthetic_temp$currency,
                            multiplier = synthetic_temp$multiplier,
                            identifiers = synthetic_temp$identifiers,
                            memberlist = members 
                    ),
                    class=cl )
           )
}

#' constructors for synthetic instruments
#' @param primary_id string describing the unique ID for the instrument
#' @param currency string describing the currency ID of an object of type \code{\link{currency}}
#' @param multiplier numeric multiplier to apply to the price in the instrument currency to get to notional value
#' @param identifiers character vector of any other identifiers that should also be stored for this instrument
#' @param ... any other passthru parameters 
#' @param cl class string, should not be set by users
#' @param members character vector of instrument identifiers that make up the synthetic
#' @param memberratio numeric vector of ratio relationchips between members, e.g. c(4,3) for a 4:3 spread
#' @aliases
#' synthetic
#' spread 
#' synthetic.ratio
#' @export
synthetic.ratio <- function(primary_id , currency , multiplier=1, identifiers = NULL, ..., cl=c("synthetic.ratio","synthetic","instrument"), members, memberratio)
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
    synthetic_temp = synthetic(primary_id , currency , multiplier=multiplier , identifiers = identifiers, members=members , memberratio=memberratio, ...  )
    ## now structure and return
    assign(primary_id, structure( list(primary_id = synthetic_temp$primary_id,
                            currency = synthetic_temp$currency,
                            multiplier = synthetic_temp$multiplier,
                            identifiers = synthetic_temp$identifiers,
                            memberlist = memberlist
                    ),
                    class=cl
            ), # end structure
            envir=as.environment(.instrument)
    )
}

#' @export
spread <- function(primary_id , currency , members, memberratio, ..., multiplier=1, identifiers = NULL)
{
    synthetic.ratio(primary_id , currency , multiplier=multiplier, identifiers = NULL, cl=c("spread","synthetic.ratio","synthetic","instrument"), members=members, memberratio=memberratio, ...=...)
}
