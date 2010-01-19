###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2010
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id:$
#
###############################################################################

synthetic <- function(primary_id , currency , multiplier=NULL, identifiers = NULL, ..., members=NULL){
    synthetic_temp = instrument(primary_id , currency , multiplier , identifiers = identifiers, ..., type="synthetic" )
    if (type=="synthetic") cl<-c("synthetic","instrument")
    else cl <- c(type,"synthetic", "instrument")
    ## now structure and return
    return(structure( list(primary_id = synthetic_temp$primary_id,
                            currency = synthetic_temp$currency,
                            multiplier = synthetic_temp$multiplier,
                            identifiers = synthetic_temp$identifiers,
                            members = members 
                    ),
                    class=cl )
           )
}

synthetic.ratio <- function(primary_id , currency , multiplier=NULL, identifiers = NULL, ..., type="synthetic.ratio", members, memberratio){
    if(!is.list(members)){
        if(length(members)!=length(memberratios) | length(members)<2){
            stop("length of members and memberratio must be equal, and contain two or more instruments")
        } else {
            memberlist<-list(members=members,memberratio=memberratio,memberpositions=NULL)   
        }
    } else {
        # TODO do some sanity checking here on the list elements
          memberlist=members
    }
    synthetic_temp = synthetic(primary_id , currency , multiplier , identifiers = identifiers, ..., members, type=type )
    ## now structure and return
    assign(primary_id, structure( list(primary_id = synthetic_temp$primary_id,
                            currency = synthetic_temp$currency,
                            multiplier = synthetic_temp$multiplier,
                            identifiers = synthetic_temp$identifiers,
                            members = memberlist
                    ),
                    class=class(synthetic_temp)
            ), # end structure
            envir=.instrument,inherits=TRUE
    )
}

spread <- function(primary_id , currency , multiplier=NULL, identifiers = NULL, ..., members, memberratio){
    synthetic.ratio(primary_id , currency , multiplier=NULL, identifiers = NULL, ..., type=c("spread","synthetic.ratio"), members, memberratios)
}
