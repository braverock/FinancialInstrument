

#' generate endpoints for volume bars
#' @param x time series containing 'Volume' column
#' @param units volume sum to mark for bars
#' @author Joshua Ulrich
#' @export
volep <- function (x, units){
    incepSum <- runSum(Vo(x),1,TRUE)                               
    bins <- incepSum %/% units
    bins[1]<-0
    ep<-which(diff(bins)!=0)
    return(ep)
}

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
