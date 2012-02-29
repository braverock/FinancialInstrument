###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2012
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, 
# Joshua Ulrich, Brian G. Peterson, and Garrett See
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################


#' Construct a hierarchy of instruments useful for aggregation
#'
#' All 'currency' instruments must be defined before instruments of other types 
#' may be defined.
#'
#' In \dots you may pass any other arbitrary instrument fields that will be used 
#' to create 'custom' fields. (This is not yet implemented)
#'
#' @param primary_ids A character vector of \code{instrument} primary_ids to be 
#' included in the hierarchy list
#' @param levels A character vector of instrument attributes in top-down order
#' @param ... not in use
#' @author Peter Carl, Alexis Petit, Garrett See
#' @return Constructs a data.frame that contains the list of assets in the first 
#' column and the category or factor for grouping at each level in the following 
#' columns
#' @seealso \code{\link{instrument.table}}
# TODO add a link to PortfolioAnalytics attribution functions, when they exist
#' @export
buildHierarchy <- function(primary_ids, levels, ...) {
    out <- data.frame(primary_ids, stringsAsFactors=FALSE)
    ilist <- lapply(primary_ids, getInstrument)
    for (level in levels) {
        tmp_level <- as.character(lapply(1:length(primary_ids), 
                                         function(x) ilist[[x]][[level]]))
        out <- cbind(out, tmp_level, stringsAsFactors=FALSE)
    }
    colnames(out) <- c("primary_id", levels)
    return(out)
}
