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


#' Construct a hierarchy of instruments useful for aggregation
#'
#' All 'currency' instruments must be defined before instruments of other types may be defined.
#'
#' In \dots you may pass any other arbitrary instrument fields that will be used to create 'custom' fields.
#'
#' @param primary_ids A list of assets to be included in the hierarchy list
#' @param levels A list of instrument attributes in top-down order
#' @param ... any other passthru parameters
#' @author Peter Carl
#' @return Constructs a data.frame that contains the list of assets in the first column and the category or factor for grouping at each level in the following columns
# @seealso
# TODO add a link to PortfolioAnalytics attribution functions, when they exist
#' @export
buildHierarchy <- function(primary_ids, levels, ...) {
    out = data.frame(NA)
    primary_ids = make.names(primary_ids)
    for(primary_id in primary_ids) {
        tmp_instr = try(getInstrument(primary_id))
        # TODO finish error checking
        # for each instrument, create a vector of factors at each level
        attrs = NA
        for(level in levels){
            attr = unname(tmp_instr[eval(level)])
            if(!is.na(attrs))
                attrs = cbind(attrs, attr)
            else
                attrs = attr
        }
        if(!is.na(out[1]))
            out = rbind(out, attrs)
        else
            out = attrs
    }
    out = cbind(primary_ids, out)
    colnames(out) = c("primary_id", levels)
    out

}
