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


#' load instrument metadata into the .instrument environment
#' 
#' This function will load instrument metadata (data about the data)
#' either from a file specified by the \code{file} argument or
#' from a \code{data.frame} specified by the \code{metadata} argument.
#' 
#' The function will attempt to make reasonable assumptions about what you're trying to do, but this isn't magic.
#' 
#' You will typically need to specify the \code{type} of instrument to be loaded, failure to do so will generate a Warning and \code{default_type} will be used.
#' 
#' You will need to specify a \code{primary_id}, or define a \code{id_col} that contains the data to be used as the primary_id of the instrument.
#' 
#' You will need to specify a \code{currency}, unless the instrument \code{type} is 'currency'
#' 
#' Typically, columns will exist for \code{multiplier} and \code{tick_size}.
#' 
#' Any other columns necessary to define the specified instrument type will also be required to avoid Errors.  
#' 
#' Additional columns will be processed, either as additional identifiers for recognized identifier names, or as custom feilds.  See \code{\link{instrument}} for more information on custom fields.
#' 
#' @param file string identifying file to load, default NULL, see Details
#' @param ... any other passthru parameters
#' @param metadata optional, data.frame containing metadata, default NULL, see Details
#' @param id_col numeric column containing id if primary_id isn't defined, default 1
#' @param default_type character string to use as instrument type fallback, see Details
#' @seealso instrument
#' @export
load.instruments <- function (file=NULL, ..., metadata=NULL, id_col=1, default_type='stock') {

    if(is.null(file) && is.null(metadata)) stop("You must pass either a file identifier string or a metadata object to be converted.")
    if(is.null(metadata)){
        if (file.exists(file)){
            filedata<-read.csv(file,stringsAsFactors=FALSE)
        } else {
            stop("The specified file",file,"does not seem to exist, maybe specify the full path?")
        }        
    } else {
        filedata<-metadata
        rm(metadata)
    }
    
    # check required column headers
    if(!any(grepl('primary_id',colnames(filedata)))) {
        #no primary_id column name, use id_col as the name
        set_primary<-TRUE
    } else {
        set_primary<-FALSE
    }
    if(!any(grepl('type',colnames(filedata)))) {
        warning("metadata does not appear to contain instrument type, using",default_type,". This may produce incorrect valuations.")
        filedata$type<-rep(default_type,nrow(filedata))
    }
    
    #now process the data
    for(rn in 1:nrow(filedata)){
        if(!isTRUE(is.instrument(getInstrument(as.character(filedata[rn,id_col]))))){
            arg<-as.list(filedata[rn,])
            arg$type<-NULL
            arg<-arg[!is.na(arg)]
            arg<-arg[!arg==""]
            if (set_primary) arg$primary_id<-filedata[rn,id_col]
            try(do.call(as.character(filedata[rn,'type']),arg))
        } else {
            warning(filedata[rn,id_col],"already exists in the .instrument environment")
        }
    } 
            
}
