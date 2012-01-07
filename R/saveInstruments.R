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


#' Save and Load all instrument definitions
#' 
#' Saves (loads) the .instrument environment to (from) disk.
#' \code{loadInstruments} will add instrument definitions that were saved to
#' your .instrument environment
#' 
#' After you have defined some instruments, you can use \code{saveInstruments}
#' to save the entire .instrument environment to disk.
#' 
#' @aliases saveInstruments loadInstruments
#' @param file_name What to name the file (name of file that holds a
#' .instrument enviroment) Does not include file extension.
#' @param dir Directory of file (defaults to current working directory. ie. "")
#' @param extension File extension of file. default is RData. This will be ignored if 
#' \code{file_name} ends with either \sQuote{.rda} or \sQuote{.RData}.
#' @return Called for side-effect
#' @author Garrett See
#' @seealso save, load load.instrument define_stocks, define_futures,
#' define_options (option_series.yahoo)
#' @examples
#' 
#' \dontrun{
#' stock("SPY","USD",1)
#' saveInstruments()
#' loadInstruments()
#' }
#' @export 
#' @rdname saveInstruments
saveInstruments <- function(file_name="MyInstruments", dir="", extension="RData") {
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
	#.instrument <- get('FinancialInstrument:::.instrument', pos=.GlobalEnv)
    .instrument <- FinancialInstrument:::.instrument
    ssfn <- strsplit(file_name, "\\.")[[1]]
    if(any(tail(ssfn, 1) == c("rda", "RData"))) {
        file_name <- paste(ssfn[1:(length(ssfn)-1)], collapse=".")
        extension <- tail(ssfn, 1)
    }
    save(.instrument,file=paste(dir,file_name,".",extension,sep=""))	
}

#' @export
#' @rdname saveInstruments
loadInstruments <-function(file_name="MyInstruments", dir="", extension="RData") {
    require("utils")
	if (!is.null(dir) && !dir == "" && substr(dir,nchar(dir),nchar(dir)) != "/")
		dir <- paste(dir,"/",sep="")
    tmpenv <- new.env()
    ssfn <- strsplit(file_name, "\\.")[[1]]
    if(any(tail(ssfn, 1) == c("rda", "RData"))) {
        file_name <- paste(ssfn[1:(length(ssfn)-1)], collapse=".")
        extension <- tail(ssfn, 1)
    }
	load(paste(dir,file_name,".",extension,sep=""),envir=tmpenv)
    .instrument <- FinancialInstrument:::.instrument
    il <- ls(tmpenv$.instrument,all.names=TRUE)
    for (i in il) {
         assign(i, tmpenv$.instrument[[i]], pos=.instrument, inherits=FALSE)
    }
}


