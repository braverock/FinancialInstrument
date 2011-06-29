#' construct a series of symbols based on root symbol and suffix letters
#'  
#' The columns needed by this version of the function are \code{primary_id} 
#' and \code{month_cycle}. \code{primary_id} should match the \code{primary_id} 
#' of the instrument describing the root contract. 
#' \code{month_cycle} should contain a comma delimited string describing the 
#' month sequence to use, e.g. \code{"F,G,H,J,K,M,N,Q,U,V,X,Z"} for all months
#' using the standard futures letters, or \code{"H,M,U,Z"} for quarters, or 
#' \code{"Mar,Jun,Sep,Dec"} for quarters as three-letter month abbreviations, etc.  
#' The correct values will vary based on your data source.
#' 
#' @param yearlist vector of year suffixes to be applied, see Details
#' @param roots data.frame containing ate least columns \code{primary_id} and \code{month_cycle}, see Details
#' @returnType 
#' @return 
#' @author Brian G. Peterson
#' @export
#' TODO: add more flexibility in input formats for \code{roots}
build_series_symbols <- function(roots, yearlist=c(0,1)) {
	symbols<-''
	id_col<-grep('primary_id',colnames(roots)) #TODO: check length
	date_col<-grep('month_cycle',colnames(roots)) #TODO: check length
	for (year_code in yearlist){
		for(i in 1:nrow(roots)) { 
			symbols <- c(symbols, paste(paste(roots[i,id_col], strsplit(as.character(roots[i,date_col]),",")[[1]],sep=''),year_code,sep='')) 
		}
	}
	return(symbols[-1])
}

###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2011
# Peter Carl, Lance Levenson, Brian G. Peterson 
#
# This code is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

