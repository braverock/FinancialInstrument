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
#' Any other columns necessary to define the specified instrument type will also be required to avoid fatal Errors.  
#' 
#' Additional columns will be processed, either as additional identifiers for recognized identifier names, or as custom fields.  See \code{\link{instrument}} for more information on custom fields.
#' 
#' @param file string identifying file to load, default NULL, see Details
#' @param ... any other passthru parameters
#' @param metadata optional, data.frame containing metadata, default NULL, see Details
#' @param id_col numeric column containing id if primary_id isn't defined, default 1
#' @param default_type character string to use as instrument type fallback, see Details
#' @seealso 
#' \code{\link{instrument}} 
#' \code{\link{setSymbolLookup.FI}} 
#' \code{\link[quantmod]{getSymbols}} 
#' \code{\link{getSymbols.FI}}
#' @export
load.instruments <- function (file=NULL, ..., metadata=NULL, id_col=1, default_type='stock') {

    if(is.null(file) && is.null(metadata)) stop("You must pass either a file identifier string or a metadata object to be converted.")
    if(is.null(metadata)){
        if (file.exists(file)){
            filedata<-read.csv(file,stringsAsFactors=FALSE, ...=...)
        } else {
            stop("The specified file ",file," does not seem to exist, maybe specify the full path?")
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
        warning("metadata does not appear to contain instrument type, using ",default_type,". This may produce incorrect valuations.")
        filedata$type<-rep(default_type,nrow(filedata))
    }
    dotargs<-list('...')
    
    #now process the data
    for(rn in 1:nrow(filedata)){
        if(!isTRUE(is.instrument(try(getInstrument(as.character(filedata[rn,id_col]),silent=TRUE),silent=TRUE)))){
            type=as.character(filedata[rn,'type'])
            arg<-as.list(filedata[rn,])
            if(type=='spread' || type=='guaranteed_spread'){
				if(!is.null(arg$members)){
					arg$members<-unlist(strsplit(arg$members,','))
				}
				if(!is.null(arg$memberratio)){
					arg$memberratio<-unlist(strsplit(arg$memberratio,','))
				}
				if(!is.null(arg$ratio)){
					arg$memberratio<-unlist(strsplit(arg$ratio,','))
				}
			}
            arg$type<-NULL
            arg<-arg[!is.na(arg)]
            arg<-arg[!arg==""]
            if (set_primary) {
                arg$primary_id<-filedata[rn,id_col]
            }
            
            #do some name cleanup to make up for Reuters silliness
            if(substr(arg$primary_id,1,1)==1) arg$primary_id <- substr(arg$primary_id,2,nchar(arg$primary_id))
            arg$primary_id<-make.names(arg$primary_id)
            if(!is.null(arg$X.RIC)){
                if(substr(arg$X.RIC,1,1)==1) arg$X.RIC <- substr(arg$X.RIC,2,nchar(arg$X.RIC))
            }            
            if(!is.null(arg$RIC)){
                if(substr(arg$RIC,1,1)==1) arg$RIC <- substr(arg$RIC,2,nchar(arg$RIC))
            }            
            if(length(dotargs)) args<-c(args,dotargs)
            
            if(is.function(try(match.fun(type),silent=TRUE))){
                out <- try(do.call(type,arg))
                #TODO recover gracefully?
            } else {
                # the call for a function named for type didn't work, so we'll try calling instrument as a generic
				type=c(type,"instrument")
				arg$type<-type # set the type
                arg$assign_i<-TRUE # assign to the environment
				try(do.call("instrument",arg))
			}
        } else {   
            warning(filedata[rn,id_col]," already exists in the .instrument environment")
        } # end instrument check
    } # end loop on rows
}

#' set quantmod-style SymbolLookup for instruments
#' 
#' This function exists to tell \code{\link[quantmod]{getSymbols}} where to look for your repository of market data.
#' 
#' The \code{base_dir} parameter \emph{must} be set or the function will fail.  
#' This will vary by your local environment and operating system.  For mixed-OS environments,
#' we recommend doing some OS-detection and setting the network share to your data to a common 
#' location by operating system.  For example, all Windows machines may use \dQuote{M:/} 
#' and all *nix-style (linux, Mac) machines may use \dQuote{/mnt/mktdata/}. 
#' 
#' The \code{split_method} currently allows either \sQuote{days} or \sQuote{common}, and expects the 
#' file or files to be in sub-directories named for the symbol.  In high frequency data, it is standard practice to split
#' the data by days, which is why that option is the default.
#'     
#' @param base_dir string specifying the base directory where data is stored, see Details 
#' @param \dots any other passthru parameters
#' @param storage_method currently only \sQuote{rda}, but we will eventually support \sQuote{indexing} at least, and maybe others
#' @param split_method string specifying the method files are split, currently \sQuote{days} or \sQuote{common}, see Details
#' @param use_identifier string identifying which column should be use to construct the \code{primary_id} of the instrument, default 'primary_id'
#' @param extension file extension, default "rda"
#' @param src which \code{\link[quantmod]{getSymbols}} sub-type to use, default \code{\link{getSymbols.FI}} by setting 'FI'
#' @seealso \code{\link{load.instruments}} 
#' \code{\link{getSymbols.FI}}
#' \code{\link{load.instruments}}
#' \code{\link[quantmod]{setSymbolLookup}}
#' @export
setSymbolLookup.FI<-function(base_dir,..., split_method=c("days","common"), storage_method='rda', use_identifier='primary_id', extension='rda', src='FI'){
    # check that base_dir exists
    if(!file.exists(base_dir)) stop('base_dir ',base_dir,' does not seem to specify a valid path' )
    
    # take split
    split_method<-split_method[1] # only use the first value
    
    #load all instrument names
    instr_names<-ls(pos=.instrument)
    
    #TODO add check to make sure that src is actually the name of a getSymbols function
    
    #initialize list
    params<-list()
    params$storage_method<-storage_method
    params$extension<-extension
    params$split_method<-split_method
    params$src<-src
    if(length(list(...))>=1){
        dlist<-list(...)
        params<-c(params,dlist)
    }
    new.symbols<-list()
    ndc<-nchar(base_dir)
    if(substr(base_dir,ndc,ndc)=='/') sepch='' else sepch='/'
    for (instr in instr_names){
        tmp_instr<-getInstrument(instr)
        if(!use_identifier=='primary_id'){
            instr_str<-make.names(tmp_instr$identifiers[[use_identifier]])
        } else {
            instr_str<-make.names(tmp_instr[[use_identifier]])
        } 
        if(!is.null(instr_str)) instr<-instr_str
        symbol<-list()
        symbol[[1]]<-params
        # construct $dir
        symbol[[1]]$dir<-paste(base_dir,instr_str,sep=sepch)
        names(symbol)[1]<-instr
        new.symbols<-c(new.symbols,symbol)
    }
    setSymbolLookup(new.symbols)
}

#' getSymbols method for loading data from split files
#' 
#' This function should probably get folded back into getSymbols.rda in quantmod.
#' 
#' Meant to be called internally by \code{\link[quantmod]{getSymbols}} .
#' 
#' The symbol lookup table will most likely be loaded by \code{\link{setSymbolLookup.FI}}
#' 
#' If date_format is NULL (the Default), we will assume an ISO date as changed by \code{\link{make.names}}, 
#' for example, 2010-12-01 would be assumed to be a file containing 2010.12.01
#'  
#' @param Symbols a character vector specifying the names of each symbol to be loaded
#' @param from Retrieve data no earlier than this date. Default '2010-01-01'.
#' @param to Retrieve data through this date. Default Sys.Date().
#' @param ... any other passthru parameters
#' @param env where to create objects. Default .GlobalEnv
#' @param dir if not specified in getSymbolLookup, directory string to use.  default ""
#' @param return.class only "xts" is currently supported
#' @param extension file extension, default "rda"
#' @param split_method string specifying the method used to split the files, currently \sQuote{days} or \sQuote{common}, see \code{\link{setSymbolLookup.FI}}
#' @param use_identifier optional. identifier used to construct the \code{primary_id} of the instrument. If you use this, you must have previously defined the \code{\link{instrument}} 
#' @param date_format format as per the \code{\link{strptime}}, see Details
#' @param verbose TRUE/FALSE
#' @param auto.assign TRUE/FALSE
#' @seealso 
#' \code{\link{instrument}}
#' \code{\link{setSymbolLookup.FI}}
#' \code{\link{load.instruments}}
#' \code{\link[quantmod]{getSymbols}}
#' 
#' @export
getSymbols.FI <- function(Symbols,
                            from='2010-01-01',
                            to=Sys.Date(),
                            ..., 
                            env,
                            dir="",
                            return.class="xts",
                            extension="rda",
                            split_method = c("days", "common"),
                            use_identifier,
                            date_format=NULL,
							verbose=TRUE,
							auto.assign=TRUE
                         ) 
{
    if(is.null(date_format)) date_format<-"%Y.%m.%d"
    importDefaults("getSymbols.FI")
    this.env <- environment()
    for(var in names(list(...))) {
        assign(var,list(...)[[var]], this.env)
    }
    
    default.return.class <- return.class
    default.dir <- dir
    default.extension <- extension
    default.split_method <- split_method
       
    for(i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class),default.return.class, return.class)
        dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
        dir <- ifelse(is.null(dir),default.dir, dir)
        #if 'dir' is actually the 'base_dir' then we'll paste the instrument name (Symbol) to the end of it 
        #first, find out what the instrument name is
        instr_str <- NA
        if(!missing(use_identifier)) { 
            tmp_instr <- try(getInstrument(Symbols[[i]], silent=FALSE))
            if (inherits(tmp_instr,'try-error') || !is.instrument(tmp_instr)) 
                stop("must define instrument first to call with 'use_identifier'")
            if (!use_identifier=='primary_id') {
                instr_str<-make.names(tmp_instr$identifiers[[use_identifier]])
            } else  instr_str <- make.names(tmp_instr[[use_identifier]])
        }
        symbol <- ifelse(is.na(instr_str), make.names(Symbols[[i]]), instr_str)
        ndc<-nchar(dir)
        if(substr(dir,ndc,ndc)=='/') dir <- substr(dir,1,ndc-1) #remove trailing forward slash
        ssd <- strsplit(dir,"/")[[1]]
        if (ssd[length(ssd)] != symbol) dir <- paste(dir,symbol,sep="/")

        if(!dir=="" && !file.exists(dir)) {
            cat("\ndirectory ",dir," does not exist, skipping\n")
            next
        }
        extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
        extension <- ifelse(is.null(extension),default.extension, extension)
        if(verbose) cat("loading ",Symbols[[i]],".....")
        split_method <- getSymbolLookup()[[Symbols[[i]]]]$split_method
        split_method <- ifelse(is.null(split_method), default.split_method, split_method)

        switch(split_method,
                days={
                    fr<-NULL
                    StartDate <- as.Date(from) 
                    EndDate <- as.Date(to) 
                    date.vec <- as.Date(StartDate:EndDate)
                    
                    for(d in date.vec) {
                        if(weekdays(as.Date(d)) == "Saturday" || weekdays(as.Date(d)) == "Sunday"){next}
                        d<-format(as.Date(d),format=date_format)
                        if(dir=="") {
                            sym.file <- paste(d,Symbols[[i]],extension,sep=".")
                        } else {
                            sym.file <- file.path(dir,paste(d,Symbols[[i]],extension,sep="."))
                        }
                        if(!file.exists(sym.file)) {
                            cat("\nfile ",paste(d,Symbols[[i]],extension,sep='.')," does not exist in ",dir,"....skipping\n")
                            next
                        }
                        local.name <- load(sym.file)
                        if(!is.null(fr)) {
                            fr<-rbind(fr,get(local.name))
                        } else assign('fr',get(local.name))
                        rm(local.name)
                    } # end date loop
                },
                common = , {
                    if(dir=="") {
                        sym.file <- paste(Symbols[[i]],extension,sep=".")
                    } else {
                        sym.file <- file.path(dir,paste(Symbols[[i]],extension,sep="."))
                    }
                    if(!file.exists(sym.file)) {
                        cat("\nfile ",paste(Symbols[[i]],extension,sep='.')," does not exist in ",dir,"....skipping\n")
                        next
                    }
                    #fr <- read.csv(sym.file)
                    local.name <- load(sym.file)
                    assign('fr',get(local.name))
                    if(verbose) cat("done.\n")
                    #if(!is.xts(fr)) fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='rda',updated=Sys.time())
                } # end 'common'/default method (same as getSymbols.rda)    
        ) # end split_method switch
        fr <- quantmod:::convert.time.series(fr=fr,return.class=return.class)
        Symbols[[i]] <-make.names(Symbols[[i]]) 
        if(auto.assign) assign(Symbols[[i]],fr,env)
        if(verbose) cat("done.\n")        
    } #end loop over Symbols
    
    if(auto.assign)
        return(Symbols)
    return(fr)
}
