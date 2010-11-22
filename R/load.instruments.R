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
            type=as.character(filedata[rn,'type'])
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
			arg<-as.list(filedata[rn,])
            arg$type<-NULL
            arg<-arg[!is.na(arg)]
            arg<-arg[!arg==""]
            if (set_primary) arg$primary_id<-filedata[rn,id_col]
            out<-try(do.call(type,arg))
			if(inherits(out,"try-error")){
				type=c(type,"instrument")
				arg$type<-type
				try(do.call("instrument",args))
			}
        } else {
            warning(filedata[rn,id_col],"already exists in the .instrument environment")
        }
    } 
            
}

setSymbolLookup.FI<-function(base_dir,storage_method='rda',split_method=c("days","common")){
    # check that base_dir exists
    if(!file.exists(base_dir)) stop('base_dir ',base_dir,' does not seem to specify a valid path' )
    
    # take split
    split_method<-split_method[1] # only use the first value
    
    #load all instrument names
    instr_names<-ls(pos=.instrument)
    
    #initialize list
    params<-list()
    params$storage_method<-storage_method
    if(storage_method=='rda') params$extension<-'rda'
    params$split_method<-split_method
    params$src<-"FI"
    new.symbols<-list()
    for (instr in instr_names){
        symbol<-list()
        symbol[[1]]<-params
        # construct $dir   
        symbol[[1]]$dir<-paste(base_dir,instr,sep="/")
        names(symbol)[1]<-instr
        new.symbols<-c(new.symbols,symbol)
    }
    setSymbolLookup(new.symbols)
}

getSymbols.FI <- function(Symbols,
                            from='2010-01-01',
                            to=Sys.Date(),
                            ..., 
                            env,
                            dir="",
                            return.class="xts",
                            extension="rda"
                         ) 
{
    importDefaults("getSymbols.FI")
    this.env <- environment()
    for(var in names(list(...))) {
        assign(var,list(...)[[var]], this.env)
    }
    
    default.return.class <- return.class
    default.dir <- dir
    default.extension <- extension
    
    if(missing(verbose)) verbose <- FALSE
    if(missing(auto.assign)) auto.assign <- TRUE
    
    for(i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class),default.return.class, return.class)
        dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
        dir <- ifelse(is.null(dir),default.dir, dir)
        if(!dir=="" && !file.exists(dir)) {
            cat("\ndirectory ",dir," does not exist, skipping\n")
            next
        }
        extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
        extension <- ifelse(is.null(extension),default.extension, extension)
        if(verbose) cat("loading ",Symbols[[i]],".....")
        switch(getSymbolLookup()[[Symbols[[i]]]]$split_method,
                days={
                    fr<-NULL
                    StartDate <- as.Date(from) 
                    EndDate <- as.Date(to) 
                    date.vec <- as.Date(StartDate:EndDate)
                    
                    date.vec.ch <- as.character(date.vec)
                    
                    for(d in date.vec) {
                        if(weekdays(as.Date(d)) == "Saturday" || weekdays(as.Date(d)) == "Sunday"){next}
                        d<-format(as.Date(d),format='%Y.%m.%d')
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
                        if(!is.null(fr)) local.name<-rbind(fr,local.name)
                        assign('fr',get(local.name))                        
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
                    if(!is.xts(fr)) fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='rda',updated=Sys.time())
                } # end 'common'/default method (same as getSymbols.rda)    
        ) # end split_method switch
        fr <- quantmod:::convert.time.series(fr=fr,return.class=return.class)
        Symbols[[i]] <-make.names(Symbols[[ii]]) 
        if(auto.assign) assign(Symbols[[i]],fr,env)
        if(verbose) cat("done.\n")        
    } #end loop over Symbols
    
    if(auto.assign)
        return(Symbols)
    return(fr)
}
