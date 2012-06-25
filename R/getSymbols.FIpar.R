#' getSymbols method for loading data from split files
#' 
#' This is \code{\link[FinancialInstrument]{getSymbols.FI}} adapted to use
#' \code{foreach}.  If a parallel backend has been registered (with, e.g.
#' \code{doMC:::registerDoMC} or \code{doSMP:::registerDoSMP},) it will get multiple symbols
#' in parallel.
#'
#' Meant to be called internally by \code{\link[quantmod]{getSymbols}} .
#' 
#' If date_format is NULL (the Default), we will assume an ISO date as changed by \code{\link{make.names}}, 
#' for example, 2010-12-01 would be assumed to be a file containing 2010.12.01
#' 
#' If auto.assign is FALSE, you'll only get the last Symbol in the symbols list returned, 
#' so only use \code{auto.assign=FALSE} with a single symbol.  Patches Welcome.
#'  
#' @param Symbols a character vector specifying the names of each symbol to be loaded
#' @param from Retrieve data no earlier than this date. Default '2010-01-01'.
#' @param to Retrieve data through this date. Default Sys.Date().
#' @param ... any other passthru parameters
#' @param dir if not specified in getSymbolLookup, directory string to use.  default ""
#' @param return.class only "xts" is currently supported
#' @param extension file extension, default "rda"
#' @param split_method string specifying the method used to split the files, currently \sQuote{days} 
#'   or \sQuote{common}, see \code{\link[FinancialInstrument]{setSymbolLookup.FI}}
#' @param use_identifier optional. identifier used to construct the \code{primary_id} of the instrument. 
#'   If you use this, you must have previously defined the \code{\link[FinancialInstrument]{instrument}} 
#' @param date_format format as per the \code{\link{strptime}}, see Details
#' @param verbose TRUE/FALSE
#' @param days_to_omit character vector of names of weekdays that should not be loaded.  
#'      Default is \code{c("Saturday", "Sunday")}.  Use \code{NULL} to attempt to load data 
#'      for all days of the week.
#' @param indexTZ valid TZ string. (e.g. \dQuote{America/Chicago} or \dQuote{America/New_York})
#'      See \code{\link[xts]{indexTZ}}.
#' @param timespan xts-style intraday subsetting string. e.g. \dQuote{T08:30:00/T15:00:00}.
#'      If \code{indexTZ} is provided, timezone conversion will occur before
#'      the time of day subsetting. \code{timespan} is only used if 
#'      \code{split_method} is \dQuote{days}.
#' @seealso 
#' \code{\link[quantmod]{getSymbols}}
#' \code{\link{getSymbols.FI}}
#' \code{\link[FinancialInstrument]{saveSymbols.days}}
#' \code{\link[FinancialInstrument]{instrument}}
#' \code{\link[FinancialInstrument]{setSymbolLookup.FI}}
#' \code{\link[FinancialInstrument]{load.instruments}}
#' \code{\link[quantmod]{getSymbols}}
#' @export
getSymbols.FIpar <- function(Symbols,
                            from='2010-01-01',
                            to=Sys.Date(),
                            ..., 
                            dir="",
                            return.class="xts",
                            extension="rda",
                            split_method = c("days", "common"),
                            use_identifier = NA,
                            date_format=NULL,
                            verbose=TRUE,
                            days_to_omit=c("Saturday", "Sunday"),
                            indexTZ,
                            timespan
                         )
{
    require(foreach)
    if (is.null(date_format)) date_format <- "%Y.%m.%d"
    if (is.null(days_to_omit)) days_to_omit <- 'NULL'
    importDefaults("getSymbols.FIpar")
    this.env <- environment()
    for(var in names(list(...))) {
        assign(var,list(...)[[var]], this.env)
    }
    
    #The body of the following function comes from Dominik's answer here: 
    #browseURL{"http://stackoverflow.com/questions/7224938/can-i-rbind-be-parallelized-in-r"}
    #it does what do.call(rbind, lst) would do, but faster and with less memory usage
    do.call.rbind <- function(lst) {
        while(length(lst) > 1) {
            idxlst <- seq(from=1, to=length(lst), by=2)

            lst <- lapply(idxlst, function(i) {
                if(i==length(lst)) { return(lst[[i]]) }

                return(rbind(lst[[i]], lst[[i+1]]))
            })
        }
        lst[[1]]
    }

    # Find out if user provided a value for each formal
    hasArg.from <- hasArg(from)
    hasArg.to <- hasArg(to)
    hasArg.dir <- hasArg(dir)
    hasArg.return.class <- hasArg(return.class)
    hasArg.extension <- hasArg(extension)
    hasArg.split_method <- hasArg(split_method)
    hasArg.use_identifier <- hasArg(use_identifier)
    hasArg.date_format <- hasArg(date_format)
    hasArg.verbose <- hasArg(verbose)
    hasArg.days_to_omit <- hasArg(days_to_omit)
    hasArg.indexTZ <- hasArg(indexTZ)
    hasArg.timespan <- hasArg(timespan)

    # Now get the values for each formal that we'll use if not provided
    # by the user and not found in the SymbolLookup table
    default.from <- from
    default.to <- to
    default.dir <- dir
    default.return.class <- return.class
    default.extension <- extension
    default.split_method <- split_method[1]
    default.use_identifier <- use_identifier
    default.date_format <- date_format
    default.verbose <- verbose
    default.days_to_omit <- days_to_omit
    default.indexTZ <- if (hasArg.indexTZ) {
        default.indexTZ <- indexTZ 
    } else NA
    default.timespan <- if (hasArg.timespan) {
        default.timespan <- timespan 
    } else ""


    # quantmod:::getSymbols will provide auto.assign and env
    # so the next 2 if statements should always be TRUE
    auto.assign <- if(hasArg(auto.assign)) {auto.assign} else TRUE
    env <- if(hasArg(env)) {env} else .GlobalEnv 
    
    # make an argument matching function to sort out which values to use for each arg
    pickArg <- function(x, Symbol) {
        if(get(paste('hasArg', x, sep="."))) {
            get(x)
        } else if(!is.null(SymbolLookup[[Symbol]][[x]])) {
            SymbolLookup[[Symbol]][[x]]
        } else get(paste("default", x, sep="."))
    }
    
    SymbolLookup <- getSymbolLookup()
    i <- NULL
    fr<-NULL
    datl <- foreach(i = icount(length(Symbols))) %dopar% {
        dir <- default.dir # I don't know why I have to do this!!!
#        timespan <- default.timespan
        from <- pickArg("from", Symbols[[i]])
        to <- pickArg("to", Symbols[[i]])
        dir <- pickArg("dir", Symbols[[i]])
        return.class <- pickArg("return.class", Symbols[[i]])
        extension <- pickArg('extension', Symbols[[i]])
        split_method <- pickArg('split_method', Symbols[[i]])
        use_identifier <- pickArg('use_identifier', Symbols[[i]])
        date_format <- pickArg('date_format', Symbols[[i]])
        verbose <- pickArg('verbose', Symbols[[i]])
        days_to_omit <- pickArg('days_to_omit', Symbols[[i]])
        indexTZ <- pickArg('indexTZ', Symbols[[i]])
        timespan <- pickArg('timespan', Symbols[[i]])
        # if 'dir' is actually the 'base_dir' then we'll paste the instrument name (Symbol) to the end of it.
        # First, find out what the instrument name is
        instr_str <- NA
        if(!is.na(use_identifier)) { 
            tmp_instr <- try(getInstrument(Symbols[[i]], silent=FALSE))
            if (inherits(tmp_instr,'try-error') || !is.instrument(tmp_instr)) 
                stop("must define instrument first to call with 'use_identifier'")
            if (!use_identifier=='primary_id') {
                instr_str<-make.names(tmp_instr$identifiers[[use_identifier]])
            } else  instr_str <- make.names(tmp_instr[[use_identifier]])
        }
        Symbol <- ifelse(is.na(instr_str), make.names(Symbols[[i]]), instr_str)
        ndc<-nchar(dir)
        if(substr(dir,ndc,ndc)=='/') dir <- substr(dir,1,ndc-1) #remove trailing forward slash
        ssd <- strsplit(dir,"/")[[1]]
        if (identical(character(0), ssd) || (!identical(character(0), ssd) && ssd[length(ssd)] != Symbol)) dir <- paste(dir,Symbol,sep="/")
        
        if(!dir=="" && !file.exists(dir)) {
            if (verbose) cat("\ndirectory ",dir," does not exist, skipping\n")
        } else {
            if(verbose) cat("loading ",Symbols[[i]],".....\n")
            switch(split_method[1],
                days={
                    StartDate <- as.Date(from) 
                    EndDate <- as.Date(to) 
                    date.vec <- as.Date(StartDate:EndDate)
                    date.vec <- date.vec[!weekdays(date.vec) %in% days_to_omit]  
                    date.vec <- format(date.vec, format=date_format)
                    sym.files <- paste(date.vec, Symbol, extension, sep=".")
                    if (dir != "") sym.files <- file.path(dir, sym.files)
                    tmpenv <- new.env()
                        dl <- lapply(sym.files, function(fp) {
                            sf <- strsplit(fp, "/")[[1]]
                            sf <- sf[length(sf)]
                            if (verbose) cat('Reading ', sf, '...')
                            if (!file.exists(fp)) {
                                if (verbose) cat(' failed. File not found in ', dir, ' ... skipping\n')
                            } else {
                                if(verbose) cat(' done.\n')
                                local.name <- load(fp)
                                #as.zoo(get(local.name))
                                dat <- get(local.name)
                                if (!is.na(indexTZ) && !is.null(dat)) indexTZ(dat) <- indexTZ
                                dat[timespan]
                            }
                        })
                        if (verbose) cat('rbinding data ... \n')
                        fr <- do.call.rbind(dl)                        
                        #fr <- do.call(rbind, dl)
                        #fr <- as.xts(fr)
                    },
                common = , {
                    sym.file <- paste(Symbol,extension,sep=".")
                    if(dir != "") sym.file <- file.path(dir, sym.file)
                    if(!file.exists(sym.file)) {
                        if (verbose) cat("file ",paste(Symbol,extension,sep='.')," does not exist in ",dir,"....skipping\n")
                    } else {
                        #fr <- read.csv(sym.file)
                        local.name <- load(sym.file)
                        dat <- get(local.name)
                        if (!is.na(indexTZ) && !is.null(dat)) indexTZ(dat) <- indexTZ
                        assign('fr', dat[timespan])
                        if(verbose) cat("done.\n")
                        #if(!is.xts(fr)) fr <- xts(fr[,-1],as.Date(fr[,1],origin='1970-01-01'),src='rda',updated=Sys.time())
                    }
                } # end 'common'/default method (same as getSymbols.rda)               
            ) # end split_method switch
            # if each file has a different value for an xtsAttribute, only the most recent is used, so fix them here
#            if (!is.null(xtsAttributes(fr)$from)) 
#                xtsAttributes(fr)$from <- from
#            if (!is.null(xtsAttributes(fr)$to)) 
#                xtsAttributes(fr)$to <- to
            fr <- quantmod:::convert.time.series(fr=fr,return.class=return.class)
            #if (!is.na(indexTZ)) indexTZ(fr) <- indexTZ
            Symbols[[i]] <-make.names(Symbols[[i]]) 
            tmp <- list()
            tmp[[Symbols[[i]]]] <- fr
            if(verbose) cat("done.\n")
            tmp     
        }
    } #end loop over Symbols
    
    if (length(Filter("+", lapply(datl, length))) == 0) {
        warning("No data found.")
        return(NULL) 
    }

    datl.names <- do.call(c, lapply(datl, names))
    missing <- Symbols[!Symbols %in% datl.names]
    if (length(missing) > 0) warning(paste('No data found for', missing))
    if(auto.assign) {
        #invisible(lapply(datl, function(x) if (length(x) > 0) assign(names(x), x[[1]], pos=env)))
        out <- Filter(function(x) length(x) > 0, datl)
        invisible(lapply(out, function(x) assign(names(x), x[[1]], pos=env)))
        return(datl.names)
    } else {
        #NOTE: Currently, NULLs aren't filtered out.  If there are data for any Symbol,
        # the returned list will have an element for each symbol requested even if some don't contain data.
        out <- lapply(datl, function(x) {
            if (length(x) > 0) x[[1]]
        })
        if (length(out) == 1)
            return(out[[1]])
        else {
            # Filter(function(x) dim(x)[1] > 0, out)
            names(out) <- Symbols
            return(out)
        }
    }
    
}

