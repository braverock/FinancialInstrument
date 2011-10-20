#' updates instrument metadata with data from yahoo
#' 
#' Adds/updates information in instrument with data downloaded from yahoo
#' 
#' if you call \code{update_instruments.yahoo} with one of \sQuote{all} or
#' \sQuote{stocks}, it is the same as calling it with the relevant ls_ function 
#' (e.g. \code{ls_stocks()}).  Therefore, functionality can be extended by using 
#' ls_ functions instead of a descriptive string.
#' 
#' @aliases update_instruments.yahoo update_instruments.TTR
#' @param Symbols can be a vector of instrument names, or, can be \sQuote{all}
#' or \sQuote{stocks} or, for update_instruments.TTR, can be NULL in which case
#' all stocks found with \code{stockSymbols} will be defined
#' @param exchange character vector of names of exchanges. Used in \sQuote{TTR}
#' method. Can be \dQuote{AMEX}, \dQuote{NASDAQ}, or \dQuote{NYSE}
#' @param verbose be verbose?
#' @return called for side-effect
#' @author Garrett See
#' @references Yahoo! Finance \url{finance.yahoo.com} YahooQuote
#' \url{http://dirk.eddelbuettel.com/code/yahooquote.html} 
#' gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm} 
#' @examples
#' \dontrun{	
#' 	stock('GS',currency('USD'))
#'     update_instruments.yahoo('GS')
#' 	getInstrument('GS')
#' }
#' @export
update_instruments.yahoo <- function(Symbols=c('stocks','all'), verbose=FALSE ) {
    if (is.null(Symbols) || is.na(Symbols) || !hasArg(Symbols)) Symbols <- 'stocks'
    sym.options <- c('all','stocks')
    symkey <- sym.options[pmatch(Symbols,sym.options)]
    symkey <- na.omit(symkey)[1]
    if (!is.na(symkey)) {
	    if (symkey == 'all' || symkey == 'stocks' || is.null(Symbols)){
            if (symkey == 'all') warning('yahoo can only update stocks.')            
            Symbols <- unique(c(ls_stocks(), ls_instruments_by('src','yahoo')))
        } 
    }
    #make sure it's a vector of instrument names
    if (!is.character(Symbols)) {
        if (verbose) cat('No stocks found to update.\n') 
        return(NULL) #stop('Symbols must be a vector of instrument names, or one of "all", "stocks"')    
    }
    yahoo.syms <- Symbols
    for (i in 1:length(Symbols)) {
        tmp_instr <- try(getInstrument(Symbols[i]),silent=TRUE)
        yahoo.syms[i] <- if (!inherits(tmp_instr, 'try-error') 
                            && !is.null(tmp_instr$src) 
                            && any(names(tmp_instr$src) == 'name')) 
                         { tmp_instr$src$name } else Symbols[i]
    }
    yahoo.syms <- paste(yahoo.syms, collapse=";")
	if (is.null(yahoo.syms) || length(yahoo.syms) == 0) 
        stop('error with symbol names; no Symbols supplied?')
    yahooStuff <- quantmod:::getQuote.yahoo(yahoo.syms,
					  what=yahooQF(c("Name", 
                        "Stock Exchange",
					    "Market Capitalization",
					    "Average Daily Volume", 
                        "Earnings/Share", 
					    "EPS Estimate Current Year", 
					    "EPS Estimate Next Year", 
            			"Book Value", "EBITDA",	
                        "52-week Range")))  
#    sym.length <- length(unlist(strsplit(Symbols,";")))    	
    #see yahooQF for available whats
    for (i in 1:length(Symbols)) {
        noNA <- function(x) {
            if (x == 'N/A' || is.na(x)) {NULL} else {x}
        }
        instr <- getInstrument(Symbols[i])
		#Only update stocks from yahoo		
		if (inherits(instr,'stock') || any(instr$src == 'yahoo')) {
		    instr$name=noNA(as.character(yahooStuff[i,2]))
			instr$exchange=noNA(as.character(yahooStuff[i,3]))
			instr$market.cap=noNA(yahooStuff[i,4])
			instr$avg.volume=noNA(suppressWarnings(as.numeric(yahooStuff[i,5])))
			instr$EPS=noNA(suppressWarnings(as.numeric(yahooStuff[i,6])))
			instr$EPS.current.year.est = noNA(suppressWarnings(as.numeric(yahooStuff[i,7])))
			instr$EPS.next.year.est = noNA(suppressWarnings(as.numeric(yahooStuff[i,8])))
			instr$book.value=noNA(suppressWarnings(as.numeric(yahooStuff[i,9])))
			instr$EBITDA=noNA(yahooStuff[i,10])
			instr$range.52wk=noNA(yahooStuff[i,11])
	#		instr$IB=twsSTK(as.character(symdesc[i,1]),'SMART'),

            tclass <- unique(c(class(instr),'instrument'))
            class(instr) <- tclass        
            db <- instr$defined.by
		    if (!is.null(db)) {
		        db <- unlist(strsplit(db,";"))
		        db <- rev(unique(c("yahoo", rev(db))))
		        db <- paste(db,collapse=";") 
		    } else db <- "yahoo"
			instr$defined.by=db 
		    instr$updated=Sys.time()
            
		    assign(Symbols[i], instr, pos=.instrument)
		}
    }        
    Symbols
}

#' @export
#' @rdname update_instruments.yahoo
update_instruments.TTR <- function(Symbols = c("stocks", "all"), exchange=c("AMEX","NASDAQ","NYSE")) {
    if (!suppressWarnings(is.currency("USD"))) currency("USD")
    df <- stockSymbols(exchange=exchange)    
    if (!is.null(Symbols) && !(any(Symbols == c("stocks","all")))) {
        cols <- try( match(Symbols,df$Symbol) )
        if (!inherits(cols, 'try-error')) {
            df <- df[cols,]
        } else {
            warning(paste(paste(Symbols,collapse=","), "not found among those listed on", paste(exchange,collapse=", ")))
            return(invisible(NULL))        
        }
    } else if (!is.null(Symbols)) df <- df[match(ls_stocks(),df$Symbol),]
    cat('defining stocks...\n')
    symout <- NULL    
    for (i in 1:nrow(df)) {
        primary_id <- as.character(df$Symbol[i])
        instr <- try(getInstrument(primary_id, silent = TRUE), silent = TRUE) 
        args <- list()
        arg <- as.list(df[i, ])
        arg$defined.by <- 'TTR'
        if (is.instrument(instr) && !inherits(instr, 'stock')) {
            #make a unique primary_id
            primary_id <- make.names(c(instr$primary_id, 
                                    ls(.instrument)),unique=TRUE)[-match(ls(.instrument),
                                        make.names(c(instr$primary_id, ls(.instrument)),unique=TRUE))]            
            warning(paste("instrument",instr$primary_id,
                          "is already defined, but not as stock.",
                          "A new instrument", primary_id ,"will be created"))
        } else if (is.instrument(instr)) {
            db <- instr$defined.by
		    if (!is.null(db)) {
		        db <- unlist(strsplit(db,";"))
		        db <- rev(unique(c("TTR", rev(db))))
		        db <- paste(db,collapse=";") 
		    } else db <- "TTR"
			arg$defined.by=db 
        }
        arg$primary_id <- primary_id
        arg$currency <- "USD"
        arg$updated <- Sys.time()
        symout <- c(symout, do.call("stock", arg))
    }
    symout 
}



