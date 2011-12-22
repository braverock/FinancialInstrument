#' convert tick data to one-second data
#'
#' From tick data with columns: \dQuote{Price}, \dQuote{Volume}, \dQuote{Bid.Price},
#' \dQuote{Bid.Size}, \dQuote{Ask.Price}, \dQuote{Ask.Size}, to data of one second frequency
#' with columns \dQuote{Bid.Price}, \dQuote{Bid.Size}, \dQuote{Ask.Price}, \dQuote{Ask.Size},
#' \dQuote{Trade.Price}, and \dQuote{Volume}
#'
#' The primary purpose of this function is to reduce the amount of data on disk so that
#' it will take less time to load the data into memory.
#'
#' If there are no trades or bid/ask price updates in a given second, we will not make
#' a row for that timestamp.  If there were no trades, but the bid or ask
#' price changed, then we _will_ have a row but the Volume and Trade.Price will be NA.  
#' 
#' @param x the xts series to convert to 1 minute BATMV
#' @return an xts object of 1 second frequency
#' @author gsee
#' @examples
#' \dontrun{
#' getSymbols("CLU1")
#' system.time(xsec <- to_secBATV(CLU1))
#' }
#' @export
to_secBATV <- function(x) {
    #require(qmao)
    # Define Bi and As functions (copied from qmao package)
    Bi <- function(x) {
        if (has.Bid(x)) 
            return(x[, grep("Bid", colnames(x), ignore.case = TRUE)])
        stop("subscript out of bounds: no column name containing \"Bid\"")
    }
    As <- function(x) {
        if (has.Ask(x)) 
            return(x[, grep("Ask", colnames(x), ignore.case = TRUE)])
        stop("subscript out of bounds: no column name containing \"Ask\"")
    }

    x <- make.index.unique(x)
    ohlcv <- suppressWarnings(to.period(x[,1:2], 'seconds', 1))
    Volm <- if (!has.Vo(ohlcv)) {
                rep(NA, NROW(ohlcv)) 
            } else Vo(ohlcv)
    ClVo <- if(length(ohlcv) != 0) { ohlcv[, 4:5] } else {
        tmp <- xts(cbind(rep(NA, NROW(x)), rep(NA, NROW(x))), index(x))
        tmp[endpoints(tmp, 'seconds')]
    }
    xx <- x[endpoints(x, 'seconds')]
    xx <- cbind(Bi(xx), As(xx), ClVo, all=TRUE)
    xx[, 1:4] <- na.locf(xx[, 1:4])
    colnames(xx) <- c("Bid.Price", "Bid.Size", "Ask.Price", "Ask.Size", "Trade.Price", "Volume")
    #if volume is zero, and all other rows are unchanged, delete that row 
    out <- xx
    v <- out[, 6]
    v[is.na(v)] <- 0
    dout <- cbind(diff(out[,c(1, 3)]), v)
    align.time(out[index(dout[!rowSums(dout) == 0])], 1)
}



#' Convert several files from tick to 1 second
#'
#' @param getdir directory to get tick data from
#' @param savedir directory to save converted data in
#' @param Symbols names of instruments to convert
#' @param overwrite TRUE/FALSE. If file already exists in savedir, should it be overwritten?
#' @return list of files that were converted
#' @author gsee
#' @examples
#' \dontrun{
#' convert.log <- alltick2sec()
#' }
alltick2sec <- function(getdir = '~/TRTH/tick/', 
                        savedir = '~/TRTH/sec/', 
                        Symbols=list.files(getdir),
                        overwrite = FALSE) {
    if (!file.exists(savedir)) stop(paste("Please create savedir (", savedir, ") first", sep=""))
    require(foreach)
    Symbols <- Symbols[!Symbols %in% c("instruments.rda")]
    gsep <- if(substr(getdir, nchar(getdir), nchar(getdir)) == "/") { "" } else "/"
    ssep <- if(substr(savedir, nchar(savedir), nchar(savedir)) == "/") {""} else "/"
    s=NULL    
    foreach(s = Symbols) %dopar% {
        cat("converting ", s, ' ...\n')
        gdir <- paste(getdir, s, sep=gsep)
        if (file.exists(gdir)) {
            sdir <- paste(savedir, s, sep=ssep) 
            if (!file.exists(sdir)) dir.create(sdir) #create dir for symbol if it doesn't exist
            fls <- list.files(gdir)
            fls <- fls[!fls %in% c("Bid.Image", "Ask.Image", "Price.Image")]
            tmpenv <- new.env() 
            unname(sapply(fls, function(fl) {
                if (!file.exists(paste(sdir, fl, sep='/')) || overwrite) {
                    xsym <- try(load(paste(gdir, fl, sep="/")))
                    if (!inherits(xsym, 'try-error') && !is.null(get(xsym))) {
                        #x <- to_secBATMV(get(xsym))
                        x <- try(to_secBATV(get(xsym)), silent=TRUE)
                        if (!inherits(x, 'try-error')) {
                            assign(xsym, x, pos=tmpenv)
                            sfl <- paste(sdir, fl, sep="/")
                            save(list = xsym, file = sfl, envir = tmpenv)
                            rm(xsym, pos=tmpenv)
                            fl
                        }
                    }
                } else warning(paste(sdir, '/', fl, 
                    " already exists and will not be overwritten. Use overwrite=TRUE to overwrite.", sep=""))
            }))
        } else warning(paste(gdir, 'does not exist'))
    }
}

