#' save data to disk
#'
#' save data to disk the way that \code{getSymbols.FI} 
#' expects it to be saved.
#' 
#' @param Symbols vector of character names of objects to be saved
#' @param base_dir character.  directory in which to store data.
#' @param env environment that holds the data to be saved (.GlobalEnv by default)
#' @return called for side-effect.
#' @seealso \code{\link{getSymbols.FI}}
#' @examples
#' \dontrun{
#' getSymbols("SPY", src='yahoo')
#' saveSymbols.common("SPY", base_dir="~/")
#' rm("SPY")
#' getSymbols("SPY", src='FI', dir="~/", split_method='common')
#' unlink("~/SPY", recursive=TRUE)
#' }
#' @export
#' @rdname saveSymbols.days
saveSymbols.days <-
function(Symbols, base_dir="", env=.GlobalEnv) {
    if (base_dir != "" && 
        !is.null(base_dir) && 
        substr(base_dir,nchar(base_dir),nchar(base_dir)) != "/") 
            base_dir <- paste(base_dir,"/",sep="")    
    tmpenv <- new.env()    
    for (symbol in Symbols) {
        tmp <- try(get(symbol,pos=env))    
        if (!is.null(tmp) && !inherits(tmp,'try-error')) { 
            D <- split(tmp,'days')
            if (!file.exists(paste(base_dir,symbol,sep=""))) dir.create(paste(base_dir,symbol,sep=""))
            fnames <- paste(base_dir,symbol,"/",
                    unlist(lapply(D, FUN=function(x) format(index(first(x)),"%Y.%m.%d"))),
                    ".", symbol, ".rda", sep="")
            for (i in 1:length(fnames)) {
                assign(symbol,D[[i]],envir=tmpenv)
                save(list=symbol,file=fnames[i],envir=tmpenv)
            }
        }    
    }
}

#' @export
#' @rdname saveSymbols.days
saveSymbols.common <- function (Symbols, base_dir = "", env = .GlobalEnv) 
{
    if (base_dir != "" && !is.null(base_dir) && substr(base_dir, 
        nchar(base_dir), nchar(base_dir)) != "/") 
        base_dir <- paste(base_dir, "/", sep = "")
    tmpenv <- new.env()
    for (symbol in Symbols) {
        tmp <- try(get(symbol, pos = env))
        if (!is.null(tmp) && !inherits(tmp, "try-error")) {
            if (!file.exists(paste(base_dir, symbol, sep = ""))) 
                dir.create(paste(base_dir, symbol, sep = ""))
            fnames <- paste(base_dir, symbol, "/", symbol, ".rda", sep = "")
            assign(symbol, tmp, envir = tmpenv)
            save(list = symbol, file = fnames, envir = tmpenv)
        }
    }
}


