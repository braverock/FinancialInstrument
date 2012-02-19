#' instrument all.equal method
#'
#' @param char.n If length of a character vector is \code{char.n} or less it 
#' will be treated as a single element.
#' @param collapse Only used if a character vector is of length less than 
#' \code{char.n}.  Unless \code{collapse} is \code{NULL}, it will be used in a 
#' call to \code{\link{paste}}.  If \code{collapse} is \code{NULL}, each element 
#' of the character vector will be compared separately.
#' @method all.equal instrument
#' @S3method all.equal instrument
#' @author Garrett See
#' @note ALPHA code. Subject to change
#' @keywords internal utilities
#' @examples
#' \dontrun{
#' currency("USD")
#' stock("SPY", "USD", validExchanges=c("SMART", "ARCA", "BATS", "BEX"))
#' stock("DIA", "USD", validExchanges=c("SMART", "ARCA", "ISLAND"), 
#'   ExtraField="something")
#' 
#' all.equal(getInstrument("SPY"), getInstrument("DIA"))
#' all.equal(getInstrument("SPY"), getInstrument("DIA"), char.n=5)
#' all.equal(getInstrument("SPY"), getInstrument("DIA"), char.n=5, collapse=NULL)
#' 
#' all.equal(getInstrument("DIA"), getInstrument("USD"))
#' }
all.equal.instrument <- function (target, current, char.n=2, collapse=";", ...) {
    # loosely based on code from base all.equal.R
    msg <- NULL
    # Same type?
    if (!isTRUE(all.equal(class(target), class(current)))) {
        msg <- paste("Classes: ", 
                     class(target)[!class(target) %in% "instrument"], ", ", 
                     class(current)[!class(current) %in% "instrument"], sep="")
        # since all instruments inherit "instrument" class, don't include 
        # "instrument in comparison. (Maybe we shouldn't include any that are
        # the same?)
    }
    nx <- names(target)
    ny <- names(current)    
    if (!all(nx %in% ny)) 
        msg <- c(msg, paste("Names in target that are not in current: <",
                            paste(nx[!nx %in% ny], collapse=", "), ">"))
    if (!all(ny %in% nx)) 
        msg <- c(msg, paste("Names in current that are not in target: <",
                            paste(ny[!ny %in% nx], collapse=", "), ">"))
    do.compare <- function(target, current, i) {
        if (!isTRUE(all.equal(target[[i]], current[[i]]))) {
            ti <- target[[i]]
            ci <- current[[i]]
            if (is.null(ti)) ti <- "NULL"
            if (is.null(ci)) ci <- "NULL"
            if (is.list(ti)) {
                unames <- uniqueNames(ti, ci)
                out <- do.call(c, 
                               lapply(unames, function(x) do.compare(ti, ci, x)))
                return(paste(i, out, sep="$"))
            }
            if (length(ti) > char.n && is.character(ti)) {
                out <- NULL
                if (!all(ti %in% ci)) 
                    out <- paste(i, "in target but not in current: <",
                                paste(ti[!ti %in% ci], collapse=", "), ">")
                if (!all(ci %in% ti))
                    out <- c(out, paste(i, "in current but not in target: <",
                                paste(ci[!ci %in% ti], collapse=", "), ">"))
                return(out)
            } else if (is.character(ti)) {
                if (!is.null(collapse)) {
                    out <- paste(paste(ti, collapse=collapse), 
                                 paste(ci, collapse=collapse), sep=", ")
                    out <- paste(i, ": ", out, sep="")
                    return(out)
                }
            }
            if (is.xts(ti)) {
                ae <- all.equal(ti, ci)
                if (!isTRUE(ae)) return(paste(i, ae, sep=": "))
            }
            
            out <- paste(ti, ci, sep=", ")
            out <- paste(i, ": ", out, sep="")
            
            return(out)
        } 
    }
    uniqueNames <- function(target, current) {  
        unique(c(names(target), names(current)))
    }
    nxy <- uniqueNames(target, current)

    msg <- c(msg, 
             do.call(c, lapply(nxy, function(x) do.compare(target, current, x))))
    
    if (is.null(msg)) 
        TRUE
    else msg
}