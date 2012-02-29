#' extract the correct expires value from an \code{instrument}
#'
#' Currently, there are methods for \code{instrument} and \code{character}
#'
#' Will return either the last expiration date before a given Date, or the 
#' first expiration date after a given Date (if \code{expired==FALSE}).
#' @param x instrument or name of instrument
#' @param ... not in use
#' @return character string representation of an expiration date
#' @author Garrett See
#' @seealso \code{\link{expires.instrument}}, \code{\link{expires.character}}, 
#'   \code{\link{getInstrument}}
#' @examples
#' \dontrun{
#' instr <- instrument("FOO_U1", currency=currency("USD"), multiplier=1,
#'                     expires=c("2001-09-01", "2011-09-01", "2021-09-01"), 
#'                     assign_i=FALSE)
#' #Last value of expires that's not after Sys.Date
#' expires(instr) 
#' # First value of expires that hasn't already passed.
#' expires(instr, expired=FALSE)
#' # last value that's not after 2011-01-01
#' expires(instr, Date="2011-01-01") 
#' # first value that's not before 2011-01-01
#' expires(instr, Date="2011-01-01", expired=FALSE) 
#'
#' ## expires.character
#' expires("FOO_U1") # warning that FOO_U1 is not defined
#' instrument("FOO_U1", currency=currency("USD"), multiplier=1,
#'            expires=c("2001-09-01", "2011-09-01", "2021-09-01"), 
#'            assign_i=TRUE)
#' expires("FOO_U1")
#' }
#' @export
expires <- function(x, ...) {
    UseMethod("expires")
}


#' instrument expires extraction method
#' 
#' Returns either the last expiration date before \code{Date}, or the 
#' first expiration date after \code{Date} (if \code{expired==FALSE}).
#' @param Date Can be a Date or character string.  When \code{expires} is a 
#'   vector, the retuned value will be one of the two values of \code{expires} 
#'   that are closest to \code{Date}. (which one will be determined by the value 
#'   of \code{expired})
#' @param expired TRUE/FALSE. This determines which date will be used when
#'   \code{expires} is a vector.  If \code{expired} is \code{TRUE} the date 
#'   returned will be the last one before \code{Date}.  If \code{expired} is 
#'   \code{FALSE} the first one after \code{Date} will be returned. Note that
#'   if \code{expires} is a single value, \code{expired} will be ignored.
#' @method expires instrument
#' @S3method expires instrument
#' @author Garrett See
#' @keywords internal
expires.instrument <- function(x, Date, expired=TRUE, ...) {
    if (is.instrument(x)) {
        if (missing(Date)) Date <- Sys.Date()
        if (!inherits(Date, "Date")) Date <- as.Date(Date)
        xp <- x[["expires"]]
        if (length(xp) == 0) return(NULL)
        dxp <- try(as.Date(xp), silent=TRUE) #Date(s) of expiration
        if (inherits(dxp, 'try-error')) return(paste(xp))
        if (length(dxp) == 1) return(paste(xp))
        if (isTRUE(expired)) {
            return(paste(last(dxp[dxp <= Date])))
        } else return(paste(first(dxp[dxp >= Date])))
    } else NextMethod("expires")
}


#' character expires extraction method
#' 
#' if no \code{instrument} can be found by the id of \code{x}, or if the 
#' \code{instrument} does not have an \code{expires} attribute, an attempt
#' will be made to infer the year and month of expiration using \code{parse_id}
#' in which case the returned value will be a string of the format 
#' \dQuote{YYYY-MM}.  Presently, \code{Date} and \code{expired} will be ignored 
#' if \code{x} is not the name of an instrument
#' @param Date Can be a Date or character string.  When \code{expires} is a 
#'   vector, the retuned value will be one of the two values of \code{expires} 
#'   that are closest to \code{Date}. (which one will be determined by the value 
#'   of \code{expired}).  
#' @param expired TRUE/FALSE. This determines which date will be used when
#'   \code{expires} is a vector.  If \code{expired} is \code{TRUE} the date 
#'   returned will be the last one before \code{Date}.  If \code{expired} is 
#'   \code{FALSE} the first one after \code{Date} will be returned.
#' @method expires character
#' @S3method expires character
#' @seealso \code{\link{expires.instrument}}
#' @author Garrett See
#' @keywords internal
expires.character <- function(x, Date, expired=TRUE, ...) {
    xi <- getInstrument(x, silent=TRUE)
    if (is.instrument(xi)) {
        expires.instrument(xi)
    } else {
        warning(paste(x, "is not defined. Inferring only Month and Year"))
        pid <- parse_id(x)
        mth <- grep(pid$month, month.abb, ignore.case=TRUE)
        mth <- sprintf("%02d", mth, sep="-")
        paste(pid$year, mth, sep="-")
    }
}


#' spread expires extraction method
#' 
#' \code{x$expires} will be returned if it is not \code{NULL}.  Otherwise, the 
#' (character representation of the) exiration date of the first-to-expire of 
#' the \code{members} will be returned.
#' 
#' @param Date Can be a Date or character string.  When \code{expires} is a 
#'   vector, the retuned value will be one of the two values of \code{expires} 
#'   that are closest to \code{Date}. (which one will be determined by the value 
    #'   of \code{expired}).  
#' @param expired TRUE/FALSE. This determines which date will be used when
#'   \code{expires} is a vector.  If \code{expired} is \code{TRUE} the date 
#'   returned will be the last one before \code{Date}.  If \code{expired} is 
#'   \code{FALSE} the first one after \code{Date} will be returned.
#' @method expires character
#' @S3method expires character
#' @seealso \code{\link{expires.instrument}}
#' @author Garrett See
#' @keywords internal
expires.spread <- function(x, Date, expired=TRUE, ...) {
    if (inherits(x, "spread")) {
        if (!is.null(x$expires)) return(x$expires)
        members <- if (!is.null(x$memberlist$members)) {
            x$memberlist$members
        } else if (!is.null(x$members)) {
            x$members
        } else {
            warning(paste("Cannot determine members of x$primary_id"))
            return(NextMethod("expires"))
        }
        return(expires(sort_ids(members)[1]))        
    } else NextMethod("expires")
}






