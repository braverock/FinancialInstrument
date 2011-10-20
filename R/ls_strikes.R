#' show strike prices of defined options
#' 
#' list the strike prices of previously defined options.
#' 
#' If no option names are supplied, the strike prices of all defined options
#' will be returned
#' 
#' @param pattern an optional regular expression. Only names matching 'pattern'
#' are returned.
#' @return vector of strike prices
#' @author Garrett See
#' @seealso ls_options, ls_calls, ls_puts ls_instruments_by ls_underlyings
#' @examples
#' 
#' \dontrun{
#' option_series.yahoo('SPY')
#' ls_strikes(ls_options('SPY'))
#' }
#' @export
ls_strikes <- function(pattern=NULL) {
    symbols <- ls_options(pattern)
    tmp_symbols <- NULL
    for (symbol in symbols) {
        tmp_instr <- try(get(symbol,pos=.instrument),silent=TRUE)
        #if (is.instrument(tmp_instr))  
        if (!is.null(tmp_instr$strike)) 
            tmp_symbols <- c(tmp_symbols,tmp_instr$strike)
    }
    unique(tmp_symbols)    
}


