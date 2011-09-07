
#' format an id
#' 
#' convert the primary_id or suffix_id of an \code{instrument} to a different format.
#' Primarily intended for \code{\link{future_series}} instruments.
#'
#' Formats for the suffix_id include 
#' 'CY', 'CYY', and 'CYYYY' where C is the month code and Y is numeric.
#' 'MMMY', 'MMMYY', 'MMMYYYY' where MMM is an uppercase month abbreviation.
#' 'xxCY', 'xxCYY', 'xxCYYYY' where xx can be any 2 characters.
#' 
#' There are currently only 2 formats available for \code{\link{option_series}}: 'opt2' and 'opt4'
#' where opt2 uses a 2 digit year and opt4 uses a 4 digit year.
#'
#' @param id character. the id to be reformatted. Can be either a primary_id or a suffix_id
#' @param format character string indicating how the id should be formatted. See Details.
#' @param parse character name of parsing method to use:  "id" or "suffix"
#' @param sep character that will separate root_id and suffix_id of output if calling with \code{parse="id"}
#' @param ... parameters to pass to the parsing function
#' @return character id of the appropriate format
#' @author Garrett See
#' @seealso \code{\link{parse_id}}, \code{\link{parse_suffix}},
#' \code{\link{M2C}}, \code{\link{month_cycle2numeric}}
#' @examples
#' format_id('U1', format='MMMYY', parse='suffix')
#' format_id('ES_JUN2011', format='CYY', parse='id')
#' format_id("SPY_20110826P129","opt2")
#' #several at once
#' id3 <- c('VX_aug1','ES_U1', 'VX_U11')
#' format_id(id3,'MMMYY')
#' format_id(id3,'CYY')
#' format_id(id3,'CY',sep="")
#' @export
format_id <- function(id, format=NULL, parse=c('id', 'suffix'), sep="_", ...) {
    if (!is.null(format) && format == FALSE) format <- NULL
	parse <- parse[[1]]
	out <- NULL
    for (i in id) {
        pid <- do.call(paste('parse', parse, sep="_"), list(i, ...))
        suffix <- ifelse(parse=='id',pid$suffix, i)
		if (any(!is.null(format))) {
			tmp <- switch(format[[1]], 
                CY=paste(pid$root,paste(M2C(pid$month),substr(pid$year,4,4),sep=""), sep=sep),
                CYY=paste(pid$root,paste(M2C(pid$month),substr(pid$year,3,4),sep=""), sep=sep),
                CYYYY=paste(pid$root,paste(M2C(pid$month),pid$year,sep=""), sep=sep),
                MMM=paste(pid$root,pid$month,sep=sep),
                MMMY=paste(pid$root, paste(pid$month,substr(pid$year,4,4), sep=""), sep=sep),
                MMMYY=paste(pid$root, paste(pid$month,substr(pid$year,3,4), sep=""), sep=sep),
                MMMYYYY=paste(pid$root, paste(pid$month,pid$year,sep=""), sep=sep),
                xxCY=paste(pid$root, paste(substr(suffix,1,2), M2C(pid$month), substr(pid$year,4,4), sep=""), sep=sep),
                xxCYY=paste(pid$root, paste(substr(suffix,1,2), M2C(pid$month), substr(pid$year,3,4), sep=""), sep=sep), 
                xxCYYYY=paste(pid$root, paste(substr(suffix,1,2), M2C(pid$month), pid$year, sep=""), sep=sep),
                NNNN=paste(pid$root, sprintf("%02d", match(pid$month,toupper(month.abb))), substr(pid$year,3,4), sep=sep),
                opt2={
                    if (!any(pid$format == c("opt2","opt4"))) stop("I'm not programmed to convert non-option_series_ids to option_series_ids")
                    ifelse(pid$format == "opt4", paste(pid$root, substr(suffix,3,nchar(suffix)), sep=sep), i)
                }, 
                opt4={
                    if (!any(pid$format == c("opt2","opt4"))) stop("I'm not programmed to convert non-option_series_ids to option_series_ids")
                    ifelse(pid$format == "opt2", paste(pid$root, paste("20",suffix,sep=""), sep=sep), i)
                }, 
                i)
		} else tmp <- paste(pid$root,suffix,sep=sep)
        if (substr(tmp,1,1) == "_") tmp <- substr(tmp,2,nchar(tmp))
        if (substr(tmp,nchar(tmp),nchar(tmp)) == "_") tmp <- substr(tmp,1,nchar(tmp)-1)
        out <- c(out, tmp)
    }
    out
}

#' coerce month_cycle to a numeric vector
#'
#' This will convert month codes or month names to numeric months.
#'
#' Input can be a vector or a comma-delimited string
#' @return numeric vector
#' @param month_cycle the expiration months of a \code{link{future}}. See examples.
#' @author Garrett See
#' @seealso \code{\link{M2C}}, \code{\link{C2M}}, \code{\link{next.future_id}}
#' \code{\link{future}}
#' @examples
#' month_cycle2numeric("H,M,U,Z")
#' month_cycle2numeric(c("H","M","U","Z"))
#' month_cycle2numeric("Mar,jun,SEP,dEc")
#' month_cycle2numeric("March,june,sep,decem")
#' month_cycle2numeric("3,6,9,12")
#' month_cycle2numeric(seq(3,12,3))
#' @export
month_cycle2numeric <- function(month_cycle) {
    if (is.character(month_cycle)) {
        cycle.chr <- toupper(strsplit(paste(month_cycle, collapse=","), ",")[[1]])
        if (!any(suppressWarnings(is.na(as.numeric(cycle.chr))))) {
            month_cycle <- as.numeric(cycle.chr)
        } else {
            month_cycle <- match(cycle.chr, M2C()) # "H,M" or c("H","M")
            if (any(is.na(month_cycle))) month_cycle <- pmatch(cycle.chr, toupper(month.name))  # "Mar,Jun" or c("MAR","JUN")
        }
    }
    month_cycle
}

# @examples
# month_cycle2code('feb,apr,jun,aug,dec')
month_cycle2code <- function(month_cycle) {
    M2C()[month_cycle2numeric(month_cycle)]
}

# @examples 
# month_cycle2string('feb,apr,jun,aug,dec')
month_cycle2string <- function(month_cycle) {
    paste(M2C()[month_cycle2numeric(month_cycle)], collapse=",")
}


#' Get the primary_id of the next-to-expire (previously expiring) future_series instrument
#' 
#' Using \code{\link{parse_id}}, this will figure out where in the \code{month_cycle} that \code{id}
#' belongs.  Then, it will use the next (previous) month in \code{month_cycle} to construct the id of the
#' next-to-expire contract.
#'
#' \code{month_cycle} can be a numeric vector (corresponding to the months in which contracts expire),
#' or it can be a vector of month codes, a vector of month abbreviations, or a comma-delimited
#' string of month codes or abbreviations, in which case an attempt will be made to convert it to a numeric vector.
#' by passing it through \code{\link{month_cycle2numeric}}
#' 
#' \code{root} is primarily used when you have an id that does not have an underscore, in which case, providing \code{root}
#' will make splitting the id into primary_id and suffix_id easier and more accurate.  \code{root} can also be used if you want
#' the returned id to be on a different \code{future} than the id you passed in (when used this way, \code{format} should also be used). 
#'
#' By default, (when called with \code{format=NULL}) the returned id will be of the same format as the \code{id} that was passed in.  
#' The format of the returned id can be specified with the \code{format} argument.  See \code{\link{format_id}} for supported values of \code{format}
#' @param id character string primary_id of a future_series instrument
#' @param month_cycle months in which contracts expire. numeric or month codes. See Details.
#' @param root root_id. usually only used if there is no underscore in the \code{id}. See Details.
#' @param format how you would like the returned id to be formatted. If NULL, it will match the format of \code{id}. See Details.
#' @return character
#' @author Garrett See
#' @seealso \code{\link{format_id}} for supported values of \code{format}.
#' \code{\link{month_cycle2numeric}}
#' @examples
#' next.future_id("ES_Z1","H,M,U,Z", format=NULL) 
#' next.future_id("VIXAUG11", 1:12, root='VIX', format=NULL)
#' next.future_id("YM_Q11", seq(3,12,3)) #gives a warning about 'Q' not being part of month_cycle
#' @export
#' @rdname next.future_id
next.future_id <- function(id, month_cycle=seq(3,12,3), root=NULL, format=NULL) {
    out <- NULL  
    #if month_cycle is character, convert it to numeric vector 
    month_cycle <- month_cycle2numeric(month_cycle)  
    for (ID in id) {
        pid <- parse_id(ID, silent=TRUE, root=root)
        y <- pid$year
        curr.m <- match(pid$month, toupper(month.abb))    

        if (is.na(match(curr.m,month_cycle))) { 
            warning('suffix falls inbetween month_cycle months.')
            #add curr.m to month_cycle
            month_cycle <- sort(c(curr.m, month_cycle))
        }

        if (curr.m == last(month_cycle)) {
            y <- y + 1
            nxt.m <- month_cycle[1]
        } else nxt.m <- month_cycle[match(curr.m,month_cycle)+1]

        suffout <- paste(M2C()[nxt.m], substr(y,3,4), sep="")
        #if there is no underscore in ID sep="", else sep="_"    
        if (identical(integer(0),grep("_",ID))) {sep <- ""} else sep="_"
        if (is.null(format)) format <- pid$format
        suffout <- format_id(suffout, format, parse='suffix')
        out <- c(out, paste(pid$root, suffout, sep=sep))
    }
    out
}

#' @export
#' @rdname next.future_id
prev.future_id <- function(id, month_cycle=seq(3,12,3), root=NULL, format=NULL) {
    out <- NULL    
    month_cycle <- month_cycle2numeric(month_cycle)
    for (ID in id) {
        pid <- parse_id(id, silent=TRUE, root=root)
        y <- pid$year
        curr.m <- match(pid$month, toupper(month.abb))    

        if (is.na(match(curr.m,month_cycle))) { 
            warning('suffix falls inbetween month_cycle months.')
            #add curr.m to month_cycle
            month_cycle <- sort(c(curr.m, month_cycle))
        }

        if (curr.m == first(month_cycle)) {
            y <- y - 1
            prev.m <- last(month_cycle)
        } else prev.m <- month_cycle[match(curr.m,month_cycle)-1]

        suffout <- paste(M2C()[prev.m], substr(y,3,4), sep="")
        #if there is no underscore in id and format==NULL sep="", else sep="_"    
        if (identical(integer(0),grep("_",id))) {sep <- ""} else sep="_"
        if (is.null(format)) format <- pid$format
        suffout <- format_id(suffout, format, parse='suffix')
        out <- c(out, paste(pid$root, suffout, sep=sep))
    }
    out
}

#' sort primary_ids of instruments
#' 
#' Primarily intended for use on the primary_ids of \code{\link{future_series}} instruments.
#' This will sort ids by expiration.  All ids that do not contain month and year information 
#' will be sorted alphabetically (separately) and appended to the end of the other sorted ids.
#' @param ids character vector of ids
#' @param ... arguments to pass through to \code{\link{parse_id}}
#' @return sorted character vector of the same length as \code{ids}
#' @author Garrett See
#' @seealso \code{\link{parse_id}}
#' @examples
#' ids <- c("ES_U11",'GLD','SPY',"YM_Jun11",'DIA','VX_V10')
#' sort_ids(ids)
#' @export
sort_ids <- function(ids, ...) {
    if (is.null(ids)) return(NULL)
    f <- function(x, ...) {
        pid <- parse_id(x, ...)
        as.Date(paste(pid$year,pid$month,15,sep=''),format="%Y%b%d")
    }
    out1 <- names(sort(sapply(ids,f, ...)))
    out2 <- sort(ids[!(ids %in% out1)])
    c(out1,out2)
}

