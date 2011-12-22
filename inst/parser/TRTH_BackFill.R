#############################################################################################
# This file contains functions that are used to parse zipped csv files from Reuters         
# After sourcing these functions (this script),                                             
# 1st run "configureTRTH" which will create an environment to hold parameter values         
# 2nd run "download_reut" to download the big zipped csv files to your archive directory    
# 3rd run "splitCSV" that will unzip the big zipped Reuters files that were downloaded.     
#   Then it will split the file such that there will be a file for each day for each symbol 
#   and it will put those split files in your csv directory                                 
# 4th run "FEreut2xts" (requires foreach) which will read the csv files into R, 
#   do a little data scrubbing, and save the xts data into your tick directory.  
#   Then it will convert the data to 1 second frequency data and save that into your sec 
#   directory.  Also, if tick.image and/or sec.image are TRUE, it will create plots of the 
#   data and store them.             
#                                                                                           
#############################################################################################
#                  Reuters Backfill Configuration Parameters                                #
#############################################################################################
## Arguments and their defaults
# config.file (character name of config file) is optional.  
# If provided, config.file will be sourced; 
# i.e you can use it instead of specifying all the parameters in the dots. 
# any arguments provided in dots will override arguments of same name from config.file
##
#path.output = '~/TRTH/'                # base directory for output
#tick_dir = '~/TRTH/tick'               # directory in which to store tick data
#archive_dir = '~/TRTH/archive'         # directory in which to store downloaded .gz files
#csv_dir = '~/TRTH/csv'                 # directory in which to store zipped csv files
#sec_dir = '~/TRTH/sec'                 # directory in which to store second data
#sec.image = TRUE                       # save a chart of the second data?
#tick.image = TRUE                      # save a chart of the tick data?
#default_type = 'guaranteed_spread'     # passed to instrument.auto if type cannot be inferred from RIC 
#default_currency = 'USD'               # passed to instrument.auto if type cannot be inferred from RIC
#digits.sec = 6                         # for options(digits.secs=digits.secs)
#width = 200                            # for options(width=width)
#instrument_file = [searches path.output for filename containing 'instruments'] #name of instrument envir RData file
#job.name = ""                          # the Reuters TRTH job name (by default all files not on disk)
#no.cores = 4                           # number of cores for foreach
#overwrite = FALSE                      # will not redownload, or overwrite files unless this is TRUE
#username = stop("")                    #TRTH user name, usually your email address
#password = stop("")                    #TRTH password
#
#email_to <- 'someuser@somehost.com'    #NOT IN USE
#email_from <- 'someuser@somehost.com'  #NOT IN USE
#############################################################################################
#               Below is how you would typically use these functions                        #
#############################################################################################
##configureTRTH('~/TRTH/TRTH_config_file.R')
## OR
#configureTRTH(
#    path.output = '~/TRTH/',
#    width = 200,
#    digits.secs = 6,
#    instrument_file = '~/TRTH/instruments.RData',
#    username = 'email@domain.com',
#    password = 'password',
#    default_type = 'guaranteed_spread',
#    default_currecy = "USD",
#    job.name = "ReutersJobName",
#    overwrite = FALSE,
#    tick.image = TRUE,
#    sec.image = TRUE,
#    no.cores = 20
#)
#
#download_reut(.TRTH)                   # Download big zipped CSV
#system.time(splitCSV(.TRTH))           # Split into daily CSVs
#system.time(Out <- FEreut2xts(.TRTH))  # Convert to xts data: tick and second
#############################################################################################


configureTRTH <- function(config.file, path.output='~/TRTH/', ...) {
    ## Create environment to hold variables that more than one function needs to access    
    .TRTH <- new.env(parent=.GlobalEnv)
    dargs <- list(...)

    ## Load required packages
    require(qmao)
    #require(FinancialInstrument)
    require(doMC)
    #require(sendmailR) # for email on failure

    ## Some convenience functions
    addslash <- function(x) {
        if (substr(x, nchar(x), nchar(x)) != '/') paste(x, "/", sep="")
        else x   
    }
    makeDir <- function(x) { #if directory does not exist, create it
        dir.create(x, showWarnings=FALSE, recursive=TRUE, mode="0775") #why not mode="0664" ???
    }

    ## Source the config_file -- this will be overwritten by any arguments in dots
    if (!missing(config.file)) source(config_file)

    # There are some variables that we need that should be in the config file.
    # Anything passed in through dots will override arguments of the same name that were in config_file
    # Some things (subdirectory names) we will create if they aren't in dots or config_file
    pickDirArg <- function(x) {
        if (!is.null(dargs[[x]])) return(dargs[[x]]) #passed through dots
        if (exists(x)) return(addslash(get(x)))
        addslash(paste(path.output, sub("_dir", "", x), sep=""))
    }

    #if (!is.null(dargs$path.output)) 
    .TRTH$path.output <- path.output <- addslash(path.output)

    .TRTH$archive_dir <- pickDirArg("archive_dir")
    .TRTH$csv_dir <- pickDirArg("csv_dir")
    .TRTH$tick_dir <- pickDirArg("tick_dir")
    .TRTH$sec_dir <- pickDirArg("sec_dir")

    # Make sure the directories we need exist.
    makeDir(.TRTH$path.output)
    makeDir(.TRTH$archive_dir)
    makeDir(.TRTH$csv_dir)
    makeDir(.TRTH$tick_dir)
    makeDir(.TRTH$sec_dir)

    pickArg <- function(x, default=NULL) {
        # if argument "x" was passed through dots, use that
        # otherwise, if it was in config_file, use that
        # if it's neither in dots, nor in config_file, use default
        if (!is.null(dargs[[x]])) return(dargs[[x]]) #passed through dots
        if (exists(x)) return(get(x))
        default
    }

    ## Set some options/preferences
    .TRTH$width <- pickArg('width', 200)
    options(width=.TRTH$width)
    .TRTH$digits.sec <- pickArg('digits.sec', 6)
    options(digits.secs=.TRTH$digits.secs)

    .TRTH$username <- pickArg('username', stop("Please provide your username"))
    .TRTH$password <- pickArg('password', stop("Please provide your password"))
    .TRTH$job.name <- pickArg('job.name', "")
    .TRTH$default_type <- pickArg('default_type', 'guaranteed_spread')
    .TRTH$default_currency <- pickArg('default_currency', 'USD')
    .TRTH$overwrite <- pickArg('overwrite', FALSE)
    .TRTH$tick.image <- pickArg('tick.image', TRUE)
    .TRTH$sec.image <- pickArg('sec.image', TRUE)
    .TRTH$no.cores <- pickArg('no.cores', 4)

    instr.file.bak <- tail(list.files(path.output)[grep("instruments", list.files(path.output))], 1)
    .TRTH$instrument_file <- pickArg('instrument_file', instr.file.bak)
    if (length(.TRTH$instrument_file) == 0 || is.na(.TRTH$instrument_file) || !file.exists(.TRTH$instrument_file))
        stop("Please specify a valid filepath for instrument_file or move a file with 'instruments' in its name to 'path.output'")

    registerDoMC(.TRTH$no.cores)
    # registerDoSEQ()

    assign('.TRTH', .TRTH, pos=.GlobalEnv)
    .TRTH
}


download_reut <- function(.TRTH) {
    if (missing(.TRTH)) {
        .TRTH <- try(get('.TRTH', pos=.GlobalEnv))
        if (inherits(.TRTH, 'try-error')) stop("Run configureTRTH function first")
    }
    attach(.TRTH)
    Sys.umask("0002")

    Archive.output <- list.files(archive_dir)
    Archive.output <- Archive.output[grep("\\.gz",Archive.output)]
    omit <- c(grep("confirmation",Archive.output),grep("report",Archive.output))
    if (length(omit) > 0) Archive.output <- Archive.output[-omit]

    listflag=FALSE
    while(!listflag)#try to download file list 
    {
        clear <- warnings() #currency loads from oanda alway generate warnings, clear them out
        Reuters <- system(paste("curl ftp://tickhistory-ftp.thomsonreuters.com:15500/results/ -u ",
                                username,":",password," --ftp-ssl -k -l",sep=""),intern=TRUE)
        cat("\n")
        w <- ''
        w <- warnings()[!warnings() %in% clear]
        if(!as.logical(length(Reuters)) || isTRUE(grep('curl',names(w))))
        {
            tmpmsg<-paste("curl returned error code", names(w),'\n',
                        'while attempting to download file list','\n',
                        'script will wait and retry in 30 min')
            #sendmail(email_to,email_from,"error downloading Reuters file list",msg=tmpmsg)
            Sys.sleep(180)
        } else listflag=TRUE
        
    }
    # now we're past the while loop, so we have a file list
    Reuters.report <- Reuters[grep("report",Reuters)]

    Reuters.output <-  Reuters[-c(grep("report",Reuters),grep("confirmation",Reuters))]
    Reuters.output <-  Reuters.output[grep(job.name, Reuters.output)]

    files.gz <- Reuters.output[!(Reuters.output %in% Archive.output)]
    #files.gz <- paste(username, "-", job.name, ".csv.gz", sep="")

    if (length(files.gz) == 0) files.gz <- Reuters.output
    .TRTH$files.gz = files.gz
    assign(".TRTH", .TRTH, pos=.GlobalEnv)
    
    for(i in 1:length(files.gz))
    {	
        filename.gz <- files.gz[i]
        filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))
	
        alias <- unlist(strsplit(filename.gz,"-"))[3]
        alias <- unlist(strsplit(alias,".csv.gz"))

	    ## Download New Datasets
        print(paste("Downloading ",filename.gz,sep=""))
        fileflag=FALSE
        Reuters2 <- 0
        while(!fileflag) #try to download individual files
        {
            if (!file.exists(paste(archive_dir, filename.gz, sep="")) || overwrite) {
                Reuters2 <- system(paste("curl -m 10800 --max-filesize 1610612736 ftp://tickhistory-ftp.thomsonreuters.com:15500/results/", 
                                    filename.gz, " -u ", username, ":", password, " --ssl -k > ", archive_dir, filename.gz, sep=""))
            } #else cat(paste(filename.gz, 'already exists, and overwrite==FALSE; not re-downloading.'), "\n")
            if(Reuters2 != 0)
            {
                w2 <- ''
                w2 <- warnings()
                tmpmsg <- paste("curl returned error code", Reuters2,"\n",
                                w2,'\n','while attempting to download',filename.gz,'\n',
                                'will wait and retry in 10 min')
                #sendmail(email_to,email_from,paste("error downloading Reuters file",filename.gz),msg=tmpmsg)
                Sys.sleep(600)
            } else fileflag=TRUE
        }
        
	    ## Download Report s
        if (!file.exists(paste(archive_dir, Reuters.report[grep(alias,Reuters.report)], sep="")) || overwrite) { 
    	    system(paste("curl ftp://tickhistory-ftp.thomsonreuters.com:15500/results/",
                        Reuters.report[grep(alias,Reuters.report)], " -u ", username, ":", password,
                        " --ftp-ssl -k > ", archive_dir, Reuters.report[grep(alias,Reuters.report)], sep=""))
	        #system(paste("gzip -d -f ",archive_dir,"Report/",Reuters.report[grep(alias,Reuters.report)],sep=""))
	        cat("\n")
        } #else cat(paste(Reuters.report[grep(alias,Reuters.report)], 
          #      "already exists, and overwrite==FALSE; not re-downloading.\n"))
    }

    #save(files.gz, file=paste(archive_dir, 'files.gz.tmp.rda', sep=""))
    #files.gz
    detach(.TRTH)
    .TRTH
}


get_files.gz <- function(archive_dir, job.name){
    # Don't _really_ need this function now that .TRTH envir is being passed around
    # but might as well use it since it's already written
    if (!file.exists(archive_dir)) stop("archive_dir does not exist")

    Archive.output <- list.files(archive_dir)
    Archive.output <- Archive.output[grep("\\.gz",Archive.output)]
    omit <- c(grep("confirmation",Archive.output),grep("report",Archive.output))
    if (length(omit) > 0) Archive.output <- Archive.output[-omit]
    Reuters.output <-  Archive.output[grep(job.name, Archive.output)]
    #if (length(Reuters.output) == 0) Reuters.output <- Archive.output
    Reuters.output
}


splitCSV <- function(.TRTH) {
    #FIXME: respect overwrite argument
    if (missing(.TRTH) && !exists(".TRTH")) stop("Run configureTRTH function first")
    attach(.TRTH)
    if (substr(path.output, nchar(path.output), nchar(path.output)) != "/") {
        .TRTH$path.output <- path.output <- paste(path.output, "/", sep="")
    }

    if (!exists('files.gz')) .TRTH$files.gz <- files.gz <- get_files.gz(archive_dir, job.name)

    if (!exists('instrument_file')) { #Don't need this anymore
        tmp <- list.files(paste(path.output))
        instrument_file <- paste(path.output, tail(tmp[grep("instruments", tmp)], 1), sep="")
        if (!file.exists(instrument_file)) {
            stop("Could not find instrument_file; please specify")
        } else .TRTH$instrument_file <- instrument_file
    }

    loadInstruments(instrument_file)
    registerDoMC(no.cores)

    ## unzip and split (new unzip method does not require rezip; keeps original gz file)
    setwd(archive_dir)

    foreach(i = 1:length(files.gz)) %dopar% 
    { # unzip in parallel
        filename.gz <- files.gz[i]
        filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))
	    #unzip the file
	    print(paste("unzipping ",filename.gz, sep=""))
        #system(paste("gzip -d -f ",archive_dir,filename.gz,sep=""))
        system(paste("gunzip -f < ", archive_dir, filename.gz, " > ", archive_dir, filename.csv, sep=""))
    }

    for (i in 1:length(files.gz)) 
    {
        filename.gz <- files.gz[i]
        filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))
        # Use awk to split the big CSV into daily CSVs.  Each CSV will have a single
        # row which we will then overwrite with the column headers.  Then we'll
        # use awk again to put the data into the split files
        #
        # First, make empty files (er, 1-row files that will be overwritten with header)
        # awk string says to make a file and put this row in it if the RIC or date are different than the previous row's RIC/date
        print(paste('Making headers from', filename.csv))
        system(paste('awk -v f2="" -F "," '," '",'{f1 = $1"."$2".csv";if(f1 != f2) { print >> f1; close(f2); f2=f1; } }',"' ",filename.csv, sep=""))
    }

    tmpfiles <- list.files(archive_dir)
    files.header <- tmpfiles[grep("RIC",tmpfiles)]        
    big.files <- tmpfiles[grep("@", tmpfiles)] #Big zipped CSVs from Reuters have e-mail address in name
    #big.files <- tmpfiles[grep(job.name, tmpfiles)]
    # csv files will be everthing that is not in "ignore" below
    # all these things we're ignoring should actually be in path.output, not here
    ignore <- c(big.files, files.header, 'NA', "Report", 
                tmpfiles[grep("Tick2Sec|TRTH_config_file", tmpfiles)],
                tmpfiles[grep("\\.rda", tmpfiles)], 
                tmpfiles[grep("\\.RData", tmpfiles)],
                tmpfiles[grep("missing_instruments", tmpfiles)]               
                )

    files.csv <- tmpfiles[!tmpfiles %in% ignore]
    .TRTH$files.csv <- files.csv
    # files.header now has several identical rows. Delete all but first by extracting first row, and overwritting file with it
    system(paste('head -1 "', files.header, '" > header.csv', sep="")) # extract 1st line
    # head -1 "RIC.Date[G].csv" > header.csv
    system(paste('mv header.csv "', files.header, '"', sep=""))        # replace files.header with only 1st line
    # mv header.csv "RIC.Date[G].csv"

    for (fl in files.csv) {
        system(paste('cp "', files.header, '" ', paste(archive_dir, fl, sep=""), sep=""))
        #cp "#RIC.Date[G].csv" /home/garrett/TRTH/archive/GEM1-U1.01-APR-2008.csv
    }

    for (j in 1:length(files.gz))
    {   
        filename.gz <- files.gz[j]
        filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))

	    ## Split the Files 
	    print(paste("Splitting ",filename.csv,sep=""))

        # The following awk will put data in our CSV files which currently only have column headers;
        #  Improved awk w/ file close to deal with 'too many open files', thanks to Josh Ulrich
        system(paste('awk -v f2="" -F "," '," '",'{f1 = $1"."$2".csv";print >> f1; if(f1 != f2) { close(f2); f2=f1; } }',"' ",filename.csv, sep=""))
        ## command line awkstring would look like this:
        # awk -v f2="" -F ","  '{f1 = $1"."$2".csv"; print >> f1; if(f1 != f2) { close(f2); f2=f1; } }' sourcefile.csv
        ## NOTE: if you get errors on 'too many open files' from awk, you'll need to adjust ulimit/nolimit
	    print(paste('Done splitting ', filename.csv, sep=""))

        # remove header file
        invisible(file.remove(paste(archive_dir, files.header, sep="")))
        # remove unzipped csv
        invisible(file.remove(paste(archive_dir, filename.csv, sep="")))
	    ## Zip the File
        # print(paste("zipping ",filename.csv,sep=""))
        # system(paste("gzip -f ",archive_dir,filename.csv,sep=""))
    }	

    # Move
    #mv -vf paste(files) csv_dir

    # Move split CSVs into csv_dir
    files.xts <- NULL
#    foreach (k = icount(length(files.csv))) %dopar%
    for (k in 1:length(files.csv))
    {
        #print(k)
        name.csv <- files.csv[k]                        # "ASBC.O.08-JAN-2011.csv"
        #name <- unlist(strsplit(name.csv,".",fixed=TRUE))[1]
        spl.name <- unlist(strsplit(name.csv, "\\."))   # "ASBC" "O" "08-JAN-2011" "csv" 
        last2 <- (length(spl.name) - 1):length(spl.name)# 3 4
        name <- paste(spl.name[-last2], collapse=".")   # "ASBC.O"
        #RIC.date <- try(as.Date(unlist(strsplit(name.csv,".",fixed=TRUE))[2], format="%d-%b-%Y"))
        RIC.date <- try(as.Date(spl.name[last2[1]], format="%d-%b-%Y"))
        date.format <- gsub("-",".",RIC.date)

        ## Handle leading digits and VIX and Cash
        name.new <- if(substr(name,1,1)==1){
            make.names(substr(name,2,nchar(name)))
        } else make.names(name)

        ## Create directory if it does not exist
        dir.create(paste(csv_dir, date.format, "/", sep=""), showWarnings=FALSE, recursive=TRUE, mode='0775') #mode='0664'

        ## Move files to appropriate place
        #system(paste("mv -vf ", path.output,"Archives/",name.csv, " ", path.output,date.format,"/",date.format,".",name.new,".csv", sep=""))
        system(paste("mv -f ", name.csv, " ", csv_dir, date.format, "/", date.format, ".", name.new, ".csv", sep=""))

        print(paste(date.format, name.new, "moved", sep=" "))
        files.xts <- rbind(files.xts,as.data.frame(cbind(name.new,date.format),stringsAsFactors=FALSE))
    }
    files.xts$type <- rep(NA, NROW(files.xts))

    .TRTH$files.xts <- files.xts
    assign('.TRTH', .TRTH, pos=.GlobalEnv)
    

    missing_i <- NULL
    instr_s <- unique(files.xts[,'name.new'])
    print(paste('Defining', length(instr_s), 'missing instruments'))
    missing_list <- list() # list to hold auto-defined missing instruments
    for(i in 1:length(instr_s)){
        instr <- getInstrument(instr_s[i], silent=TRUE)
        iauto <- NULL
        if(is.instrument(instr)){ 
            files.xts[files.xts$name.new ==instr_s[i],]$type <- paste(instr$type, collapse=";")
        } else {
            #print(paste(instr_s[i], 'does not appear to be an instrument, setting it to', default_type))
            iauto <- instrument.auto(instr_s[i], currency=default_currency, 
                                    default_type=default_type, assign_i=FALSE)
            if (!is.instrument(iauto)) {
                warning(paste("Could NOT create ", default_type, " from ", 
                            instr_s[i], ". Creating _NULL_ instrument instead.", sep=""))
                iauto <- suppressWarnings(instrument.auto(instr_s[i], currency=default_currency,
                                            default_type="NULL", assign_i=FALSE))
            }
            missing_list[[iauto$primary_id]] <- iauto
            #assign(iauto$primary_id, iauto, pos=missing_i_envir) 
            files.xts[files.xts$name.new==instr_s[i],]$type <- paste(iauto$type, collapse=";")
            missing_i <- c(missing_i, instr_s[i])
        }
    }

    # Remove everything from .instrument, put back the auto-defined missing instruments and save them
    try(rm_instruments(), silent=TRUE)
    lapply(missing_list, function(x) {
        assign(x$primary_id, x, pos=.instrument)
    })

    saveInstruments(paste("missing_instr",  format(Sys.time(), "%Y.%m.%d_%H%M%S"), sep='_'), path.output) #If you load this with loadInstruments it will not clobber .instrument
    # now that we've save only the newly defined instruments, we can load back our other instruments
    loadInstruments(instrument_file)
    if (!is.null(iauto)) {
        .TRTH$missing_i <- missing_i <- data.frame(symbol=missing_i,type=iauto$type[1]) #legacy
        write.csv(missing_i,file=paste(path.output,'missing_instruments.CSV',sep='')) 
    }
    .TRTH$files.xts <- files.xts
    assign('.TRTH', .TRTH, pos=.GlobalEnv)
    detach(.TRTH)
    .TRTH
}


FEreut2xts <- function(.TRTH) {
    if (missing(.TRTH) && !exists(".TRTH")) stop("Run configureTRTH function first")
    attach(.TRTH)
    # Make sure csv_dir exists since it is where we read the data from
    if (!file.exists(csv_dir)) stop("There is no directory", paste(csv_dir))
    if (!exists('files.xts')) stop("Cannot find 'files.xts' -- Run splitCSV first")
    oldTZ <- Sys.getenv("TZ")
    Sys.setenv(TZ='GMT')

    write.tick <- TRUE #if the tickdata file already exists and overwrite==FALSE, this will be set to FALSE
    write.sec <- TRUE #if the secdata file already exists and overwrite==FALSE, this will be set to FALSE

    nc <- nchar(path.output) # make sure path.output ends with a forward slash
    if(substr(path.output, nc, nc) != "/") path.output <- paste(path.output, "/", sep="") 

    # Function that we'll use to save charts of the data
    makeImages <- function(Data, dir, RIC, date) {
        stopifnot(file.exists(paste(dir, RIC, sep="")))
        ## Bid
        dir.create(paste(dir, RIC, "/Bid.Image/", sep=""), showWarnings=FALSE, mode='0775')
        png(filename=paste(dir,RIC,"/Bid.Image/",date,".",RIC,".png",sep=""),width=1500,height=1000)
        try(chartSeries(to.minutes(Data$Bid.Price,1),type="bar"),silent=TRUE)
        dev.off()
        ## Ask
        dir.create(paste(dir,RIC,"/Ask.Image/",sep=""), showWarnings=FALSE, mode='0775')
        png(paste(dir,RIC,"/Ask.Image/",date,".",RIC,".png",sep=""),width=1500,height=1000)
        try(chartSeries(to.minutes(Data$Ask.Price,1),type="bar"),silent=TRUE)
        dev.off()
        ## Price
        Data.1 <- Data[!is.na(Data$Price),]
        if(dim(Data.1)[1]>50)
        {
            dir.create(paste(dir,RIC,"/Price.Image/",sep=""), showWarnings=FALSE, mode='0775')
            png(paste(dir,RIC,"/Price.Image/",date,".",RIC,".png",sep=""),width=1500,height=1000)
            try(chartSeries(to.minutes(na.omit(Data$Price),1),type="bar"),silent=TRUE)
            dev.off()
        }
    }

    Out <- foreach(ii=icount(NROW(files.xts)), .inorder=FALSE, .errorhandling='pass') %dopar% {
        RIC=files.xts[ii, 1]
        date=files.xts[ii, 2] 
        type=files.xts[ii, 3]        

        file.name.xts <- paste(tick_dir, RIC, "/", date, ".", RIC, ".RData", sep="")
        file.name.sec <- paste(sec_dir, RIC, "/", date, ".", RIC, ".RData", sep="")	
        if(!isTRUE(overwrite)) {
            if (file.exists(file.name.xts)){
	            cat(paste(file.name.xts, "already exists, not overwriting\n"))
                write.tick <- FALSE
                tick.image <- FALSE
            }
            if (file.exists(file.name.sec)) {
            	cat(paste(file.name.sec, "already exists, not overwriting\n"))
                write.sec <- FALSE
                sec.image <- FALSE
            }
        }

        print(paste(date, RIC, paste(c("tick", "sec")[c(write.tick, write.sec)], collapse=" "), ii, "of", NROW(files.xts), sep=" "))

        # if xts and sec data already exist for this product/Date, and overwrite == FALSE, 
        # there is nothing to be done -- return NULL
        if (!any(c(write.tick, write.sec))) return(NULL) 

        CSV.name <- paste(csv_dir, date, '/', date, '.', RIC, '.csv', sep="")
        if (!file.exists(CSV.name) && file.exists(paste(CSV.name, ".gz", sep=""))) {
            #only zipped file on disk. We'll have to unzip.
            system(paste("gzip -d -f ", CSV.name, ".gz", sep=""))
        }
        Data <- read.csv(CSV.name, stringsAsFactors=FALSE,header=TRUE)
        # Now that we've read the CSV, zip it and delete original to conserve disk space
        # print(paste("zipping ", CSV.name, sep=""))
        system(paste("gzip -f ", CSV.name, sep=""))

        OTC.remove <- grep("IRGCOND",Data$Qualifiers)
        #OTC.remove <- c(OTC.remove,grep("High[USER]",Data$Qualifiers,fixed=TRUE))
        #OTC.remove <- c(OTC.remove,grep("Low[USER]",Data$Qualifiers,fixed=TRUE))
        OTC.remove <- c(OTC.remove, grep("[USER]", Data$Qualifiers, fixed=TRUE))

        if(substr(RIC,1,(nchar(RIC)-2))=="ICF"){OTC.remove <- NULL}
        if(substr(RIC,1,(nchar(RIC)-2))=="DOL"){OTC.remove <- NULL}
        if(dim(Data)[1]<=25){return(NULL)}

        ## Remove block trades
        if(length(OTC.remove)){Data <- Data[-OTC.remove, ]}

        index.new <- as.POSIXct(paste(Data$Date.G.,Data$Time.G,sep=" "),format="%d-%b-%Y%H:%M:%OS",tz="GMT")

        ## Force Everything to numerics <-- should not be necessary, but I'll leave it as is
        Data <- Data[,c("Price","Volume","Bid.Price","Bid.Size","Ask.Price","Ask.Size")]
        Data$Price <- as.numeric(Data$Price)
        Data$Volume <- as.numeric(Data$Volume)
        Data$Bid.Price <- as.numeric(Data$Bid.Price)
        Data$Bid.Size <- as.numeric(Data$Bid.Size)
        Data$Ask.Price <- as.numeric(Data$Ask.Price)
        Data$Ask.Size <- as.numeric(Data$Ask.Size)

        Data <- xts(Data,order.by=index.new,tz="GMT")

        ## Turn bids/offers that are less than zero into NA for outrights
        type <- unlist(strsplit(type, ";"))	
        if(!"synthetic" %in% type)
        { #outrights
            Data$Bid.Price[Data$Bid.Price < 0, ] <- NA
	        Data$Ask.Price[Data$Ask.Price < 0, ] <- NA
	        Data$Price[Data$Price < 0, ] <- NA
        } 

        ## If Bid.Price and Bid.Size are zero set both to NA
        zero.replace <- which(Data$Bid.Price == 0 & Data$Bid.Size == 0)
        if (length(zero.replace) != 0) {
            Data$Bid.Price[zero.replace] <- NA
            Data$Bid.Size[zero.replace] <- NA
        }
        ## Do same thing with Ask Price/Size
        zero.replace <- which(Data$Ask.Price == 0 & Data$Ask.Size == 0)
        if (length(zero.replace) != 0) {
            Data$Ask.Price[zero.replace] <- NA
            Data$Ask.Size[zero.replace] <- NA
        }

        ## Carry last bid/offer forward
        Data$Bid.Price <- na.locf(Data$Bid.Price)
        Data$Bid.Size <- na.locf(Data$Bid.Size)
        Data$Ask.Price <- na.locf(Data$Ask.Price)
        Data$Ask.Size <- na.locf(Data$Ask.Size)

        ## Remove Trades with Volume of zero
        Volume.remove <- which(Data$Volume == 0)
        if(length(Volume.remove) != 0) Data <- Data[-Volume.remove]

        if(dim(Data)[1]<=25){return(NULL)}

        ## Remove Price w/ Volume of NA and
        ## Volume w/ Price of NA	
        na.remove <- c(which(!is.na(Data$Price) & is.na(Data$Volume)),
        which(is.na(Data$Price) & !is.na(Data$Volume)))
        if (length(na.remove)!=0) {	Data <- Data[-na.remove] }

        ## not enough rows
        if(dim(Data)[1]<=10){return(NULL)}

        ## Remove leading NAs on Bid/Ask
        bid.remove <- which(is.na(Data$Bid.Price))
        ask.remove <- which(is.na(Data$Ask.Price))
        union.remove <- c(bid.remove,ask.remove)
        if(length(union.remove)>0){Data <- Data[-union.remove]}

        ## not enough rows
        if(dim(Data)[1]<=25){return(NULL)}

        if(write.tick) {
            dir.create(paste(tick_dir, RIC, sep=""), showWarnings=FALSE, mode='0775')
            assign(RIC, Data)  # Rename Data to RIC
            save(list=RIC, file=file.name.xts)
        }

        datarange <- range(index(Data),na.rm = TRUE)
        datarange.dif <- difftime(datarange[2],datarange[1],units="secs")
        if(isTRUE(tick.image) && datarange.dif>3600) makeImages(Data, tick_dir, RIC, date)

        # Convert to 1 second data and save
        if (write.sec) {
            dir.create(paste(sec_dir, RIC, "/", sep=""), showWarnings=FALSE, mode='0775')
            secData <- to_secBATV(Data)
            if (length(secData) == 0) return(NULL)
            assign(RIC, secData)
            save(list=RIC, file=file.name.sec)
        }
        if (isTRUE(sec.image) && datarange.dif > 3600) makeImages(Data, sec_dir, RIC, date)
    } # End foreach loop
    #rm(list = 'RIC')
    Sys.setenv(TZ=oldTZ)
    save(.TRTH, file=paste(path.output, 'config.env.RData', sep=""))
    Out
}  ## End fn reut2xts 

#
## now clean up
#files.rm <- list.files(archive_dir)
#files.rm <- files.rm[-grep(".csv.gz",files.rm)]
#files.rm <- files.rm[grep(".csv",files.rm)]
#file.remove(files.rm)
#file.remove('files.xts.tmp.rda')
#
#rm(missing_i)
##rm(Out)

###############################################################################
# Copyright (c) 2009-2011
# Peter Carl,  Brian G. Peterson, Lance Levenson, Joshua Ulrich, Garrett See
#
# This code is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

