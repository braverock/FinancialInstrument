##############################################################################
#                  Reuters Backfill Configuration Parameters                 #         
##############################################################################
#
# You should create a configuration file that includes the following
#
#instrument_file <- '/full/path/to/instruments.rda' #where your instrument metadata is stored
#
#archive_dir <- "/full/path/to/archives/" # where the split CSV, job reports, and gz files will be stored
#path.output <- "/full/path/to/output/" # root dir where the .rda files for the xts will be placed
#
#job.name <- "mybackfill" # the Reuters TRTH job name
#
#username <- "username" #TRTH user name, usually your email address
#password <- "password" #TRTH password
#image <- TRUE
#default_type='future_series'
#default_type='guaranteed_spread'
#
#email_to <- 'someuser@somehost.com'
#email_from <- 'someuser@somehost.com'
#
#overwrite <- FALSE # whether to overwrite files from the same date, default FALSE
#image <- TRUE # whether to write write Bid, Ask, and Price images to disk for diagnostics, default TRUE 
#
#no.cores <- 1 # for foreach
#
# # Also add any additional options that would make this script run better in your environment
#
##############################################################################
config_file<-'TRTH_config_file.R'
source(config_file)

load(instrument_file)

Sys.umask("0002")

options(width=200)
Date <- Sys.Date()

require(xts)
require(quantmod)
require(FinancialInstrument)
require(doMC)
#require(sendmailR) # for email on failure
#error.codes<-read.csv('curl.errors.csv',stringsAsFactors=FALSE,header=TRUE,row.names=1)

registerDoMC(no.cores)
# registerDoSEQ()

Archive.output <- list.files(archive_dir)
Archive.output <- Archive.output[grep(".gz",Archive.output)]
Archive.output <- Archive.output[-c(grep("confirmation",Archive.output),grep("report",Archive.output))]

listflag=FALSE
while(!listflag)#try to download file list 
{
    clear<-warnings() #currency loads from oanda alway generate warnings, clear them out
    Reuters <- system(paste("curl ftp://tickhistory-ftp.thomsonreuters.com:15500/results/ -u ",username,":",password," --ftp-ssl -k -l",sep=""),intern=TRUE)
    w<-''
    w<-warnings()
    if(!as.logical(length(Reuters)) || isTRUE(grep('curl',names(w))))
    {
        tmpmsg<-paste("curl returned error code", names(w),'\n','while attempting to download file list','\n','script will wait and retry in 30 min')
        #sendmail(email_to,email_from,"error downloading Reuters file list",msg=tmpmsg)
        Sys.sleep(180)
    } else listflag=TRUE
    
}

# now we're past the while loop, so we have a file list
Reuters.report <- Reuters[grep("report",Reuters)]
Reuters.output <-  Reuters[-c(grep("report",Reuters),grep("confirmation",Reuters))]
Reuters.output <-  Reuters.output[grep(job.name,Reuters.output)]

Reuters.new <- Reuters.output[!(Reuters.output %in% Archive.output)]

# or to start here in the script from the file list
# (see comment above about fixing broken set)
# Reuters.output <-  Archive.output[grep(job.name,Archive.output)]
# Reuters.new <- Reuters.output


for(i in 1:length(Reuters.new))
{	
	filename.gz <- Reuters.new[i]
	filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))
	
	alias <- unlist(strsplit(filename.gz,"-"))[3]
	alias <- unlist(strsplit(alias,".csv.gz"))
	
	## Download New Datasets
	print(paste("Downloading ",filename.gz,sep=""))
    fileflag=FALSE	
    while(!fileflag) #try to download individual files
    {
        Reuters2<-system(paste("curl -m 10800 --max-filesize 1610612736 ftp://tickhistory-ftp.thomsonreuters.com:15500/results/",filename.gz, " -u ",username,":",password," --ftp-ssl -k > ",archive_dir,"/",filename.gz,sep=""))
        if(Reuters2!=0)
        {
            w2<-''
            w2<-warnings()
            tmpmsg<-paste("curl returned error code", Reuters2,"\n",w2,'\n','while attempting to download',filename.gz,'\n','will wait and retry in 10 min')
            #sendmail(email_to,email_from,paste("error downloading Reuters file",filename.gz),msg=tmpmsg)
            Sys.sleep(600)
        } else fileflag=TRUE
    }
    
	## Download Report s
	system(paste("curl ftp://tickhistory-ftp.thomsonreuters.com:15500/results/",Reuters.report[grep(alias,Reuters.report)], " -u ",username,":",password," --ftp-ssl -k > ",archive_dir,"/",Reuters.report[grep(alias,Reuters.report)],sep=""))
	#system(paste("gzip -d -f ",archive_dir,"Report/",Reuters.report[grep(alias,Reuters.report)],sep=""))
	
}	

## now unzip, split, rezip
setwd(archive_dir)
#foreach(j=1:length(Reuters.new)) %dopar%
if (!length(Reuters.new)){
    stop('you have no files to download, maybe you need to define a file list manually and run from there?')
} else for(j in 1:length(Reuters.new))
{
	
	#	system(paste("split --lines=10000000 -d ",filename.csv," split.",sep=""))
	#	files.split <- list.files("/home/mktdata/ReutersData/Archives", pattern="split.")
	
	filename.gz <- Reuters.new[j]
	filename.csv <- substr(filename.gz,1,(nchar(filename.gz)-3))

	#unzip the file
	print(paste("unzipping ",filename.gz, sep=""))
	system(paste("gzip -d -f ",archive_dir,'/',filename.gz,sep=""))
	
	alias <- unlist(strsplit(filename.gz,"-"))[3]
	alias <- unlist(strsplit(alias,".csv.gz"))
		
	## Split the Files 
	## command line awkstring would look like this:
	#awk -F "," '{print >>$1"."$2".csv"}' sourcefile.csv
	#
	## modified to deal with 'too many open files', thanks to Josh Ulrich
	#	awk -v f2="" -F "," '{
	#			f1 = $1"."$2".csv";
	#			print >> f1;
	#			if(f1 != f2) {
	#			close(f2);
	#			f2=f1;
	#			}
	#			}' sourcefile.csv
	## NOTE: if you get errors on 'too many open files' from awk, you'll need to adjust ulimit/nolimit
	print(paste("Splitting ",filename.csv,sep=""))
	#system(paste('awk -F "," ',"'{print >> $1", '"."$2".csv','"}',"' ",filename.csv, sep=""))
	system(paste('awk -v f2="" -F "," '," '",'{
			f1 = $1"."$2".csv";
			print >> f1;
			if(f1 != f2) {
			close(f2);
			f2=f1;
			}
			}',"' ",filename.csv, sep="")) #Improved awk w/ file close thanks to Josh Ulrich
	## Zip the File
	print(paste("zipping ",filename.csv,sep=""))
	system(paste("gzip -f ",archive_dir,'/',filename.csv,sep=""))
	
}	

#initialize output list empty
files.xts <- NULL

#build the file list to work on
files.csv <- list.files(archive_dir)
files.csv <- files.csv[-grep(".csv.gz",files.csv)]
files.csv <- files.csv[grep(".csv",files.csv)]
files.header <- files.csv[grep("RIC",files.csv)]
files.csv <- files.csv[-grep("RIC",files.csv)]

for(k in 1:length(files.csv))
{
	#print(k)
	name.csv <- files.csv[k]
	name <- unlist(strsplit(name.csv,".",fixed=TRUE))[1]
	RIC.date <- try(as.Date(unlist(strsplit(name.csv,".",fixed=TRUE))[2], format="%d-%b-%Y"))
    #if this failed, see if it is one of Reuters .O or similar symbols
    if(class(RIC.date)=='try-error') {
        RIC.date <- try(as.Date(unlist(strsplit(name.csv,".",fixed=TRUE))[3], format="%d-%b-%Y"))
        if(!class(RIC.date)=='try-error'){
            tmp<-unlist(strsplit(name.csv,".",fixed=TRUE))
            name <- paste(tmp[1],tmp[2],sep=.)
        } else {
            print(paste('File',name.csv,'could not be converted.'))
            next
        }
    }
	date.format <- gsub("-",".",RIC.date)
	
	try(if( weekdays(RIC.date)=="Saturday" | weekdays(RIC.date)=="Sunday")
	{
		file.remove(paste(archive_dir,name.csv,sep=""))
		next
				
	})
		
	## Handle leading digits and VIX and Cash
	if(substr(name,1,1)==1){name.new <- substr(name,2,nchar(name));name.new <- make.names(name.new)}
    else{name.new <- make.names(name)}
	
	## Does directory exist?
	ex <- file.exists(paste(path.output,date.format,"/",sep=""))
	if(ex != TRUE){dir.create(paste(path.output,date.format,"/",sep=""), mode="775")}
	
	## Move files to appropriate place
	#system(paste("mv -f ", path.output,"Archives/",name.csv, " ", path.output,date.format,"/",date.format,".",name.new,".csv", sep=""))
	system(paste("mv -f ", name.csv, " ", path.output,date.format,"/",date.format,".",name.new,".csv", sep=""))
	
	print(paste(date.format, name.new, "moved", sep=" "))
	files.xts <- rbind(files.xts,as.data.frame(cbind(name.new,date.format),stringsAsFactors=FALSE))
	
}

# now get instrument data
files.xts$type<-rep(NA,nrow(files.xts))
missing_i<-''
instr_s<-unique(files.xts[,'name.new'])
for(i in 1:length(instr_s)){
    instr<-suppressWarnings(getInstrument(instr_s[i]))
    if(is.instrument(instr)){ 
        files.xts[files.xts$name.new ==instr_s[i],]$type<-as.character(instr$type[1])
    } else {
        print(paste(instr_s[i], 'does not appear to be an instrument, setting it to', default_type))
        files.xts[files.xts$name.new==instr_s[i],]$type<-default_type
        missing_i<-c(missing_i,instr_s[i])
    }
}
missing_i<-missing_i[-1]
missing_i<-data.frame(symbol=missing_i,type=default_type)
write.csv(missing_i,file=paste(archive_dir,'missing_instruments.CSV',sep='')) 

##If trying to fix a broken set:
#files.csv<-'';for(dir in list.files(getwd(),pattern="20")) {files.csv<-c(files.csv,list.files(paste(getwd(),'/',dir,'/',sep=''),pattern=".csv"))}[-1]
#files.xts<-NULL
#for(l in 1:length(files.csv)) { rsplit<-as.vector(strsplit(files.csv[l],'.',fixed=TRUE)[[1]]); files.xts<-rbind(files.xts,cbind(rsplit[4],paste(rsplit[1],rsplit[2],rsplit[3],sep='.'))); print(files.xts[l,])}
#colnames(files.xts)<-c('name.new','date.format')

save(files.xts,file='files.xts.tmp.rda')

H <- read.csv(paste(path.output,"Archives/#RIC.Date[G].csv",sep=""),header=FALSE,stringsAsFactors=FALSE)
H <- H[nrow(H),]
H <- make.names(H)

## Into xts###############################################################
reut2xts <- function( data, datapath, image=TRUE, overwrite=FALSE, xFUN=NULL )
{
	prod <- data[,'name.new']
	date <- data[,'date.format']
    type <- data[,'type']
    
	print(paste(date, prod, "xts", sep=" "))
	
	RIC.code <- prod
	
	file.name <- paste(datapath,"xts/",RIC.code,"/",date,".",RIC.code,".RData",sep="")
	if(!isTRUE(overwrite) && file.exists(file.name)){
		return(paste(file.name,"already exists, not overwriting"))	
	}

	Data <- read.csv(paste(datapath,date,'/',date,'.',prod,'.csv',sep=''),stringsAsFactors=FALSE,header=FALSE)
    if(ncol(Data)!=length(H)){
        warning("length of headers and downloaded data do not match, trying to adjuust, but be careful!")
        H<-H[1:ncol(Data)]
    }
	names(Data) <- H
	
	OTC.remove <- grep("IRGCOND",Data$Qualifiers)
	OTC.remove <- c(OTC.remove,grep("High[USER]",Data$Qualifiers,fixed=TRUE))
	OTC.remove <- c(OTC.remove,grep("Low[USER]",Data$Qualifiers,fixed=TRUE))
	
	if(substr(prod,1,(nchar(prod)-2))=="ICF"){OTC.remove <- NULL}
	if(substr(prod,1,(nchar(prod)-2))=="DOL"){OTC.remove <- NULL}
	if(dim(Data)[1]<=25){return(NULL)}
	
	index.new <- as.POSIXct(paste(Data$Date.G.,Data$Time.G,sep=" "),format="%d-%b-%Y%H:%M:%OS",tz="GMT")
	
	## Force Everything to numerics
	Data <- Data[,c("Price","Volume","Bid.Price","Bid.Size","Ask.Price","Ask.Size")]
	Data$Price <- as.numeric(Data$Price)
	Data$Volume <- as.numeric(Data$Volume)
	Data$Bid.Price <- as.numeric(Data$Bid.Price)
	Data$Bid.Size <- as.numeric(Data$Bid.Size)
	Data$Ask.Price <- as.numeric(Data$Ask.Price)
	Data$Ask.Size <- as.numeric(Data$Ask.Size)
	
	Data <- xts(Data,order.by=index.new,tz="GMT")
	
	## Remove block trades
	if(length(OTC.remove)){Data <- Data[-OTC.remove]}

	## Turn bids/offers that are zero or less into NA for outrights
	
	if(type !="guaranteed_spread")
	{
        #remove prints with Prices less than or equal to zero
		Data$Bid.Price[Data$Bid.Price<=0,] <- NA
		Data$Ask.Price[Data$Ask.Price<=0,] <- NA
		Data$Price[Data$Ask.Price<=0,] <- NA
        ## Remove Trades with Price or Volume of zero   
        Price.remove <- which(Data$Price == 0)    
    } else {
        Price.remove<-NULL
    }
    
	## Carry last bid/offer forward
	
	Data$Bid.Price <- na.locf(Data$Bid.Price)
	Data$Ask.Price <- na.locf(Data$Ask.Price)
	
	Data$Bid.Size <- na.locf(Data$Bid.Size)
	Data$Ask.Size <- na.locf(Data$Ask.Size)
	
    ## Remove empty Trade rows (Volume of zero)   
    Volume.remove <- which(Data$Volume == 0)
	
	if(length(c(Price.remove,Volume.remove))!=0)
	{
		Data <- Data[-c(Price.remove,Volume.remove)]
	}
	if(dim(Data)[1]<=25){return(NULL)}
	
	
	## Remove Price w/ Volume of NA and
	## Volume w/ Price of NA	
    na.remove <- c(which(!is.na(Data$Price) & is.na(Data$Volume)),
    which(is.na(Data$Price) & !is.na(Data$Volume)))
    if (length(na.remove)!=0) {	Data <- Data[-sort(na.remove)] }
	
	## not enough rows
	if(dim(Data)[1]<=10){return(NULL)}
	
	## Remove leading NAs on Bid/Ask
	bid.remove <- which(is.na(Data$Bid.Price))
	ask.remove <- which(is.na(Data$Ask.Price))
	union.remove <- c(bid.remove,ask.remove)
	if(length(union.remove)>0){Data <- Data[-union.remove]}
	
	## not enough rows
	if(dim(Data)[1]<=25){return(NULL)}
	
	## Rename Data to RIC code
	assign(RIC.code,Data)
	
	## Does xts directory exist?
	ex <- file.exists(paste(datapath,"xts/",prod,"/",sep=""))
	if(!ex){dir.create(paste(datapath,"xts/",prod,"/",sep=""),mode="775")}
	
	
	file.name <- paste(datapath,"xts/",RIC.code,"/",date,".",RIC.code,".RData",sep="")
	if(!isTRUE(overwrite) && file.exists(file.name)){
		return(paste(file.name,"already exists, not overwriting"))	
	} else {
		file.text <- paste("save(",RIC.code," ,file='",file.name,"')",sep="")
		if(!file.exists(paste(datapath,"xts/",RIC.code,sep=""))){dir.create(paste(datapath,"xts/",RIC.code,sep=""),mode="775")}
		eval(parse(text=file.text))
	}
	
	datarange <- range(index(Data),na.rm = TRUE)
	datarange.dif <- difftime(datarange[2],datarange[1],units="secs")
	
    #do we need to run some custom post-process parsing?
    if(!is.null(xFUN) && is.function(try(get(xFUN),silent=TRUE))){
        #run any additional custom, local function required by the user
        do.call(osFUN)
    }
	
    if(isTRUE(image) && datarange.dif>3600)
	{
		## Bid
		if(!file.exists(paste(datapath,"xts/",RIC.code,"/Bid.Image/",sep=""))){dir.create(paste(datapath,"xts/",RIC.code,"/Bid.Image/",sep=""),mode="775")}
		png(filename=paste(datapath,"xts/",RIC.code,"/Bid.Image/",date,".",RIC.code,".png",sep=""),width=1500,height=1000)
		try(chartSeries(to.minutes(Data$Bid.Price,1),type="bar"),silent=TRUE)
		dev.off()
		
		file.copy(paste(datapath,"xts/",RIC.code,"/Bid.Image/",date,".",RIC.code,".png",sep=""),paste(datapath,"Archives/Bid.Image.tmp/",date,".",RIC.code,".png",sep=""))
		
		## Ask
		if(!file.exists(paste(datapath,"xts/",RIC.code,"/Ask.Image/",sep=""))){dir.create(paste(datapath,"xts/",RIC.code,"/Ask.Image/",sep=""),mode="775")}
		png(paste(datapath,"xts/",RIC.code,"/Ask.Image/",date,".",RIC.code,".png",sep=""),width=1500,height=1000)
		try(chartSeries(to.minutes(Data$Ask.Price,1),type="bar"),silent=TRUE)
		dev.off()
		
		file.copy(paste(datapath,"xts/",RIC.code,"/Ask.Image/",date,".",RIC.code,".png",sep=""),paste(datapath,"Archives/Ask.Image.tmp/",date,".",RIC.code,".png",sep=""))
		
		## Price
		Data.1 <- Data[!is.na(Data$Price),]
		if(dim(Data.1)[1]>50)
		{
			if(!file.exists(paste(datapath,"xts/",RIC.code,"/Price.Image/",sep=""))){dir.create(paste(datapath,"xts/",RIC.code,"/Price.Image/",sep=""),mode="775")}
			png(paste(datapath,"xts/",RIC.code,"/Price.Image/",date,".",RIC.code,".png",sep=""),width=1500,height=1000)
			try(chartSeries(to.minutes(na.omit(Data$Price),1),type="bar"),silent=TRUE)
			dev.off()
			
			file.copy(paste(datapath,"xts/",RIC.code,"/Price.Image/",date,".",RIC.code,".png",sep=""),paste(datapath,"Archives/Price.Image.tmp/",date,".",RIC.code,".png",sep=""))
		}
	}
	
	rm(list = paste(RIC.code,sep=""))
	
}  ## End fn reut2xts 

Out <- foreach(ii=iter(1:nrow(files.xts)),.errorhandling='pass') %dopar% reut2xts(files.xts[ii,,drop=FALSE],datapath=path.output, image=image, overwrite=overwrite)

# now clean up
files.rm <- list.files(archive_dir)
files.rm <- files.rm[-grep(".csv.gz",files.rm)]
files.rm <- files.rm[grep(".csv",files.rm)]
file.remove(files.rm)
file.remove('files.xts.tmp.rda')

rm(missing_i)
rm(Out)

###############################################################################
# Copyright (c) 2009-2011
# Peter Carl,  Brian G. Peterson, Lance Levenson 
#
# This code is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

