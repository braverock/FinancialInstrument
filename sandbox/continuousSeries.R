

#' continuous series 
#' @param data 
#' @param file 
#' @param dirPath 
#' @param extension 
#' @param separator 
#' @param outputFile 
#' @param rollMethod 
#' @param bindMethod 
#' @author Ilya Kipnis <ilya<dot>kipnis<at>gmail<dot>com>
#' @export
continuousSeries<-function(data=NULL,file=NULL,dirPath=NULL,extension=".csv",separator=",",outputFile=NULL,rollMethod="OICross",bindMethod="splooth"){

    if(!is.null(data)) {     #works differently depending on input
    	Data<-data
    } else if(!is.null(file)) {
    	Data<-read.csv(file,header=TRUE,stringsAsFactors=FALSE,sep=separator)
    } else if(!is.null(dirPath) & !is.null(extension)){ #needs an extension of your filetype, defaults to csv, but can also be ".txt" or whatever else you have read into R before.
        setwd(dirPath)                                  #not sure if xls works--if you're working with spreadsheets, I recommend saving your files to csv types.
        directory<-list.files(getwd(),pattern=extension)
        Data<-NULL
        for(i in 1:length(directory)){
         workingFile<-read.table(directory[i],header=TRUE,stringsAsFactors=FALSE,sep=separator)
         Data<-rbind(Data,workingFile)
        }
    } else {
    		stop("you must either pass a data.frame as the 'data' parameter, the 'file' parameter, or the directory with its extension")
    }
    
    #Data<-read.csv("CLseries2011.csv",stringsAsFactors=FALSE,header=TRUE)
    #Data<-Data[-which(Data$Qualifiers=="No Trades"),]
    Data<-Data[which(Data$Open.Interest>0),] #don't need the No Trades since these filters takes care of it
    Data<-Data[which(Data$Volume>0),]
    
    completeFrame<-NULL
    Data<-split(Data,Data$X.RIC)
    for(i in 1:(length(Data)-1)){
     #print(i)
     monthOne<-Data[[i]]
     monthTwo<-Data[[i+1]]
     monthOne<-monthOne[which(monthOne$Date.L.%in%monthTwo$Date.L.),]    #grab month i and the next month, get their common dates
     monthTwo<-monthTwo[which(monthTwo$Date.L.%in%monthOne$Date.L.),]
     monthOne$trailMonthLast<-monthTwo$Last
    
     if(rollMethod=="OICross"){
      rollVec<-which(monthTwo$Open.Interest>monthOne$Open.Interest)   #set the roll day to be the desired cross which occurs no earlier than 10 days before expiry
      rollVec<-rollVec[which(rollVec>=(nrow(monthOne)-10))]
      rollDay<-rollVec[1]
     } else if(rollMethod=="VCross"){
      rollVec<-which(monthTwo$Volume>monthOne$Volume)
      rollVec<-rollVec[which(rollVec>=(nrow(monthOne)-10))]
      rollDay<-rollVec[1]
     } else {
      rollDay<-(nrow(monthOne)-5)   #or if the roll date isn't specified by a certain metric cross, just have it be 5 days before expiry
     }
     
     if(is.na(rollDay)){        #if a cross method is chosen and the cross hasn't occured yet, set it to be the last day of the series
      rollDay<-(nrow(monthOne))
     }
     
     monthOne<-monthOne[1:rollDay,]
     
     if(i>1){
       removeDates<-which(monthOne[,which(substr(colnames(monthOne),1,4)=="Date")] %in% completeFrame[,which(substr(colnames(monthOne),1,4)=="Date")])  #remove days that have already been accounted for earlier
       monthOne<-monthOne[-(removeDates[1]:removeDates[length(removeDates)]),]
     }
    
    if(nrow(monthOne)>0){    #make sure that there's data left to bind or do calculations on
        if(bindMethod=="splooth"){
          closeGap<-monthOne$trailMonthLast[nrow(monthOne)]-monthOne$Last[nrow(monthOne)]   #splooth a bunch of things for the closeGap (difference in close prices on roll day between front month and next month)
          #print(closeGap)
          monthOne$Adj.Open<-monthOne$Open+closeGap
          monthOne$Adj.High<-monthOne$High+closeGap
          monthOne$Adj.Low<-monthOne$Low+closeGap
          monthOne$Adj.Last<-monthOne$Last+closeGap
          monthOne$Adj.Settle<-monthOne$Settle+closeGap
          monthOne$Adj.trailMonthLast<-monthOne$trailMonthLast
          completeFrame$Adj.Open<-completeFrame$Open+closeGap
          completeFrame$Adj.High<-completeFrame$High+closeGap
          completeFrame$Adj.Low<-completeFrame$Low+closeGap
          completeFrame$Adj.Last<-completeFrame$Last+closeGap
          completeFrame$Adj.Settle<-completeFrame$Settle+closeGap
          completeFrame$Adj.trailMonthLast<-completeFrame$trailMonthLast+closeGap
          completeFrame<-rbind(completeFrame,monthOne)
        } else {
          completeFrame<-rbind(completeFrame,monthOne)
        }
      }
     
    }
     
 	if(!is.null(outputFile)){
		write.csv(completeFrame,outputFile) #This is where I write my output.
	} else {
		return(completeFrame)
	}
 
}

###############################################################################
# R (http://r-project.org/) Instrument Class Model
#
# Copyright (c) 2009-2011
# Peter Carl, Dirk Eddelbuettel, Jeffrey Ryan, Joshua Ulrich and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################

