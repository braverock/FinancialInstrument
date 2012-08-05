
minSpreadVarWeights <- function(series,var.method='var',interval=c(-10,10)) {
    #returns weight of 1st symbol.  Weight of 2nd Symbol = (1-weight of prc1)
    #if (length(symbols) < 2) stop("You must provide a list containing at least 2 instrument names")
    #if (length(symbols) > 2) warning("only using 1st 2 symbols")
    ab.prc<-series
    if (length(ab.prc) > 0) {
        if (var.method == 'sd') {
            f <- function(x) {
                sd(x * ab.prc[,1] - ((1-x) * ab.prc[,2]))
            }
        } else {
            f <- function(x) {
                var(x * ab.prc[,1] - ((1-x) * ab.prc[,2]))
            }
        }
        tmp <- optimize(f, interval)$minimum
        if(tmp < -10 || tmp > 10) warning('solution at extreme, use larger interval')

        #tmp <- nlm(f,guess)$estimate
        out <- c(tmp, 1-tmp)
    } else {
        out <- NA
    }
    out
}


#minSpreadVarRatio: solve for weights where variance of ( w1*p1 - (1-w2)*p2 ) is at a minimum
m1 <- minSpreadVarRatio <- function(series,var.method='var',interval=c(-10,10)) {
    #Returns number of 2nd symbol to short for 1 long in 1st symbol
    w <- minSpreadVarWeights(series,var.method=var.method,interval=interval)
    w[2]/w[1]
}

#stdVarRatio: qty of instrument 2 that you need for long 1 in instrument 1 = ((p1*var(p1))/(p2*var(p2)))
stdVarRatio <- function(priceSeries,retSeries,use.price='last',var.method='var') {
    #returns weight of 1st symbol.  Weight of 2nd Symbol = (1-weight of prc1)
    ab.prc<-priceSeries
    ab.ret<-retSeries
    if (length(ab.prc) > 0) {
        names(ab.prc) <- c('prc1','prc2')

        #if (use == 'price' | use =='Price') ab.ret <- ab.prc
        #else ab.ret <- diff(log(ab.prc))[-1]
        #returns qty of second instrument you should short for each unit of 1st instrument
        #implicicly assumes 100% correlation
        #I use var below because it's faster, but maybe sd is better
        if (var.method=='sd') {
            if (use.price=='first')	tmp <- as.numeric((first(ab.prc[,1]) * sd(ab.ret[,1]))/(first(ab.prc[,2]) * sd(ab.ret[,2])))
            else  tmp <- as.numeric((last(ab.prc[,1]) * sd(ab.ret[,1]))/(last(ab.prc[,2]) * sd(ab.ret[,2])))
        } else {
            if (use.price=='first')	tmp <- as.numeric((first(ab.prc[,1]) * var(ab.ret[,1]))/(first(ab.prc[,2]) * var(ab.ret[,2])))
            else  tmp <- as.numeric((last(ab.prc[,1]) * var(ab.ret[,1]))/(last(ab.prc[,2]) * var(ab.ret[,2])))
        }
    } else tmp <- NA
    tmp
}
m2 <- stdPriceVarRatio <-  function(priceSeries,retSeries,use.price='last',var.method='var')
    stdVarRatio(priceSeries=priceSeries,retSeries=retSeries,use.price=use.price,var.method='var')
m3 <- stdReturnVarRatio <-  function(priceSeries,retSeries,use.price='last',var.method='var')
    stdVarRatio(priceSeries=priceSeries,retSeries=retSeries,use.price=use.price,var.method='var')
m4 <- stdPriceStdevRatio <-  function(priceSeries,retSeries,use.price='last',var.method='sd')
    stdVarRatio(priceSeries=priceSeries,retSeries=retSeries,use.price=use.price,var.method='sd')
m5 <- stdReturnStdevRatio <-  function(priceSeries,retSeries,use.price='last',var.method='sd')
    stdVarRatio(priceSeries=priceSeries,retSeries=retSeries,use.price=use.price,var.method='sd')


#stdVarRatio(c('CO_4','QS_7'))

#beta (price,or return): regress instrument 2 on instrument 1. qty of instrument 2 that you need = coefficient of correlation
betaRatio <- function(series) {
    ab.prc <- series
    #merge(prc1,prc2,all=F)
    if (length(ab.prc) > 0) {

       # if (use=='return' | use=='ret') ab.prc <- diff(log(ab.prc),na.pad=FALSE)
       # if (use=='change' | use=='chg') ab.prc <- diff(ab.prc,na.pad=FALSE)
        if (length(ab.prc[!any(is.nan(ab.prc))]) > 0) {
            names(ab.prc) <- c('prc1','prc2')
            m <- lm(prc1 ~ prc2 + 0, data=ab.prc)
            tmp <- as.numeric(coef(m)[1])
            #equivalently:
            #tmp <- as.numeric(cov(ab.prc[,1],ab.prc[,2])/var(ab.prc[,2]))
        } else tmp <- NA
    } else tmp <- NA
    tmp
}
regPriceRatio <- function(series) betaRatio(series,use=use) #==?
regPrcChgRatio <- function(series) betaRatio(series,use=use) #==onChg
regReturnRatio <- function(series) betaRatio(series,use=use) #==onLogRets


pcaRatio <- function(series) {
    ab.prc <- series

    if (length(ab.prc[!any(is.nan(ab.prc))]) > 0) {

        names(ab.prc) <- c('prc1','prc2')
        r <- princomp(ab.prc)
        tmp <- r$loadings[2,1] / r$loadings[1,1] #slope
    } else tmp <- NA
    tmp
}


#equalizedRatio: solve for ratio that makes price of spread at first time equal to price of spread at last time.
equalizedRatio <- function(series) {
    ab.prc <- series

    if (length(ab.prc) > 0) {
        names(ab.prc) <- c('prc1','prc2')

        tmp <- (as.numeric(first(ab.prc[,1]))-as.numeric(last(ab.prc[,1])))/
                (as.numeric(first(ab.prc[,2]))-as.numeric(last(ab.prc[,2])))
    } else tmp <- NA
    tmp
}

MADratio<-function(barData){
   #prc<-getPrice(rawdata)
   #ohlcprc<-to.period(rawdata,on=minutes',k=10)
   #ohlcprc$range<-abs(Hi(ohlcprc)-Lo(ohlcprc))
   colnames(barData)<-c("O1","H1","L1","C1","O2","H2","L2","C2")
   barData$Range1=abs(barData$H1-barData$L1)
   barData$Range2=abs(barData$H2-barData$L2)
   Ratio=mean(barData$Range1)/mean(barData$Range2)
   Ratio
}

covRatio <-function(series) {
  xtmp<-cov(series)
  covRat<-sqrt(xtmp[1,1])/sqrt(xtmp[2,2])
  covRat
}

#' The calcHedgeRatio function takes in data on symbols, a vector of choices of methods to calculate hedge ratios, and a price.method ('price','return', or 'diff').
#' Some other arguments include the on and k arguments, with on taking an argument of a certain frequency ('microseconds','milliseconds','seconds',minutes',etc.) and k stating how many units of the on argument pass between each print.
#' The type of price the time series data will use gets subset by prefer.price.
#' Use.price determines whether open ('first') prices are used, or anything else (defaults at 'last')
#' Interval is for the minVar/minSd functions only.
#' from and to set the date
#' these are the different hedge ratio options:
#' minVar/minSd: spread = [weight(A) * price.method(A) * multiplier(A)] - [(1-weight(A)) * price.method(B) * multiplier(B) ]
#' solve for the value of weight(A) that minimizes the variance/sd of spread. hedgeRatio=weight(B)/weight(A)
#'  'stdVar'/'stdDev'
#' 	calculate the variances of price.method(A) and price.method(B) and set equal to var(A) and var(B)
#' 	hedgeRatio = [last(price.method(A)) * var(A) * multiplier(A)] / [last(price.method(B)) * var(B) * multiplier(B)]
#' where last(price.method(A)) and last(price.method(B)) are the most recent price.methods of A and B.
#' stdDev is identical, except with var being sd
#' 'betaRatio'
#' 	regress price.method of B on price.method of A. hedgeRatio is the coefficient
#' 	set up model like this  price.method(A) ~ price.method(B) + 0
#'  pcaRatio--ratio of principal component price methods
#'  equalizedRatio--ratio that has sread values be identical on the first and last days of the sample.  Performance drops on out of sample data.  Use with caution.
#'  MADRatio--Mean Absolute Deviation--a ratio of ranges of the instruments for the specified time frame.
#'  covRatio--computes the ratio of standard deviations
#'  
#' @param symbols 
#' @param methods 
#' @param interval 
#' @param use.price 
#' @param price.method 
#' @param prefer.price 
#' @param on period to calculate on, see \code{\link{endpoints}}
#' @param k number of sub periods to aggregate on
#' @param from Retrieve data no earlier than this date.
#' @param to Retrieve data through this date
#' @param session xts style ISO-8601 time subsetting for intraday sessions
#' @param \dots any other passthru parameters
#' @author Ilya Kipnis, Garrett See, Brian G. Peterson
#' @export
calcHedgeRatio<-function(symbols,
        methods=c("minVar","minSd","stdVar","stdDev","betaRatio","pcaRatio","equal","MADratio","covRatio"),
        # methods=c("minVar"),
        interval=c(-10,10),
        use.price='last',
        price.method='price',
        prefer.price=NULL,
        on="minutes",k=1,
        from='1970-01-01',
        to=Sys.Date()-1,
        session=NULL,
        ...
)
{

    if (length(symbols) < 2) stop("You must provide a list containing at least 2 instrument names")
    if (length(symbols) > 2) warning("only using 1st 2 symbols. patches welcome.")

    mult <- NULL
    for (Symbol in symbols) {
        tmp_instr <- try(getInstrument(Symbol))
        if (inherits(tmp_instr, "try-error") | !is.instrument(tmp_instr)) {
            warning(paste("Instrument", Symbol, " not found, using contract multiplier of 1"))
            mult <- c(mult,1)
        } else {
            mult <- c(mult,tmp_instr$multiplier)
        }
    }
    
    prc1 <- getSymbols(symbols[1], from=from, to=to, auto.assign=FALSE, ...=...)
    prc2 <- getSymbols(symbols[2], from=from, to=to, auto.assign=FALSE, ...=...)
    
    if(is.null(prefer.price)){
        if(is.BBO(prc1)){
            prc1<-getPrice(prc1,NULL,prefer="Bid")
            prc2<-getPrice(prc2,NULL,prefer="Bid")
        } else {
            prc1<-getPrice(prc1,NULL,prefer="Ad")
            prc2<-getPrice(prc2,NULL,prefer="Ad")
        }
    } else {
        prc1<-getPrice(prc1,NULL,prefer=prefer.price)
        prc2<-getPrice(prc2,NULL,prefer=prefer.price)
    }

    DF<-cbind(prc1,prc2)

    fn_split_highFreq <- function(DF,session=NULL)
    {
        DF.split <- split(DF,"days")
        ret <- NULL

        for(d in 1:length(DF.split))
        {
            tmp <- na.locf(DF.split[[d]])
            tmp <- na.omit(tmp)
            ret <- rbind(ret,tmp)
        }
        #attr(attr(ret,"index"),"tzone") <- "GMT" # no longer needed?
        #attr(ret,".indexTZ") <- "GMT" # no longer needed?
        colnames(ret) <- colnames(DF)
        if(!is.null(session)) ret<-ret[session]
        ret
    }

    fn_split_daily <- function(DF)
    {
        ret<-NULL
        tmp<-na.locf(DF)
        tmp<-na.omit(tmp)
        ret<-tmp
        colnames(ret)<-colnames(DF)
        ret
    }

    if(median(diff(.index(prc1)))<86400){
        series<-fn_split_highFreq(DF,session)
    } else {
        series<-fn_split_daily(DF)
    }

    #notionalize the series
    series[,1]<-series[,1]*mult[1]
    series[,2]<-series[,2]*mult[2]

    #TODO FIXME convert one series if the currencies aren't the same
    
    OHLCprc1<-to.period(series[,1],period=on,k=k)
    OHLCprc2<-to.period(series[,2],period=on,k=k)
    barData<-cbind(OHLCprc1,OHLCprc2)

    series<-series[endpoints(series,on=on,k=k),]
    
    priceSeries<-series #for use with stdVarRatio
    
    if(length(price.method)!=1){stop("Your price method must be of length one.")}
    if(price.method=='price'|price.method=='Price'|price.method=='prc'){
      series<-series #do nothing
      barData<-barData
    } else if(price.method=='return'|price.method=='Return'|price.method=='ret'){
        series<-na.omit(diff(log(series)))
        barData<-na.omit(diff(log(barData)))
    } else {
        series<-na.omit(diff(series))
        barData<-na.omit(diff(barData))
    }
    
    if(length(methods)<=0) stop("You must provide at least one method")

    methodSwitch<-function(series,method){
        switch(method,
                minVar = minSpreadVarWeights(series=series,var.method='var',interval=interval),
                minSd = minSpreadVarWeights(series=series,var.method='sd',interval=interval),
                stdVar = stdVarRatio(priceSeries=priceSeries,retSeries=series,use.price=use.price,var.method='var'),
                stdDev = stdVarRatio(priceSeries=priceSeries,retSeries=series,use.price=use.price,var.method='sd'),
                betaRatio = betaRatio(series),
                pcaRatio = pcaRatio(series),
                equal = equalizedRatio(series),
                MADratio = MADratio(barData),
                covRatio = covRatio(series)
        )
    }

    ansRow<-NULL
    ansColNames<-c()
    for(method in methods){
        ansColNames<-c(ansColNames,method)
        ansRow<-cbind(ansRow,methodSwitch(series,method))
    }
    ansRow<-as.data.frame(ansRow)
    colnames(ansRow)<-ansColNames

    if(length(which(ansColNames=="minVar")>0)){ #is the minVar method in there?
        ansRow$minVar<-ansRow$minVar/ansRow$minVar[1] #if so, set it to 1 and HR
    }

    if(length(which(ansColNames=="equal")>0)){ #same deal with equal
        ansRow$equal[1]=1
        ansRow$equal[2]=ansRow$equal[2]+1
    }

    for(i in 1:length(ansRow[1,])){ #the rest just give the same output twice
        ansRow[1,i]=1
    }

    ansRow<-cbind(symbols,ansRow)
    ansRow$symbols<-as.character(ansRow$symbols)

    return(ansRow)

}