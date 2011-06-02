# Script for parsing DJUBS index daily price data series from the
# DJ website.

# Peter Carl

# DETAILS
# Parse index close prices from the spreadsheet containing the full series:
# http://www.djindexes.com/mdsidx/downloads/xlspages/ubsci/DJUBS_full_hist.xls


# Several series, all index values
# Remove the footer at the bottom
# Load needed packages:
require(zoo)
require(gdata)

# Set the working directory, where there's a .incoming folder that contains
# the downloaded spreadsheet.
filesroot = "~/Data/DJUBS"

# Create and set the working directory if it doesn't exist
if (!file.exists(filesroot))
  dir.create(filesroot, mode="0777")

# Create and set the .incoming directory if it doesn't exist
if (!file.exists(paste(filesroot, "/.incoming", sep="")))
  dir.create(paste(filesroot, "/.incoming", sep=""), mode="0777")
setwd(paste(filesroot, "/.incoming", sep=""))

# Download the xls workbook directly from the web site:
system("wget http://www.djindexes.com/mdsidx/downloads/xlspages/ubsci/DJUBS_full_hist.xls")

if(!file.exists("DJUBS_full_hist.xls"))
  stop(paste("No spreadsheet exists.  Download the spreadsheet to be processed from http://www.msci.com into ", filesroot, "/.incoming", sep=""))
  
x = read.xls("DJUBS_full_hist.xls", sheet="Total Return", pattern="Symbol")

# Get the descriptions to add as attributes
x.attr = read.xls("DJUBS_full_hist.xls", sheet="Total Return", pattern="Date", nrows=1, header=FALSE)

# Get rid of the last line, which contains the disclaimer
x=x[-dim(x)[1],]
# Remove blank columns between sections
x=x[,-which(apply(x,2,function(x)all(is.na(x))))]

# Get attributes and labels
categoryNames = x.attr[,!is.na(x.attr)]
symbolNames = paste(make.names(colnames(x[,])), ".IDX", sep="")
ISOdates = as.Date(x[,1], "%m/%d/%Y")

for(i in 2:length(symbolNames)) {
  # check to make sure directories exist for each symbol
  dir.create(paste(filesroot, symbolNames[i], sep="/"), showWarnings = FALSE, 
  recursive = FALSE, mode = "0777")
}

# Parse the columns into individual price objects
for( i in 2:dim(x)[2]){
  x.xts = as.xts(as.numeric(x[,i]), order.by=ISOdates)
  R.xts = Return.calculate(x.xts)
  x.xts = cbind(x.xts, R.xts)
  colnames(x.xts)=c("Close", "Returns")
  xtsAttributes(x.xts) <- list(Description = paste(categoryNames[,i], "Index"))
#   assign(symbolNames[i], x.xts)
  save(x.xts, file=paste(filesroot, symbolNames[i], paste(symbolNames[i], ".rda", sep=""), sep="/"))
  # Describe the metadata for each index
  instrument(symbolNames[i], currency="USD", multiplier=1, tick_size=.01, start_date=head(index(x.xts),1), description=paste(categoryNames[,i], "Index"), data="CR", source="DJUBS", assign_i=TRUE)
}

# Now, whenever you log in you need to register the instruments.  This
# might be a line you put into .Rprofile so that it happens automatically:
# require(quantmod) # this requires a development build after revision 560 or so.
setSymbolLookup.FI(base_dir=filesroot, split_method='common')

# Now you should be able to:
getSymbols("DJUBSTR.IDX")
# chartSeries(Cl(DJUBSTR.IDX), theme="white")
charts.PerformanceSummary(DJUBSTR.IDX["2010::","Returns"], ylog=TRUE, wealth.index=TRUE, main = "DJUBS Total Returns Index Returns")
tail(DJUBSTR.IDX)