# Script for downloading and parsing a monthly total return series from
# http://www.mscibarra.com/
#

# Peter Carl

# Currently, this script assumes that a single index is in the spreadsheet.
# For cases where there are multiple indexes, this script will need to be
# extended.

# Load needed packages:
require(zoo)
require(gdata)

# Set the working directory, where there's a .incoming folder that contains
# the downloaded spreadsheet.
filesroot = "~/Data/MSCI"

# Create and set the working directory if it doesn't exist
if (!file.exists(filesroot))
  dir.create(filesroot, mode="0777")

# Create and set the .incoming and symbol directories if they don't exist
if (!file.exists(paste(filesroot, "/.incoming", sep="")))
  dir.create(paste(filesroot, "/.incoming", sep=""), mode="0777")
setwd(paste(filesroot, "/.incoming", sep=""))

if (!file.exists(paste(filesroot, "/MSCI.World.IDX", sep="")))
  dir.create(paste(filesroot, "/MSCI.World.IDX", sep=""), mode="0777")

if(!file.exists("historyIndex.xls"))
  stop(paste("No spreadsheet exists.  Download the spreadsheet to be processed from http://www.msci.com into ", filesroot, "/.incoming", sep=""))

# Read the first sheet in the xls workbook directly from the working directory:
x = read.xls("historyIndex.xls", pattern="Date")
x = x[-((dim(x)[1]-15):dim(x)[1]),] # trim off last 16 lines of disclaimer
x.dates = paste(substring(x[,1],1,6),substring(x[,1],7,10)) # unmangle the dates
x.dates = as.Date(x.dates, format="%b %d %Y")
x.prices = as.numeric((sub(",","", x[,2], fixed=TRUE))) # get rid of commas
x.xts = xts(x.prices, order.by=x.dates)
x.returns = Return.calculate(x.xts)
x.xts = cbind(x.xts, x.returns)
colnames(x.xts) = c("Close","Returns")

# Save it into an rda file on the filesystem
save(x.xts, file=paste(filesroot,"MSCI.World.IDX/MSCI.World.IDX.rda", sep="/"))

# Create currencies first:
require(FinancialInstrument)
currency("USD")

# Describe the metadata for the index
instrument("MSCI.World.IDX", currency="USD", multiplier=1, tick_size=.01, start_date="1969-12-31", description="MSCI World Index Standard (Large+Mid Cap) USD", data="CR", source="msci.com", assign_i=TRUE)

# Now, whenever you log in you need to register the instruments.  This
# might be a line you put into .Rprofile so that it happens automatically:
# require(quantmod) # this requires a development build after revision 560 or so.
setSymbolLookup.FI(base_dir=filesroot, split_method='common')

# Now you should be able to:
getSymbols("MSCI.World.IDX")
chartSeries(Cl(MSCI.World.IDX), theme="white")
head(MSCI.World.IDX)