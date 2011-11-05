# Script for managing the download of data from
# http://pitrading.com/free_market_data.htm
# including some long series of continuous futures contracts 

# This script requires the following directory structure:
# filesroot [directory set in the script below]
#   Each symbol's processed csv files are stored in sub-directories
#   named for each symbol, e.g., ~/Data/EOD\ Global\ Indexes/XMI.IDX.
#   These directories and files will be created and updated by this script.
# filesroot/.incoming
#   New or updated zip files should be placed here for processing.
#   This is also the working directory for the processing done in 
#   this script. Unzipped csv files are redirected here for processing.
#   Temporary files are stored here before being appended to the symbol
#   file csv in the appropriate directory

filesroot = "/home/peter/Data/pitrading"

start_t<-Sys.time()

# Create and set the working directory if it doesn't exist
if (!file.exists(paste(filesroot, "/.incoming", sep="")))
   dir.create(paste(filesroot, "/.incoming", sep=""), mode="0777")
setwd(paste(filesroot, "/.incoming", sep=""))

# Does the archive directory structure exist?
if (!file.exists("../.archive")){  
  dir.create("../.archive", mode="0777")
  dir.create("../.archive/zip_files", mode="0777")
}
if (!file.exists("../.archive/zip_files"))
  dir.create("../.archive/zip_files", mode="0777")
  
# Use wget so that we don't need a list of files to work from
system("wget -r -l1 -H -t1 -nd -N -np -A.zip http://pitrading.com/free_market_data.htm")

# -r -H -l1 -np These options tell wget to download recursively. 
# That  means it goes to a URL, downloads the page there, then follows 
# every  link it finds. The -H tells the app to span domains, meaning 
# it should  follow links that point away from the page. And the -l1 
# (a lowercase L  with a numeral one) means to only go one level deep; 
# that is, don't  follow links on the linked site. It  will take each 
# link from the list of pages, and download it. The -np  switch stands 
# for "no parent", which instructs wget to never follow a  link up to a 
# parent directory. 
#  
# We don't, however, want all the links -- just those that point to 
# zip files we haven't yet seen. Including -A.zip tells wget to only 
# download files that end with the .zip extension. And -N turns on 
# timestamping, which means wget won't download something with the same 
# name unless it's newer. 
#  
# To keep things clean, we'll add -nd, which makes the app save every 
# thing it finds in one directory, rather than mirroring the directory 
# structure of linked sites. 

# Unzip the files to text files
system("unzip \\*.zip")
system("mv *.zip ../.archive/zip_files/")

# What files did we download?
files = list.files()

# Each file contains the full history for the symbol, so we just need to 
# move the file into the correct base directory.  We don't need to do any
# data parsing.
pisymbols = vector()
for(i in 1:length(files)) {
  # generate a list of symbols from the files we downloaded
  filename.txt <- files[i]
  pisymbols[i] <- substr(filename.txt, 1, nchar(filename.txt) - 4)
}

# The extra ".CC" appended to each symbol to indicate that the data is for a 
# "continuous contract" rather than a futures contract to be used as a root.
# We're modifying the symbols used so that they don't conflict with actual
# futures contracts.
for(pisymbol in pisymbols) {
  # check to make sure directories exist for each
  dir.create(paste("../", pisymbol, ".CC", sep=""), showWarnings = FALSE, 
  recursive = FALSE, mode = "0777")
  # move files into appropriate directory
  system(paste("mv ", pisymbol, ".txt", " ../", pisymbol, ".CC/", pisymbol, ".CC.csv", sep=""))
}

end_t<-Sys.time()
print(c("Elapsed time: ",end_t-start_t))
print(paste("Processed ", length(pisymbols), " symbols.", sep=""))

# The following currencies need to be created first:
# require(FinancialInstrument)
# currency("USD")
# currency("JPY")
# currency("AUD")
# currency("CHF")
# currency("GBP")
# currency("MXN")
# currency("EUR")
# currency("CAD")

# We need to have a csv file that describes the metadata
# for all the symbols.  Remember to change the symbol to append ".CC" to each.
# load.instruments(paste(filesroot, "/.scripts/instr.pitrading.csv", sep=""))

# Dates in files are formatted as "%m/%d/%Y".
# Now, whenever you log in you need to register the instruments.  This
# might be a line you put into .Rprofile so that it happens automatically:
# require(quantmod) # this requires a development build after revision 560 or so.
# setSymbolLookup.FI(base_dir=filesroot, split_method='common', storage_method='csv', src='csv', extension='csv', format='%m/%d/%Y')

# Now you should be able to:
# > getSymbols("INX.CC")
# [1] "INX.CC"
# > chart_Series(INX.CC)
# > head(INX.CC)

