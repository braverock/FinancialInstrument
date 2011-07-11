require(FinancialInstrument)

# currencies need to be defined first

currency("USD")
currency("GBP")
currency("EUR")
currency("JPY")

# now exchange rates
exchange_rate("GBPUSD","USD","GBP")
exchange_rate("EURUSD","USD","EUR")
exchange_rate("EURGBP","GBP","EUR")
exchange_rate("EURJPY","JPY","EUR")
exchange_rate("USDJPY","JPY","USD")

# now some stocks 
stock("IBM","USD")
stock("SPY","USD")
stock("DIA","USD")

#Contract specs for options on IBM
option(".IBM","USD",multiplier=100,tick_size=.01, underlying_id="IBM")

#Define tradeable option_series instrument 
option_series(".IBM","20110716C175", expires="2011-07-16", callput='call',underlying_id='IBM')

#Or use yahoo to help define the specs and all near-term options
option_series.yahoo('SPY')
#option_series.yahoo("SPY",Exp=NULL) # would define all options on SPY

#load.instruments("./FinancialInstrument/data/currencies.csv")
#load.instruments("./FinancialInstrument/data/root_contracts.csv")
#load.instruments("./FinancialInstrument/data/future_series.csv")

# bond & bond future

# non-US
stock("BMW","EUR")
BMW <- getSymbols("BMW.DE",src='yahoo',auto.assign=FALSE)
EURUSD <- getSymbols("DEXUSEU",src='FRED',auto.assign=FALSE)

BMW.USD <- redenominate("BMW") #convert prices from EUR to USD 
# Define a synthetic instrument that is BMW denominated in USD instead of EUR.
synthetic("BMW.USD","USD",1,members=c("BMW","EURUSD"))

# Define a spread
getSymbols(c("SPY","DIA")) #download the data for both legs
SPYDIA.fSB <- fn_SpreadBuilder("SPY","DIA") #build a 2 leg spread with multiple columns
#or define the spread first
spread("SPYDIA", "USD", members=c("SPY","DIA"), memberratio=c(1,-1))
SPYDIA.bS <- buildSpread("SPYDIA", auto.assign=FALSE) #and build it (could be multiple-leg)

SPYDIA.rat <- buildRatio(c("SPY","DIA")) #calculate ratios of prices


##Look at what has been defined.
#bottom-up
buildHierarchy( c("IBM","SPY",".IBM"), c("currency", "multiplier"))
#top-down
it <- instrument.table()
head(it)

instrument.table( ,attrs.of='USD') #only show attributes that instrument "USD" also has

