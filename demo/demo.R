require(FinancialInstrument)

# currencies need to be defined first

currency("USD")
currency("GBP")
currency("EUR")
currency("YEN")

# now exchange rates
exchange_rate("USDGBP","USD","GBP")
exchange_rate("USDEUR","USD","EUR")
exchange_rate("EURGBP","EUR","GBP")
exchange_rate("EURYEN","EUR","YEN")
exchange_rate("USDYEN","USD","YEN")

# now a stock and an option contract on it
stock("IBM","USD",1)
option(".IBM","USD",100,underlying_id="IBM")
#@TODO:Jeff to pull put and call option series for 2009

# bond & bond future

# non-US

# add a spread
