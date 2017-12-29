# Mikhail Andreev (c) 2017
# Base Quote Adapter: flat file

library(quantmod)

QUOTE.HIGH = 1
QUOTE.OPEN = 2
QUOTE.LOW = 3
QUOTE.CLOSE = 4

TICKERS.FILE = 'data/quotes/tickers.csv'
QUOTES.FOLDER = 'data/quotes'

#t <- getSymbols("GAZP.ME",src='csv',dir = QUOTES.FOLDER,return.class = "xts", auto.assign = F)

QuoteAdapterBase <- R6Class("QuoteAdapterBase",
                            
                            private = list(
                              assets = NULL,
                              quotes = list(),
                              
                              loadQuote = function(ticker) {
                                
                                print(paste("Loading", ticker))
                                getSymbols(ticker,src='csv',dir = QUOTES.FOLDER,return.class = "xts", auto.assign = F)
                              }
                            ),
                            
                            public = list(
                              initialize = function() {
                                private$assets <- read.csv(file = TICKERS.FILE, header = F, stringsAsFactors = F)
                                for(asset in private$assets) { 
                                  private$quotes[[asset]] <- private$loadQuote(asset)
                                }
                              },
                              
                              getQuote = function(asset.id, date, price.type = QUOTE.CLOSE) {
                                return(Cl(private$quotes[[asset.id]][paste0(format(date, DATE.PATTERN))]))
                              },
                              
                              getTimeSeries = function(asset.id, to) {
                                return(private$quotes[[asset.id]][paste0("::", format(to, DATE.PATTERN))])
                                #return(0)
                              }
                              
                              
                            )
                            
)
