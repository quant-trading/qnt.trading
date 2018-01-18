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
                              
                              loadQuote_finam = function(ticker) {
                                print(paste("Loading", ticker))
                                data <- read.csv(file = paste0(QUOTES.FOLDER, "/", ticker, ".csv"), header = T, stringsAsFactors = F, sep = ";", strip.white = T)
                                data <- data[,c(-2)]
                            
                                names(data) <- c("Date", "Open", "High", "Low", "Close", "Volume")
                                data$Date <- as.Date(as.character(data$Date), format = "%Y%m%d")
                                #print(data)
                                
                                xts(data[, -1], order.by=data$Date)
                              },
                              
                              
                              
                              loadQuote = function(ticker) {
                                print(paste("Loading", ticker))
                                getSymbols(ticker,src='csv',dir = QUOTES.FOLDER,return.class = "xts", auto.assign = F)
                              }
                            ),
                            
                            public = list(
                              initialize = function() {
                                private$assets <- read.csv(file = TICKERS.FILE, header = F, stringsAsFactors = F)
                                for(asset in private$assets) { 
                                  private$quotes[[asset]] <- private$loadQuote_finam(asset)
                                  #print(private$quotes)
                                }
                              },
                              
                              getQuote = function(asset.id, date, price.type = QUOTE.CLOSE) {
                                return(Cl(private$quotes[[asset.id]][paste0(format(date, DATE.PATTERN))]))
                              },
                              
                              getVolume = function(asset.id, date) {
                                return(Vo(private$quotes[[asset.id]][paste0(format(date, DATE.PATTERN))]))
                              },
                              
                              getTimeSeries = function(asset.id, to) {
                                return(private$quotes[[asset.id]][paste0("::", format(to, DATE.PATTERN))])
                                #return(0)
                              }
                              
                              
                            )
                            
)
