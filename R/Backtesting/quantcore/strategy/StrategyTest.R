# Mikhail Andreev (c) 2017
# Test Strategy: RSI Indicator

source("quantcore/strategy/StrategyBase.R")
source("quantcore/TradingSignal.R")

StrategyTest <- R6Class("StrategyTest",
                        inherit = StrategyBase,
                        
                        public = list(
                          initialize = function() {
                            self$localUniverse <- LocalUniverse$new()
                          },
                          
                          getTradingSignals = function(date, adapter) {
                            
                            tickers <- self$localUniverse$getLocalUniverse(date)
                            
                            tradingSignals <- list()
                            
                            for(ticker in tickers) {
                              
                              quotes <- na.omit(Cl(adapter$getTimeSeries(ticker, to = date)))
                              indicator <- RSI(quotes)[format(date, DATE.PATTERN)]
                              #print(indicator)
                              
                              signal = 0
                              
                              if(indicator < 30) {
                                signal = DIRECTION.BUY
                                #print(paste("BUY", ticker))
                                } else if(indicator > 70) {
                                  signal = DIRECTION.SELL
                                  #print(paste("SELL", ticker))
                                }
                              
                              #print(signal)
                              
                              if(signal != 0)
                              {
                                tradingSignals[[ticker]] <- TradingSignal$new(ticker, date, signal)
                              }
                              
                              #print(Cl(adapter$getTimeSeries(ticker, to = date)))
                            }
                            
                            return(tradingSignals)
                          }
                        )
)