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
                          
                          getTradingSignals = function(date) {
                            
                            tickers <- self$localUniverse$getLocalUniverse(date)
                            
                            tradingSignals <- list()
                            
                            for(ticker in tickers) {
                              
                              # price indicator
                              quotes <- na.omit(Cl(Global.Quote.Adapter$getTimeSeries(ticker, to = date)))
                              indicator <- RSI(quotes)[format(date, DATE.PATTERN)]
                              
                              # volume indicator
                              volumes <- na.omit(Vo(Global.Quote.Adapter$getTimeSeries(ticker, to = date)))
                              avg.vol <- SMA(volumes, n = 3)[format(date, DATE.PATTERN)]
                              vol <- volumes[format(date, DATE.PATTERN)]
                              #print(paste(vol, avg.vol))
                              vol.indicator <- ifelse(vol < avg.vol, 1, 0)
                              
                              # signal generation
                              signal = 0
                              
                              if(length(indicator) > 0) {
                              if(indicator < 40 && vol.indicator) {
                                signal = DIRECTION.BUY
                                #print(paste("BUY", ticker))
                                } else if(indicator > 60 && vol.indicator) {
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
                            }
                            
                            return(tradingSignals)
                          }
                        )
)