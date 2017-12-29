# Mikhail Andreev (c) 2017
# Base Strategy

source("quantcore/TradingSignal.R")
source("quantcore/LocalUniverse.R")

StrategyBase <- R6Class("StrategyBase",
                        
                        public = list(
                          
                          localUniverse = NULL,
                          
                          initialize = function() {
                            self$localUniverse <- LocalUniverse$new()
                          },
                          
                          getTradingSignals = function() {
                            return(list())
                          }
                        )
)