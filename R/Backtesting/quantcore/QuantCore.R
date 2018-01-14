# Mikhail Andreev (c) 2017
# Quant Core

source("quantcore/strategy/StrategyFactory.R")
source("exchange/TradingOrder.R")

QuantCore <- R6Class("QuantCore",
                  
                  private = list(
                    strategy = NULL,
                    
                    calculateTradingQuantity = function(assetID) {
                      return(DEFAULT.TRADING.QUANTITY)
                    },
                    
                    createTradingOrder = function(signal) {
                      TradingOrder$new(asset_id = signal$getAssetID(), 
                                       qty = private$calculateTradingQuantity( signal$getAssetID() ), 
                                       direction = signal$getDirection())
                    }
                  ),
                  
                  public = list(
                    initialize = function(strategy.id) {
                     strategyFactory = StrategyFactory$new()
                     private$strategy = strategyFactory$createStrategy(strategy.id)
                    },
                    
                    getTradingOrders = function(date, adapter) {
                      signals <- private$strategy$getTradingSignals(date, adapter)
                      
                      orders <- list()
                      k = 1
                      for(signal in signals) {
                        orders[[k]] <- private$createTradingOrder(signal)
                        k = k + 1
                      }
                      return(orders)
                    }
                  )
)