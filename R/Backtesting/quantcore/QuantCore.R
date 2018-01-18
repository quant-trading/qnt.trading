# Mikhail Andreev (c) 2017
# Quant Core

source("quantcore/strategy/StrategyFactory.R")
source("exchange/TradingOrder.R")

QuantCore <- R6Class("QuantCore",
                  
                  private = list(
                    strategy = NULL,
                    limit_t2 = NULL,
                    
                    calculateTradingQuantity = function(signal) {
                      
                      pos.limit <- 0
                      
                      if(signal$getDirection() == DIRECTION.SELL) {
                        pos.limit <- private$limit_t2$get_short_position_limit(signal$getAssetID())
                        print(paste("Short Limit", pos.limit))
                      } else {
                        if(signal$getDirection() == DIRECTION.BUY) {
                          pos.limit <- private$limit_t2$get_long_position_limit(signal$getAssetID())
                          print(paste("Long Limit", pos.limit))
                        }
                      }
                      
                      
                      return(max(0, min(pos.limit, DEFAULT.TRADING.QUANTITY)))
                    },
                    
                    createTradingOrder = function(signal) {
                      TradingOrder$new(asset_id = signal$getAssetID(), 
                                       qty = private$calculateTradingQuantity( signal ), 
                                       direction = signal$getDirection())
                    }
                  ),
                  
                  public = list(
                    initialize = function(strategy.id) {
                     strategyFactory = StrategyFactory$new()
                     private$strategy = strategyFactory$createStrategy(strategy.id)
                    },
                    
                    getTradingOrders = function(date, limit) {
                      private$limit_t2 <- limit
                      signals <- private$strategy$getTradingSignals(date)
                      
                      orders <- list()
                      k = 1
                      for(signal in signals) {
                        order <- private$createTradingOrder(signal)
                        if(order$qty != 0) {
                        orders[[k]] <- order 
                        k = k + 1
                        }
                      }
                      return(orders)
                    }
                  )
)