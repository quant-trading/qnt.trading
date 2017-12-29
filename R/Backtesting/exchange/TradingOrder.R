# Mikhail Andreev (c) 2017
# Trading Order

ORDER.STATUS.PENDING  = 1
ORDER.STATUS.EXECUTED = 2
ORDER.STATUS.REJECTED = 3


TradingOrder <- R6Class("TradingOrder",
                        
                        public = list(
                          
                          assetID = NULL,
                          qty = NULL,
                          direction = NULL,
                          
                          status = NULL,
                          executedLots = list(),
                          commission = 0,
                          
                          
                          initialize = function(asset_id, qty, direction) {
                           self$assetID = asset_id
                           self$qty = qty
                           self$direction = direction
                           self$status = ORDER.STATUS.PENDING
                          },
                          
                          
                          getAverageExecutionPrice = function() {
                            return(0)
                          }
                        )
)