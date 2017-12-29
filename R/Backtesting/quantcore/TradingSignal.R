# Mikhail Andreev (c) 2017
# Trading Signal

TradingSignal <- R6Class("TradingSignal",
                         
                         private = list(
                           assetID = NULL,
                           direction = NULL,
                           date = NULL
                         ),
                         
                         public = list(
                           initialize = function(assetID, date, direction) {
                             private$assetID = assetID
                             private$date = date
                             private$direction = direction
                           },
                           
                           getAssetID = function() { private$assetID },
                           getDirection = function() { private$direction }
                         )
  
)