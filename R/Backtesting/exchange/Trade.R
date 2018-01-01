# Mikhail Andreev (c) 2017
# Trade

Trade <- R6Class("Trade",
                 public = list(
                   
                   assetID = NULL,
                   qty = NULL,
                   direction = NULL,
                   ex.price = 0,
                   date = NULL,

                   commission = 0,
                   settlementMode = NULL,
                   initial.margin = 0,
                   
                   initialize = function(asset_id, qty, direction) {
                     self$assetID = asset_id
                     self$qty = qty
                     self$direction = direction
                     self$date = Current.Date
                   },
                   
                   
                   getCommission = function() {self$commission},
                   
                   
                   getSignedQuantity = function() {return(self$qty * self$direction)},
                   
                   
                   getExecutedAmount = function() {
                     as.numeric(-self$getSignedQuantity() * Global.Dictionary.Adapter$getLotSize( self$assetID ) * self$ex.price)
                   }
                 )                 
)
