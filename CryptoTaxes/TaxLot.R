TaxLot <- R6Class("TaxLot", 
                  public = list(
                    initialize = function(asset, qty, open.date, cost.basis, fees = 0) {
                     private$qty = qty
                     private$open.date = open.date
                     private$cost.basis = cost.basis
                     private$open.price = cost.basis / qty
                     private$asset = asset
                     private$tax.factor = ifelse(Sys.Date() - private$open.date > 365, 0.15, 0.27)
                    },
                    
                    get_open_price = function() {
                      private$open.price
                    },
                    
                    get_open_date = function() {private$open.date},
                    
                    get_qty = function(){private$qty},
                    
                    get_asset = function() {private$asset},
                    
                    reduce_qty = function(q) {private$qty <- private$qty - q},
                    
                    get_info = function(price) {
                      data.frame(
                        asset = private$asset,
                        qty = private$qty,
                        tax.factor = private$tax.factor,
                        cost.basis = private$cost.basis,
                        current.mv = private$qty * price,
                        holding.period = self$get_long_short(),
                        holding.days = Sys.Date() - private$open.date,
                        pnl = (price - private$open.price) / private$open.price,
                        gnl = (price - private$open.price) * private$qty
                      )
                    }, 
                    
                    get_long_short = function() {ifelse(Sys.Date() - private$open.date > 365, "long", "short")}
                  ),
                  
                  private = list(
                    asset = NULL,
                    open.date = NULL,
                    open.price = NULL,
                    qty = NULL,
                    cost.basis = NULL,
                    tax.factor = NULL
                    
                  )
)