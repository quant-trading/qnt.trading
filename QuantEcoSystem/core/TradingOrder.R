ORDER.OPEN.BUY = 1
ORDER.OPEN.SELL = -2
ORDER.CLOSE.SELL = -1
ORDER.CLOSE.BUY = 2

TradingOrder <- R6Class("TradingOrder",
                        
                        public = list(
                          initialize = function(id, q, type, curr) {
                            private$t_asset_id = id
                            private$t_qty = q
                            private$t_type = type
                            private$t_curr = curr
                          }
                        ),
                        
                        active = list(
                          asset_id = function() {private$t_asset_id},
                          q = function() {private$t_qty},
                          type = function() {private$t_type},
                          currency = function() {private$t_curr}
                          
                        ),
                        
                        private = list(
                          t_asset_id = NULL,
                          t_qty = NULL,
                          t_type = NULL,
                          t_exch = NULL,
                          t_curr = NULL
                        )
)