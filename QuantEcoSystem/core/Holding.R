Holding <- R6Class("Holding",
                   
                   public = list(
                     initialize = function(id, quantity, type) {
                       private$id <- id
                       private$qty <-as.numeric(quantity)
                       private$asset_type <- type
                       
                       private$adapter_ref <- switch (type,
                         TYPE.CRYPTO = DATA.ADAPTER.CRYPTO
                       )
                     },
                     
                     update = function() {
                       private$current_price <- private$adapter_ref$get_current_price(private$id)
                     }
                   ),
                   
                   private = list(
                     id = NULL,
                     qty = 0,
                     lot_size = 1,
                     asset_type = NULL,
                     current_price = 1,
                     adapter_ref = NULL,
                     sell_flag = NULL
                   ),
                   
                   active = list(
                     market_value = function() {
                       private$qty * private$current_price * private$lot_size
                     },
                     
                     mark_to_sell = function() private$sell_flag = 1,
                     
                     a_id = function() private$id,
                     q = function() private$qty
                   )
)