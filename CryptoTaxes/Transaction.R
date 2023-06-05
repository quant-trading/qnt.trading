Transaction <- R6Class("Transaction",
                       public = list(
                         initialize = function(asset, qty, open.price, open.date, close.price, close.date){
                           self$asset = asset
                           self$qty = qty
                           self$open.price = open.price
                           self$open.date = open.date
                           self$close.price = close.price
                           self$close.date = close.date                   
                         },
                         
                         is_potential_wash_sale = function(){
                           ifelse(self$open.price > self$close.price && Sys.Date() - self$close.date < 31, TRUE, FALSE)
                         },
                         
                         asset = NULL,
                         qty = NULL,
                         open.price = NULL,
                         open.date = NULL,
                         close.price = NULL,
                         close.date = NULL
                       ),
                       private = list()
                       )