# Mikhail Andreev 2017
# Portfolio Class

Portfolio <- R6Class("Portfolio",
                     
                     public = list(
                       
                       positions = list(),
                       
                       initialize = function(cash) {
                         self$positions[[DEFAULT.CURRENCY]] <- cash
                       },
                       
                       get_value = function() {
                         v <- 0
                         for(id in names(self$positions)) {
                           if(id == DEFAULT.CURRENCY) {
                             v <- v + self$positions[[DEFAULT.CURRENCY]]
                           } else {
                             v <- v + abs(self$positions[[id]]) * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date)
                           }
                         }
                         as.numeric(v)
                       },
                       
                       get_cash = function() {
                         as.numeric(self$positions[[DEFAULT.CURRENCY]])
                       },
                       
                       get_longs = function() {
                         v <- 0.0
                         for(id in names(self$positions)) {
                           if(id != DEFAULT.CURRENCY && self$positions[[id]] > 0) {
                             v <- v + self$positions[[id]] * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date)
                           }
                         }
                         as.numeric(v)
                       },
                       
                       
                       get_shorts = function() {
                         v <- as.numeric(0)
                         for(id in names(self$positions)) {
                           if(id != DEFAULT.CURRENCY && self$positions[[id]] < 0) {
                             v <- v + self$positions[[id]] * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date)
                           }
                         }
                         as.numeric(v)
                       }
                         
                       
                     )
)