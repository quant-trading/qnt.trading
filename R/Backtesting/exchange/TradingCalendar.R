# Mikhail Andreev (c) 2017
# Trading Calendar

TradingCalendar <- R6Class("TradingCalendar",
                           
                           private = list(
                             tradingDates = NULL
                           ),
                           
                           public = list(
                             initialize = function() {
                               
                             },
                             
                             getNextTradingDate = function(dt) {
                               return(0)
                             }
                           )
)