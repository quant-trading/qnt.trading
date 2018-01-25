# Mikhail Andreev. 2017
# Trade Analyzer

TradeAnalyzer <- R6Class("TradeAnalyzer",
                         
                         public = list(
                           
                           trades = c(),
                           
                           initialize = function() {
                             
                           },
                           
                           add_trade = function(trade) {
                               self$trades <- c(self$trades, trade)  
                           },
                           
                           plot_distribution = function() {
                             hist(self$trades, breaks=12, col="red")
                           }
                         )
                         
                         )