# Mikhail Andreev (c) 2017
# Exchange Simulation


source("exchange/TradingCalendar.R")
source("exchange/TransactionCostModel.R")

Exchange <- R6Class("Exchange",
                  
                  private = list(
                    transactionCostModel = NULL,
                    tradingCalendar = NULL
                  ),
                  public = list(
                    
                    initialize = function() {
                      tradingCalendar = TradingCalendar$new()
                      transactionCostModel = TransactionCostModel$new()
                    },
                    
                    getNextTradingDate = function(dt) {
                      private$tradingCalendar$getNextTradingDate(dt)
                    },
                    
                    executeTradingOrders = function(orders) {
                      print("TODO: Orders have been executed")
                      return(NULL)
                    }
                  )
)