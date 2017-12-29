# Mikhail Andreev (c) 2017
# Exchange Simulation


source("exchange/TradingCalendar.R")
source("exchange/TransactionCostModel.R")
source("holdings/TaxLot.R")

Exchange <- R6Class("Exchange",
                  
                  private = list(
                    transactionCostModel = NULL,
                    tradingCalendar = NULL
                  ),
                  public = list(
                    
                    initialize = function() {
                      private$tradingCalendar = TradingCalendar$new()
                      private$transactionCostModel = TransactionCostModel$new()
                    },
                    
                    getNextTradingDate = function(dt) {
                      private$tradingCalendar$getNextTradingDate(dt)
                    },
                    
                    executeTradingOrders = function(orders) {

                      for(order in orders) {
                        if(order$status == ORDER.STATUS.PENDING) {
                          
                          # execute orders
                          quote <- Global.Adapter$getQuote(asset.id = order$assetID, date = Current.Date)
                          spread <- private$transactionCostModel$getMarketSpread(assetID = order$assetID, date = Current.Date)
                          mkt.impact <- private$transactionCostModel$getMarketImpact(order)
                          
                          taxLot <- TaxLot$new(order$assetID)
                          taxLot$qty <- order$qty
                          taxLot$price <- quote * (1 - spread - mkt.impact)
                          taxLot$openDate <- Current.Date
                          
                          order$executedLots[[1]] <- taxLot
                          
                          order$commission <- private$transactionCostModel$getExchangeCommission(order)
                          order$status <- ORDER.STATUS.EXECUTED
                        }
                      }
                      
                      return(orders)
                    }
                  )
)