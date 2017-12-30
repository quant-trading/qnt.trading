# Mikhail Andreev (c) 2017
# Exchange Simulation


source("exchange/TradingCalendar.R")
source("exchange/TransactionCostModel.R")
source("exchange/Trade.R")

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

                      trades <- list()
                      k = 1
                      
                      for(order in orders) {
                        if(order$status == ORDER.STATUS.PENDING) {
                        
                          # execute orders
                          order$status <- ORDER.STATUS.EXECUTED
                          
                          # create new trade
                          trade = Trade$new(order$assetID, order$qty, order$direction)
                          
                          # calculate execution price
                          quote <- Global.Quote.Adapter$getQuote(asset.id = order$assetID, date = Current.Date)
                          spread <- private$transactionCostModel$getMarketSpread(assetID = order$assetID, date = Current.Date)
                          mkt.impact <- private$transactionCostModel$getMarketImpact(order)
                          
                          if(order$direction == DIRECTION.BUY) {
                            price <- quote * (1 + spread + mkt.impact)
                            print(paste("BUY",order$qty,order$assetID,price))
                          } else {
                            price <- quote * (1 - spread - mkt.impact)
                            print(paste("SELL",order$qty,order$assetID,price))
                          }
                          
                          trade$ex.price = price
                          
                          # update settlement mode    
                          trade$settlementMode <- Global.Dictionary.Adapter$getSettlementMode(order$assetID)
                          
                          # calculate commission
                          trade$commission <- private$transactionCostModel$getExchangeCommission(order)
                          
                          trades[[k]] <- trade
                          
                          k = k + 1
                        }
                      }
                      
                      return(trades)
                    }
                  )
)