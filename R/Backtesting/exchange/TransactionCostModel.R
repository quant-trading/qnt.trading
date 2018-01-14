# Mikhail Andreev (c) 2017
# Transaction Cost Model

TransactionCostModel <- R6Class("TransactionCostModel",
                                
                                public = list(
                                  initialize = function() {
                                    
                                  },
                                  
                                  getMarketSpread = function(assetID, date = NULL) {
                                    return(DEFAULT.MARKET.SPREAD)
                                  },
                                  
                                  
                                  getExchangeCommission = function(order) {
                                    if(!EXCHANGE.COMMISSION) {
                                      return(0)
                                    } else {
                                      return(EXCHANGE.COMMISSION.RATE * abs(order$qty) * Global.Dictionary.Adapter$getLotSize(order$assetID) * Global.Quote.Adapter$getQuote(order$assetID, Current.Date))
                                    }
                                  },
                                  
                                  
                                  getMarketImpact = function(order) {
                                    return(0)
                                  }
                                )
                                
                          )