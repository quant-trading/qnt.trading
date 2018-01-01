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
                                      return(2)
                                    }
                                  },
                                  
                                  
                                  getMarketImpact = function(order) {
                                    return(0)
                                  }
                                )
                                
                          )