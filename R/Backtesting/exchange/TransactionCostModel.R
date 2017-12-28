# Mikhail Andreev (c) 2017
# Transaction Cost Model

TransactionCostModel <- R6Class("TransactionCostModel",
                                
                                public = list(
                                  initialize = function() {
                                    
                                  },
                                  
                                  getMarketSpread = function(assetID, date) {
                                    return(0.002)
                                  },
                                  
                                  
                                  getExchangeCommission = function(order) {
                                    return(0)
                                  }
                                )
                                
                          )