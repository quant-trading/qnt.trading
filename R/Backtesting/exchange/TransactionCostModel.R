# Mikhail Andreev (c) 2017
# Transaction Cost Model

TransactionCostModel <- R6Class("TransactionCostModel",
                                
                                public = list(
                                  initialize = function() {
                                    
                                  },
                                  
                                  getMarketSpread = function(assetID, date = NULL) {
                                    return(0.002)
                                  },
                                  
                                  
                                  getExchangeCommission = function(order) {
                                    return(2)
                                  },
                                  
                                  
                                  getMarketImpact = function(order) {
                                    return(0)
                                  }
                                )
                                
                          )