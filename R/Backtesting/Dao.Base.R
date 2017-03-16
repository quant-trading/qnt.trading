DAO.Base <- R6Class("DAO.Base",
                    public = list(
                      initialize = function() {},
                      
                      loadQuotes = function(asset)
                      {
                        getSymbols(asset, auto.assign = F)
                      }
                    )
                    ) 