source("core/StrategyBase.R")

Strategy <- R6Class("Strategy", 
                    
                    inherit = StrategyBase,
                    
                    public = list(
                      initialize = function(id) {
                        super$initialize(id)
                      }
                    ),
                    
                    private = list()
                    
                    
)