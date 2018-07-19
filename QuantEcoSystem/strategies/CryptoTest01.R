source("core/StrategyBase.R")

Strategy <- R6Class("Strategy", 
                    
                    inherit = StrategyBase,
                    
                    public = list(
                      initialize = function(profile) {
                        super$initialize(profile$strategy_id,
                                         profile$strategy_type,
                                         profile$last_update,
                                         profile$portfolio_id
                                         )
                      }
                    ),
                    
                    private = list()
                    
                    
)