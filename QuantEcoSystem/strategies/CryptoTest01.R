source("core/StrategyBase.R")
source("crypto/CrytpoTest01.SignalsGenerator.R")

Strategy <- R6Class("Strategy", 
                    
                    inherit = StrategyBase,
                    
                    public = list(
                      initialize = function(profile) {
                        super$initialize(profile$strategy_id,
                                         profile$strategy_type,
                                         profile$last_update,
                                         profile$portfolio_id
                                         )
                        
                        self$signal_generator = CrytpoTest01.SignalsGenerator$new()
                        
                      }
                    ),
                    
                    private = list()
                    
                    
)