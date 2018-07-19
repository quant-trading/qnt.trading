StrategyBase <- R6Class("StrategyBase",
                        
                        public = list(
                          initialize = function(id) {
                            private$strategy_id <- id
                          }
                        ),
                        
                        private = list(
                          strategy_id = NULL
                        ),
                        
                        active = list(
                          id = function() private$strategy_id
                        )
)