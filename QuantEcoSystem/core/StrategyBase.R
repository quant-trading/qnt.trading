StrategyBase <- R6Class("StrategyBase",
                        
                        public = list(
                          initialize = function(id, type, last_dt, port_id) {
                            private$strategy_id <- id
                            private$strategy_type <- type
                            private$last_update <- last_dt
                            private$portfolio_id <- port_id
                          }
                        ),
                        
                        private = list(
                          strategy_id = NULL,
                          strategy_type = NULL,
                          last_update = NULL,
                          portfolio_id = NULL
                        ),
                        
                        active = list(
                          id = function() private$strategy_id,
                          port_id = function() private$portfolio_id,
                          last_dt = function() private$last_update
                        )
)