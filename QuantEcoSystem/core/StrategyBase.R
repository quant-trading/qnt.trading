StrategyBase <- R6Class("StrategyBase",
                        
                        public = list(
                          signal_generator = NULL,
                          
                          initialize = function(id, type, last_dt, port_id) {
                            private$strategy_id <- id
                            private$strategy_type <- type
                            private$last_update <- last_dt
                            private$portfolio_id <- port_id

                          },
                          
                          get_trading_signals = function() {
                            print(self$signal_generator)
                            self$signal_generator$generate_trading_signals()
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