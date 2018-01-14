# Mikhail Andreev (c) 2017
# Account Limits

AccountLimit <- R6Class("AccountLimit",
                        
                        private = list(
                          m_portfolio = numeric(0),
                          m_cash = numeric(0),
                          m_UDS = numeric(0),
                          m_init_margin = numeric(0),
                          m_adj_margin = numeric(0),
                          m_min_margin = numeric(0),
                          m_available = numeric(0),
                          m_shorts = numeric(0),
                          m_longs = numeric(0),
                          m_cash_in = numeric(0),
                          m_requirement = numeric(0),
                          m_block = numeric(0),
                          m_value = numeric(0),
                          m_short_margin = numeric(0),
                          m_long_margin = numeric(0)
                        ),
                        
                        public = list(
                          initialize = function() {},
                          
                          calculate = function(portfolio) {
                            
                            # cash
                            private$m_cash <- portfolio$get_cash()
                            
                            # calculate value
                            private$m_value <- portfolio$get_value()
                            
                            # long / short
                            private$m_longs <- as.numeric(portfolio$get_longs())
                            private$m_shorts <- as.numeric(portfolio$get_shorts())
                            
                          },
                          
                          get_value = function() {
                            private$m_value
                          },
                          
                          get_cash = function() {
                            private$m_cash
                          },
                          
                          get_long_mv = function() {
                            private$m_longs
                          },
                          
                          get_short_mv = function() {
                            private$m_shorts
                          }
                        )
)