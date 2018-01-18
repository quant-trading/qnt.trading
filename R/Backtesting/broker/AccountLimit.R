# Mikhail Andreev (c) 2017
# Account Limits

AccountLimit <- R6Class("AccountLimit",
                        
                        private = list(
                          portfolio_t2 = NULL,
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
                          m_long_margin = numeric(0),
                          m_leverage = numeric(0)
                        ),
                        
                        public = list(
                          initialize = function() {},
                          
                          calculate = function(portfolio) {
                            
                            private$portfolio_t2 <- portfolio
                            
                            # cash
                            private$m_cash <- portfolio$get_cash()
                            
                            # calculate value
                            private$m_value <- portfolio$get_value()
                            
                            # long / short
                            private$m_longs <- as.numeric(portfolio$get_longs())
                            private$m_shorts <- as.numeric(portfolio$get_shorts())
                            
                            # portfolio
                            private$m_portfolio <- as.numeric(self$calc_portfolio(portfolio))
                            
                            # margin
                            private$m_min_margin <- self$calc_min_margin(portfolio)
                            private$m_init_margin <- self$calc_init_margin(portfolio)
                            private$m_adj_margin <- private$m_init_margin  # we do not model intraday orders
                            
                            private$m_available <- private$m_portfolio - private$m_adj_margin
                            private$m_requirement <- min(0, private$m_portfolio - private$m_min_margin)
                            
                            # UDS
                            print(private$m_init_margin)
                            print(private$m_min_margin)
                            if(abs(private$m_init_margin - private$m_min_margin) < 0.01) {
                                private$m_UDS <- 100
                            } else {
                                private$m_UDS <- (private$m_portfolio - private$m_min_margin) / (private$m_init_margin - private$m_min_margin)
                            }
                            
                            # leverage
                            private$m_short_margin <- abs(private$m_shorts)
                            private$m_long_margin <- abs(private$m_shorts) + max(0, private$m_shorts - private$m_cash) + private$m_shorts
                            private$m_leverage = (private$m_short_margin + private$m_long_margin + private$m_value) / private$m_value
                          },
                          
                          calc_min_margin = function(portfolio) {
                            v <- 0.0
                            for(id in names(portfolio$positions)) {
                              if(id != DEFAULT.CURRENCY) {
                                
                                if(portfolio$positions[[id]] >= 0) {
                                  d <- Global.Dictionary.Adapter$get_d_min_margin_long(id)
                                } else {
                                  d <- Global.Dictionary.Adapter$get_d_min_margin_short(id)
                                }
                                
                                v <- v + abs(portfolio$positions[[id]]) * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date) * d  * Global.Dictionary.Adapter$is_marginal(id)
                              }
                            }
                            as.numeric(v * Global.Dictionary.Adapter$is_marginal(id))
                          },
                          
                          calc_init_margin = function(portfolio) {
                            v <- 0.0
                            for(id in names(portfolio$positions)) {
                              if(id != DEFAULT.CURRENCY) {
                                
                                if(portfolio$positions[[id]] >= 0) {
                                  d <- Global.Dictionary.Adapter$get_d_init_margin_long(id)
                                } else {
                                  d <- Global.Dictionary.Adapter$get_d_init_margin_short(id)
                                }
                                
                                v <- v + abs(portfolio$positions[[id]]) * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date) * d * Global.Dictionary.Adapter$is_marginal(id)
                              }
                            }
                            as.numeric(v)
                          },
                          
                          calc_portfolio = function(portfolio) {
                            v <- 0.0
                            for(id in names(portfolio$positions)) {
                              if(id != DEFAULT.CURRENCY) {
                                
                                v <- v + abs(portfolio$positions[[id]]) * Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date) * Global.Dictionary.Adapter$is_marginal(id)
                              }
                            }
                            as.numeric(v + portfolio$positions[[DEFAULT.CURRENCY]])
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
                          },
                          
                          get_UDS = function() {
                            private$m_UDS
                          },
                          
                          get_leverage = function() {
                            private$m_leverage
                          },
                          
                          get_long_margin = function() {
                            abs(private$m_long_margin)
                          },
                          
                          get_short_margin = function() {
                            abs(private$m_short_margin)
                          },
                          
                          get_long_position_limit = function(id) {
                            floor(private$m_available / Global.Dictionary.Adapter$get_d_init_margin_long(id) / (Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date)))
                          },
                          
                          get_short_position_limit = function(id) {
                            
                            if(Global.Dictionary.Adapter$is_shortable(id)) {
                              l <- floor(private$m_available / Global.Dictionary.Adapter$get_d_init_margin_short(id) / (Global.Dictionary.Adapter$getLotSize(id) * Global.Quote.Adapter$getQuote(id, Current.Date)))
                            } else {
                              q <- private$portfolio_t2$positions[[id]]
                              if(q > 0) {
                                l <- abs(q)
                              } else {
                                l <- 0
                              }
                            }
                            l
                          }
                        )
)