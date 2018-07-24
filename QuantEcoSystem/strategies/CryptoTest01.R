source("core/StrategyBase.R")
source("crypto/CrytpoTest01.SignalsGenerator.R")

MAX.LIMIT.PER.ASSET = 0.20


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
                        
                      },
                      
                      get_trading_orders = function(portfolio, signals) {
                        open_orders <- private$get_open_orders(portfolio, signals)
                        close_orders<- private$get_close_orders(portfolio)
                        c(open_orders, close_orders)
                      }
                    ),
                    
                    private = list(
                      get_close_orders = function(portfolio) {
                        if(length(portfolio$holdings)>0) {
                          
                          orders <- list()
                          
                          for(i in seq(1, length(portfolio$holdings))) {
                            if(portfolio$holdings[[i]]$a_id != DEFAULT.CURRENCY) {
                              order <- TradingOrder$new(id = portfolio$holdings[[i]]$a_id,
                                                        q = portfolio$holdings[[i]]$q,
                                                        type = ORDER.CLOSE.SELL)
                              portfolio$holdings[[i]]$mark_to_sell()
                              orders[[length(orders) + 1]] <- order
                            }
                          }
                          return(orders)
                        } else {
                          return(list())
                        }
                      },
                      
                      get_open_orders  = function(portfolio, signals) {
                        orders <- list()
                        
                        if(length(signals) == 0) {return(list())}

                        lot <- portfolio$market_value_btc * MAX.LIMIT.PER.ASSET
                        
                        for(i in seq(1, length(signals))) {
                          order <- TradingOrder$new(id = signals[[i]]$asset,
                                                    q = lot / signals[[i]]$current_price,
                                                    type = ifelse(signals[[i]]$dir == SIGNAL.BUY, ORDER.OPEN.BUY, ORDER.OPEN.SELL),
                                                    curr = "BTC")

                          orders[[length(orders) + 1]] <- order                          
                        }
                        orders
                      } 
                    )
)