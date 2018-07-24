source("core/Holding.R")


Portfolio <- R6Class("Portfolio",
                     
                     public = list(
                       
                       
                       
                       initialize = function(p_id = "Default Portfolio") {
                         private$p_id <- p_id 
                       },
                       
                       add_holding = function(holding) {
                         n <- length(private$p_holdings)
                         private$p_holdings[[n+1]] <- holding
                       },
                       
                       remove_holding = function(holding) {
                         
                       },
                       
                       update = function() {lapply(private$p_holdings, function(x) {x$update()})}
                     ),
                     
                     private = list(
                       p_id = NULL,
                       
                       p_holdings = list()
                     ),
                     
                     active = list(
                       market_value = function() {
                         mv <- 0
                         for(h in private$p_holdings) mv <- mv + h$market_value
                         mv
                       },
                       
                       market_value_btc = function() {
                         self$market_value / DATA.ADAPTER.YAHOO$get_current_price(USD.BTC)
                       },
                       
                       holdings = function() private$p_holdings,
                       id = function() private$p_id
                     )
                     
)