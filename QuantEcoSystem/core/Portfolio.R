source("core/Holding.R")


Portfolio <- R6Class("Portfolio",
                     
                     public = list(
                       
                       
                       
                       initialize = function(p_id = "Default Portfolio") {
                         private$id <- p_id 
                       },
                       
                       add_holding = function(holding) {
                         n <- length(private$holdings)
                         private$holdings[[n+1]] <- holding
                       },
                       
                       remove_holding = function(holding) {
                         
                       },
                       
                       update = function() {lapply(private$holdings, function(x) {x$update()})}
                     ),
                     
                     private = list(
                       id = NULL,
                       
                       holdings = list()
                     ),
                     
                     active = list(
                       market_value = function() {
                         mv <- 0
                         for(h in private$holdings) mv <- mv + h$market_value
                         mv
                       }
                     )
                     
)