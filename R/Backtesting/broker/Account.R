# Mikhail Andreev (c) 2017
# Trading Account


source("holdings/Holding.R")
source("results/AccountState.R")
source("broker/Portfolio.R")
source("broker/AccountLimit.R")

Account <- R6Class("Account",
                   
                   private = list(
                     accountID = NULL,
                     accountType = NULL,
                     holdings = list(),
                     
                     limit_T2 = NULL,
                     
                     projection.T1 = list(),
                     projection.T2 = list(),
                     
                     T1.k = 0,
                     T2.k = 0,
                     
                     currency = NULL,
                     cash = as.numeric(0),
                     
                     # Internal Calculations --------------------------------------------------------------------------
                     get_portfolio = function(mode) {
                       
                       # create T0 portfolio
                       portfolio <- Portfolio$new(private$cash)
                       
                       for(h in private$holdings) {
                         portfolio$positions[[h$getID()]] <- h$get_quantity()
                       }
                       
                       # adjust for T1
                       if(mode > SETTLEMENT.T0) {
                         private$apply_projection(portfolio, private$projection.T1)
                       }
                       
                       
                       # adjust for T2
                       if(mode > SETTLEMENT.T1) {
                         private$apply_projection(portfolio, private$projection.T2)
                       }
                       
                       portfolio
                     },
                     
                     apply_projection = function(portfolio, projection) {
                       #print(projection)
                       if(length(projection) >= 1) {
                         for(trade in projection) {
                           
                           if(is.null(trade$assetID)) {
                             next()
                           }
                           
                           # open new position
                           if( !(trade$assetID %in% names(portfolio$positions))) {
                             portfolio$positions[[trade$assetID]] <- trade$getSignedQuantity()
                             portfolio$positions[[DEFAULT.CURRENCY]] <- portfolio$positions[[DEFAULT.CURRENCY]] - trade$getExecutedAmount()
                           }
                           
                           # update existing position
                           qty.before <- portfolio$positions[[trade$assetID]]
                           qty.after <- qty.before + trade$getSignedQuantity()
                           
                           # taking position in the same direction
                           if(sign(qty.before) * sign(qty.after) == 1) {
                             portfolio$positions[[trade$assetID]] <- qty.after
                             qty.traded <- abs(qty.after - qty.before)
                             # increase position
                             if(abs(qty.after) > abs(qty.before)) {
                               portfolio$positions[[DEFAULT.CURRENCY]] <- portfolio$positions[[DEFAULT.CURRENCY]] - trade$getExecutedAmount()
                             } else {
                               # descrease position
                               portfolio$positions[[DEFAULT.CURRENCY]] <- portfolio$positions[[DEFAULT.CURRENCY]] + trade$getExecutedAmount()
                             }
                           }
                           
                           # reverse position or close existing one
                           if(sign(qty.before) * sign(qty.after) <= 0) {
                             portfolio$positions[[trade$assetID]] <- qty.after
                             qty.traded <- abs(qty.before) + abs(qty.after)
                             
                             portfolio$positions[[DEFAULT.CURRENCY]] <- portfolio$positions[[DEFAULT.CURRENCY]] + trade$getExecutedAmount() * abs(qty.before) / abs(qty.traded) 
                             portfolio$positions[[DEFAULT.CURRENCY]] <- portfolio$positions[[DEFAULT.CURRENCY]] - trade$getExecutedAmount() * abs(qty.after) / abs(qty.traded)
                           }
                         } 
                       }
                     }
                     
                   ),
                   public = list(
                     
                     initialize = function(account_id, account_type, initial_cash = 0) {
                       private$accountID = account_id
                       private$accountType = account_type
                       private$cash = initial_cash
                       private$currency = DEFAULT.CURRENCY
                       private$accountType = DEFAULT.ACCOUNT.TYPE
                       
                       # update limits
                       private$limit_T2 <- AccountLimit$new()
                       private$limit_T2$calculate(portfolio = private$get_portfolio(SETTLEMENT.T2))
                     },
                     
                     # Get Properties ----------------------------------------------------------------------
                     getID = function() { private$accountID},
                     
                     get_state = function() {
                       account.state <- AccountState$new(private$accountID, Current.Date)
                       
                       #account.state$portfolio[[SLICE.T0]] <- private$get_portfolio(SETTLEMENT.T0)
                       #account.state$portfolio[[SLICE.T1]] <- private$get_portfolio(SETTLEMENT.T2)
                       account.state$portfolio[[SLICE.T2]] <- private$get_portfolio(SETTLEMENT.T2)
                       
                       account.state$limits[[SLICE.T2]] <- private$limit_T2
                       
                       account.state$cumulative.tax.liability <- self$get_tax_liability()
                       
                       return(account.state)
                     },
                     
                     get_value = function() {
                       private$limit_T2$get_value()
                     },
                     
                     get_cash = function() {
                       private$cash
                     },
                     
                     get_long_margin = function() {
                       private$limit_T2$get_long_margin()
                     },
                     
                     get_short_margin = function() {
                       private$limit_T2$get_short_margin()
                     },
                     
                     
                     # Get Calculated Measures --------------------------------------------------------------------------
                     
                     get_tax_liability = function() {
                       mv = 0
                       if(private$accountType != ACCOUNT.TYPE.TAX.EXEMPT) {
                         if(length(private$holdings) >= 1) {
                           for(k in seq(1,length(private$holdings))) {
                             mv = mv + private$holdings[[k]]$getTaxLiability()
                           }
                         }
                       }
                       return(as.numeric(mv))
                     },
                     
                     getUnrealizedPnL = function() {
                       
                     },
                     
                     # Update Account --------------------------------------------------------------------------
                     
                     processTrade = function( trade ) {
                       
                       # expense commission
                       self$expense_costs( trade$getCommission() )
                       
                       # create new holding if needed
                       if( !(trade$assetID %in% names(private$holdings))) {
                         private$holdings[[trade$assetID]] = Holding$new(trade$assetID)
                       }
                       
                       # settle trade if immediate
                       if(trade$settlementMode == SETTLEMENT.T0) {
                         private$holdings[[trade$assetID]]$update(
                           qty   = trade$getSignedQuantity(),
                           price = trade$ex.price,
                           date  = trade$date)
                         
                         self$settle_trade(trade)
                         
                       } else {
                         # deferred settlement
                         # add trade to the queue
                         if(trade$settlementMode == SETTLEMENT.T2) {
                           private$T2.k <- private$T2.k + 1
                           private$projection.T2[[private$T2.k]] <- trade
                         }
                         
                         if(trade$settlementMode == SETTLEMENT.T1) {
                           private$T1.k <- private$T1.k + 1
                           private$projection.T1[[private$T1.k]] <- trade
                         }
                       }
                     },
                     
                     # settle trade
                     settle_trade = function(trade) {
                       
                       # calculate quantity before and quantity after
                       qty.before <- private$holdings[[trade$assetID]]$get_quantity()
                       qty.after <- qty.before + trade$getSignedQuantity()
                       
                       # update holdings
                       private$holdings[[trade$assetID]]$update(
                         qty   = trade$getSignedQuantity(),
                         price = trade$ex.price,
                         date  = trade$date)
                       
                       # update cash
                       if(trade$direction == DIRECTION.BUY) {
                         self$settle_buy_trade(trade$getExecutedAmount(), qty.before, qty.after)
                       }
                       
                       if(trade$direction == DIRECTION.SELL) {
                         self$settle_sell_trade(trade$getExecutedAmount(), qty.before, qty.after)
                       }
                     },
                     
                     settle_buy_trade = function(amount, qty.before, qty.after) {
                       
                       qty.long = 0
                       qty.close.short = 0
                       qty.traded = qty.after - qty.before
                       
                       # Long Only
                       if(qty.before >= 0) {
                         self$update_cash( -abs(amount) )
                       }
                       
                       # Closing Short
                       if(qty.before < 0) {
                         
                         # Standing Short
                         if(qty.after <= 0) {
                           qty.long = 0
                           qty.close.short = qty.traded
                           self$update_cash( abs(amount * qty.close.short /  qty.traded) )
                         }
                         
                         # Reverse to Long
                         if(qty.after > 0) {
                           qty.long = qty.after
                           qty.close.short = qty.before
                           
                           self$update_cash( abs(amount * qty.close.short /  qty.traded) )
                           self$update_cash( -abs(amount * qty.long /  qty.traded) )
                         }
                       }
                     },
                     
                     settle_sell_trade = function(amount, qty.before, qty.after) {
                       
                       qty.short = 0
                       qty.close.long = 0
                       qty.traded = qty.after - qty.before
                       
                       print(paste(amount, qty.traded, qty.before, qty.after))
                       
                       if(qty.before <= 0) {
                         self$update_cash( -abs(amount) )
                       }
                       
                       if(qty.before > 0) {
                         if(qty.after >= 0) {
                           qty.short = 0
                           qty.close.long = qty.traded
                           self$update_cash( abs(amount * qty.close.long / qty.traded) )
                         }
                         
                         if(qty.after < 0) {
                           qty.close.long = qty.before
                           qty.short = qty.after
                           
                           self$update_cash( abs(amount * qty.close.long / qty.traded) )
                           self$update_cash(-abs(amount * qty.short / qty.traded) )
                         }
                       }
                     },
                     
                     # expense costs
                     expense_costs = function(cost) {
                       private$cash = private$cash - as.numeric(cost)
                     },
                     
                     # udate cash position
                     update_cash = function(adj) {
                       private$cash = private$cash + as.numeric(adj)
                     },
                     
                     # rolling over account overnight
                     rollover = function() {
                       
                       N = as.numeric(Current.Date - Previous.Date)
                       
                       
                       private$cash <- private$cash * (1. + BROKER.REBATE.RATE * N / 365)
                       
                       # update limits
                       private$limit_T2 <- AccountLimit$new()
                       private$limit_T2$calculate(portfolio = private$get_portfolio(SETTLEMENT.T2))
                       
                       # settle T1 -> To
                       if(length(private$projection.T1) > 0) {
                         for(k in seq(1,private$T1.k)) {
                           
                           if(indexExists(k, private$projection.T1) &&
                              !is.null(private$projection.T1[[k]] )) {
                             
                             trade <- private$projection.T1[[k]]
                             self$settle_trade(trade)
                             private$projection.T1[[k]] <- NULL
                           }
                         }
                       }
                       
                       # rollover T2 -> T1
                       if(length(private$projection.T2) > 0) {
                         for(k in seq(1,private$T2.k)) {
                           
                           if(indexExists(k, private$projection.T2) &&
                              !is.null(private$projection.T2[[k]])  ) {
                             private$T1.k <- private$T1.k + 1
                             private$projection.T1[[private$T1.k]] <- private$projection.T2[[k]]
                             private$projection.T2[[k]] <- NULL
                           }
                         }
                       }
                     }
                   )
)