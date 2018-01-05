# Mikhail Andreev (c) 2017
# Trading Account


source("holdings/Holding.R")
source("results/AccountState.R")

Account <- R6Class("Account",
                   
                   private = list(
                     accountID = NULL,
                     accountType = NULL,
                     holdings = list(),
                     
                     projection.T1 = list(),
                     projection.T2 = list(),
                     
                     T1.k = 0,
                     T2.k = 0,
                     
                     currency = NULL,
                     free.cash = as.numeric(0),
                     blocked.cash = as.numeric(0),
                     marginal.cash = as.numeric(0)
                   ),
                   public = list(
                     
                     initialize = function(account_id, account_type, initial_cash = 0) {
                       private$accountID = account_id
                       private$accountType = account_type
                       private$free.cash = initial_cash
                       private$currency = DEFAULT.CURRENCY
                       private$accountType = DEFAULT.ACCOUNT.TYPE
                     },
                     
                     # Get Properties ----------------------------------------------------------------------
                     getID = function() { private$accountID},
                     
                     getState = function() {
                       account.state <- AccountState$new(private$accountID, Current.Date)
                       
                       account.state$cash.blocked = round(private$blocked.cash, 2)
                       account.state$cash.available = round(private$free.cash, 2)
                       account.state$cash.marginal = round(private$marginal.cash, 2)
                       account.state$cash.total = self$getCashAmount()
                       account.state$total.mv.T0 = self$getNetMarketValue( SETTLEMENT.T0  )
                       account.state$cumulative.tax.liability = self$getTaxLiability()
                       if(length(private$holdings) > 0) {
                         account.state$tmp = private$holdings[['GAZP.ME']]$getNetQuantity()
                       } else {
                         account.state$tmp = 0
                       }
                       
                       return(account.state)
                     },
                     
                     getHoldings = function(mode) {
                       private$holdings
                     },
                     
                     getCashAmount = function(mode) {
                       return(as.numeric(private$free.cash + private$blocked.cash))
                     },
                     
                     get_marginal_cash = function() {
                       private$marginal.cash
                     },
                     
                     get_available_cash = function() {
                       private$free.cash
                     },

                     # Get Calculated Measures --------------------------------------------------------------------------
                     getHoldingsMarketValue = function(mode) {
                       mv = 0
                       if(length(private$holdings) >= 1) {
                         for(k in seq(1,length(private$holdings))) {
                           mv = mv + private$holdings[[k]]$getNetMarketValue()
                         }
                       }
                       return(as.numeric(mv))
                     },
                     
                     getShortMarketExposure = function(mode) {
                       mv = 0
                       if(length(private$holdings) >= 1) {
                         for(k in seq(1,length(private$holdings))) {
                           expo <- private$holdings[[k]]$getMarketExposure()
                           mv = mv + min(0, expo)
                         }
                       }
                       return(as.numeric(mv))                       
                     },
                     
                     getGrossMarketValue = function(mode) {
                       
                       mv = private$free.cash + private$blocked.cash
                         
                       if(length(private$holdings) >= 1) {
                         for(k in seq(1,length(private$holdings))) {
                           mv = mv + private$holdings[[k]]$getGrossMarketValue()
                         }
                       }
                       return(as.numeric(mv))                       
                     },
                     
                     getTaxLiability = function() {
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
                     
                     getNetMarketValue = function(mode) {
                       cash = self$getCashAmount()
                       holdings.mv = self$getHoldingsMarketValue()
                       return(cash + holdings.mv + private$marginal.cash)
                     },
                     
                     getUnrealizedPnL = function() {
                       
                     },
                     
                     # Update Account --------------------------------------------------------------------------
                     
                     processTrade = function( trade ) {
                       
                       # expense commission
                       self$expenseCosts( trade$getCommission() )
                       
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
                         
                         # update free cash 
                         self$updateCash( trade$getExecutedAmount() )
                         
                       } else {
                         # deferred settlement
                         
                         # block initial margin
                         #self$blockMargin(trade$initial.margin)
                         
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
                     
                     rollover = function() {
                       
                       # settle T1 -> To
                       if(length(private$projection.T1) > 0) {
                         for(k in seq(1,private$T1.k)) {
                           
                           if(indexExists(k, private$projection.T1) &&
                              !is.null(private$projection.T1[[k]] )) {
                             trade <- private$projection.T1[[k]]
                             
                             qty.before <- private$holdings[[trade$assetID]]$getNetQuantity()
                             
                             # release margin
                             #self$releaseMargin(abs(trade$getExecutedAmount()))
                             
                             # settle trade
                             private$holdings[[trade$assetID]]$update(
                               qty   = trade$getSignedQuantity(),
                               price = trade$ex.price,
                               date  = trade$date)
                             
                             qty.after <- qty.before + trade$getSignedQuantity()
                             
                             if(trade$direction == DIRECTION.BUY) {
                               self$updateMarginalPosition_BUY(trade$getExecutedAmount(), qty.before, qty.after)
                             }
                             
                             if(trade$direction == DIRECTION.SELL) {
                               self$updateMarginalPosition_SELL(trade$getExecutedAmount(), qty.before, qty.after)
                             }
                             
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
                             # release initial margin
                             #self$releaseMargin(private$projection.T2[[k]]$initial.margin)
                             
                             # block full cost of trade
                             #self$blockMargin(abs(private$projection.T2[[k]]$getExecutedAmount()))
                             
                             private$projection.T1[[private$T1.k]] <- private$projection.T2[[k]]
                             private$projection.T2[[k]] <- NULL
                           }
                         }
                       }
                       
                       for(h in private$holdings) {
                         print(paste(h$getID(), h$getNetQuantity()))
                       }
                       
                       # update marginal requirements
                       # TODO: implement TRUE T+2 mode
                       self$update_margin(SETTLEMENT.T2)
                     },
                     
                     update_margin = function(mode) {
                       marginal.assets = self$getGrossMarketValue(mode) - self$getNetMarketValue(mode)
                       self$releaseMargin(private$blocked.cash)
                       self$blockMargin(marginal.assets * DEFAULT.INITIAL.MARGIN)

                     },
                     
                     updateMarginalPosition_BUY = function(amount, qty.before, qty.after) {
                       
                       qty.long = 0
                       qty.close.short = 0
                       qty.traded = qty.after - qty.before
                       
                       # Long Only
                       if(qty.before >= 0) {
                         self$updateCash( amount )
                       }
                       
                       # Closing Short
                       if(qty.before < 0) {
                         
                         # Standing Short
                         if(qty.after <= 0) {
                           qty.long = 0
                           qty.close.short = qty.traded
                         }
                         
                         # Reverse to Long
                         if(qty.after > 0) {
                           qty.long = qty.after
                           qty.close.short = qty.before
                         }
                         
                         self$updateCash( amount * qty.long /  qty.traded )
                         self$updateMarginalCash( amount * qty.close.short /  qty.traded )
                       }
                     },
                     
                     updateMarginalPosition_SELL = function(amount, qty.before, qty.after) {
                       
                       qty.short = 0
                       qty.close.long = 0
                       qty.traded = qty.after - qty.before
                       
                       print(paste(amount, qty.traded, qty.before, qty.after))
                       
                       if(qty.before <= 0) {
                         self$updateMarginalCash( amount )
                       }
                       
                       if(qty.before > 0) {
                         if(qty.after >= 0) {
                           qty.short = 0
                           qty.close.long = qty.traded
                         }
                         
                         if(qty.after < 0) {
                           qty.close.long = qty.before
                           qty.short = qty.after
                         }
                         
                         self$updateCash( amount * abs(qty.close.long / qty.traded) )
                         self$updateMarginalCash( amount * abs( qty.short / qty.traded) )
                         
                       }
                       
                     },

                     blockMargin = function(amount) {
                       private$free.cash = private$free.cash - amount
                       private$blocked.cash = private$blocked.cash + amount
                     },

                     releaseMargin = function(amount) {
                       
                       private$free.cash = private$free.cash + amount
                       private$blocked.cash = private$blocked.cash - amount
                       
                     },

                     expenseCosts = function(cost) {
                       private$free.cash = private$free.cash - as.numeric(cost)
                     },

                     updateCash = function(adj) {
                       private$free.cash = private$free.cash + as.numeric(adj)
                     },
                     
                     updateMarginalCash = function(adj) {
                       #print(paste("Marginal Cash Update:", adj))
                       #private$marginal.cash = private$marginal.cash + as.numeric(adj)
                       #private$free.cash <- private$free.cash - as.numeric(adj) * DEFAULT.INITIAL.MARGIN
                       #private$blocked.cash <- private$blocked.cash + as.numeric(adj) * DEFAULT.INITIAL.MARGIN
                     }
                   )
)