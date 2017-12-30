# Mikhail Andreev (c) 2017
# Trading Account


source("holdings/Holding.R")

Account <- R6Class("Account",
                   
                   private = list(
                     accountID = NULL,
                     accountType = NULL,
                     holdings = list(),
                     #T_n.holdings = list(),
                     currency = NULL,
                     free.cash = as.numeric(0),
                     blocked.cash = as.numeric(0)
                   ),
                   public = list(
                     
                     initialize = function(account_id, account_type, initial_cash = 0) {
                       private$accountID = account_id
                       private$accountType = account_type
                       private$free.cash = initial_cash
                       private$currency = DEFAULT.CURRENCY
                       private$accountType = DEFAULT.ACCOUNT.TYPE
                     },
                     
                     
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
                       }
                       
                     },
                     
                     
                     getHoldingsMarketValue = function(mode) {
                       mv = 0
                       if(length(private$holdings) >= 1) {
                         for(k in seq(1,length(private$holdings))) {
                           mv = mv + private$holdings[[k]]$getNetMarketValue()
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
                     
                     
                     getTotalMarketValue = function(mode) {
                       cash = self$getCashAmount()
                       holdings.mv = self$getHoldingsMarketValue()
                       return(cash + holdings.mv)
                     },
                     
                     
                     getCashAmount = function(mode) {
                       return(as.numeric(private$free.cash + private$blocked.cash))
                     },
                     
                     
                     getHoldings = function(mode) {
                       private$holdings
                     },
                     
                     
                     expenseCosts = function(cost) {
                       private$free.cash = private$free.cash - as.numeric(cost)
                     },
                     
                     
                     updateCash = function(adj) {
                       private$free.cash = private$free.cash + as.numeric(adj)
                     },
                     
                     
                     getUnrealizedPnL = function() {
                       
                     }
                   )
)