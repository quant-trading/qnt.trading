# Mikhail Andreev (c) 2017
# Broker Simulation


source("broker/Account.R")

Broker <- R6Class("Broker",
                   
                   private = list(
                     accounts = list(),
                     
                     
                     calculateBrokerCommission = function(trade) {
                       return(2)
                     }
                   ),
                   public = list(
                     
                     initialize = function() {
                     },
                     
                     getInitialMargin = function(trade) {
                       return(0.3)
                     },
                     
                     
                     createAccount = function(account_id, account_type, initial_cash = 0) {
                       private$accounts[[account_id]] <- Account$new(account_id, account_type, initial_cash)
                     },
                     
                     
                     getAccountValue = function(account_id, mode) {
                       return(private$accounts[[account_id]]$getTotalMarketValue(mode))
                     },
                     
                     
                     getAccountTaxLiability = function(account_id) {
                       return( private$accounts[[account_id]]$getTaxLiability() )
                     },
                     
                     processTradingOrders = function(exchange, orders, account_id) {
                       
                       # TODO: check if we have enough money to process trading order 
                       
                       trades <- exchange$executeTradingOrders(orders)
                       
                       # TODO: split execution results
                       for(trade in trades) {
                         
                           # set initial margin requirements
                           trade$initial.margin <- self$getInitialMargin(trade)
                           
                           # broker commission
                           trade$commission = trade$commission + private$calculateBrokerCommission( trade )
                           
                           # update account
                           private$accounts[[account_id]]$processTrade(trade)

                       }
                       
                     },
                     
                     
                     rolloverAccounts = function() {
                       
                       # charge for margin
                       
                       # rollover account
                     }
                     
                     
                   )
)