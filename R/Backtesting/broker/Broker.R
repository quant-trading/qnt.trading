# Mikhail Andreev (c) 2017
# Broker Simulation


source("broker/Account.R")


Broker <- R6Class("Broker",
                   
                   private = list(
                     accounts = list(),
                     
                     
                     calculateBrokerCommission = function(trade) {
                       if(!BROKER.COMMISSION) {
                         return(0) 
                       } else { 
                         return(2) 
                         }
                     },
                     
                     calculateCommissionForMarginalShortPosition = function(mv) {
                       
                       n = Current.Date - Previous.Date
                       #print(paste("Days n", n))
                       
                       return(as.numeric(SHORT.MARGINAL.RATE / 365 * n * mv))
                     }
                   ),
                   public = list(
                     
                     initialize = function() {
                     },
                     
                     
                     getInitialMargin = function(trade) {
                       return(DEFAULT.INITIAL.MARGIN * abs(trade$getExecutedAmount()))
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
                      
                       # rollover account
                       for(account in private$accounts) {
                         
                         # charge for margin
                         holdings <- account$getHoldings(SETTLEMENT.T0)
                         
                         for(h in holdings) {
                           
                           print(paste(h$getID(), h$getNetQuantity()))
                           
                           if(h$getNetQuantity() < 0) {
                             
                             marginal.trade.fee <- private$calculateCommissionForMarginalShortPosition(
                               abs(h$getNetMarketValue())
                             )
                             print(paste("Fee", marginal.trade.fee))
                             account$expenseCosts( marginal.trade.fee )
                           }
                           
                         }
                         
                         # rollover
                         account$rollover()
                       }
                     },
                     
                     
                     getAccountStates = function() {
                       
                       states <- list()
                       
                       for(account in private$accounts) {
                         state <- account$getState()
                         state$saveToCsv()
                         states[[account$getID()]] <- state
                       }   
                      
                       return(states)
                     }
                     
                     
                   )
)