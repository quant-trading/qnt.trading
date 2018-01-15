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
                        return(trade$getExecutedAmount() * BROKER.COMMISSION.RATE ) 
                      }
                    },
                    
                    calculateCommissionForMarginalShortLoan = function(mv) {
                      n = as.numeric(Current.Date - Previous.Date)
                      return(round(as.numeric(SHORT.MARGINAL.RATE / 365 * n * mv), 2))
                    },
                    
                    
                    calculateCommissionForMarginalLongLoan = function(mv) {
                      n = as.numeric(Current.Date - Previous.Date)
                      return(round(as.numeric(LONG.MARGINAL.RATE / 365 * n * mv), 2))
                    }
                  ),
                  public = list(
                    
                    initialize = function() {
                    },
                    
                    
                    createAccount = function(account_id, account_type, initial_cash = 0) {
                      private$accounts[[account_id]] <- Account$new(account_id, account_type, initial_cash)
                    },
                    
                    
                    getAccountValue = function(account_id, mode) {
                      return(private$accounts[[account_id]]$get_value())
                    },
                    
                    
                    getAccountTaxLiability = function(account_id) {
                      return( private$accounts[[account_id]]$getTaxLiability() )
                    },
                    
                    
                    processTradingOrders = function(exchange, orders, account_id) {
                      
                      # TODO: check if we have enough money to process trading order 
                      
                      trades <- exchange$executeTradingOrders(orders)
                      
                      # TODO: split execution results
                      for(trade in trades) {
                        # broker commission
                        trade$commission <- trade$commission + private$calculateBrokerCommission( trade )
                        
                        # update account
                        private$accounts[[account_id]]$processTrade(trade)
                      }
                    },
                    
                    
                    rolloverAccounts = function() {
                      
                      # rollover account
                      for(account in private$accounts) {
                        
                        # rollover
                        account$rollover()
                        
                        # # charge for margin
                        if(account$get_short_margin() > 0.001) {
                          marginal.trade.fee <- private$calculateCommissionForMarginalShortLoan(account$get_short_margin())
                          print(paste("Fee S", marginal.trade.fee))
                          account$expense_costs( marginal.trade.fee ) 
                        }
                        
                        if(account$get_long_margin() > 0.001) {
                          marginal.trade.fee <- private$calculateCommissionForMarginalLongLoan(account$get_long_margin())
                          print(paste("Fee L", marginal.trade.fee))
                          account$expense_costs( marginal.trade.fee )
                        }
                      }
                    },
                    
                    
                    getAccountStates = function() {
                      
                      states <- list()
                      
                      for(account in private$accounts) {
                        state <- account$get_state()
                        state$saveToCsv()
                        states[[account$getID()]] <- state
                      }   
                      
                      return(states)
                    }
                    
                    
                  )
)