# Mikhail Andreev (c) 2017
# Broker Simulation

FIFO.TAX.LOTS = 1  # FIFO
LIFO.TAX.LOTS = 2  # LIFO
AWC.TAX.LOTS  = 3 # Average Weighted Cost

source("broker/Account.R")

Broker <- R6Class("Broker",
                   
                   private = list(
                     accounts = list(),
                     
                     
                     calculateBrokerCommission = function(order) {
                       return(0)
                     }
                   ),
                   public = list(
                     
                     initialize = function() {
                     },
                     
                     
                     createAccount = function(account_id, account_type, initial_cash = 0) {
                       private$accounts[[account_id]] <- Account$new(account_id, account_type, initial_cash)
                     },
                     

                     
                     
                     processTradingOrders = function(exchange, orders) {
                       
                       # TODO: check if we have enough money to process trading order 
                       
                       executedOrders <- exchange$executeTradingOrders(orders)
                       
                       # TODO: split execution results
                       
                     }
                     
                     
                   )
)