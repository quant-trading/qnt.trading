# Mikhail Andreev (c) 2017
# Backtest Core Framework

source("broker/Broker.R")
source("exchange/Exchange.R")

DEFAULT.ACCOUNT.NAME = "TestAccount01"
INITIAL.BUDGET.VALUE = 100000

BackTest <- R6Class("BackTest",
                   
                   private = list(
                     broker = NULL,
                     exchange = NULL,
                     
                     preRun = function() {
                       
                       # Initialiaze Accounts Here
                       private$broker$createAccount(DEFAULT.ACCOUNT.NAME, 
                                                    ACCOUNT.TYPE.STANDARD,
                                                    INITIAL.BUDGET.VALUE)
                     }
                   ),
                    
                   public = list(
                     
                     initialize = function() {
                       private$broker = Broker$new()
                       private$exchange = Exchange$new()
                     },
                     
                     run = function() {
                       print("Starting backtest...")
                       private$preRun()
                     }
                     

                   )
)
