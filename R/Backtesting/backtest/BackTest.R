# Mikhail Andreev (c) 2017
# Backtest Core Framework

source("broker/Broker.R")
source("exchange/Exchange.R")
source("quantcore/QuantCore.R")
source("adapters/AdapterInstance.R")

DEFAULT.ACCOUNT.NAME = "TestAccount01"
INITIAL.BUDGET.VALUE = 100000

DIRECTION.BUY = 1
DIRECTION.SELL = -1

SLICE.T0 = 0
SLICE.T1 = 1
SLICE.T2 = 2

Current.Date <- 0

BackTest <- R6Class("BackTest",
                    
                    private = list(
                      broker = NULL,
                      exchange = NULL,
                      quantCore = NULL,
                      dataAdapter = NULL,
                      
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
                        private$quantCore = QuantCore$new(STRATEGY.ID)
                        private$dataAdapter = Global.Quote.Adapter
                      },
                      
                      run = function() {
                        print("Starting backtest...")
                        private$preRun()
                        
                        dt <- as.Date(START.DATE, format = DATE.PATTERN)
                        
                        Current.Date <<- dt
                        
                        while(dt <= as.Date(END.DATE, format = DATE.PATTERN) ) {
                          
                          dt <- private$exchange$getNextTradingDate(dt)
                          print(dt)
                          
                          tradingOrders <- private$quantCore$getTradingOrders(date = dt, adapter = private$dataAdapter)
                          
                          if(!is.null(tradingOrders)) {
                            private$broker$processTradingOrders(exchange = private$exchange, 
                                                                orders = tradingOrders,
                                                                account_id = DEFAULT.ACCOUNT.NAME)
                          }
                          
                          print(private$broker$getAccountValue(DEFAULT.ACCOUNT.NAME, SLICE.T0))
                          
                          # rollover accoints Tn -> Tn+1
                          private$broker$rolloverAccounts()
                        }
                      }
                    )
)
