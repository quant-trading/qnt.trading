# Mikhail Andreev (c) 2017
# Backtest Core Framework

source("broker/Broker.R")
source("exchange/Exchange.R")
source("quantcore/QuantCore.R")
source("adapters/AdapterInstance.R")
source("results/AccountPerformance.R")


Current.Date <- 0
Previous.Date <- 0

BackTest <- R6Class("BackTest",
                    
                    private = list(
                      broker = NULL,
                      exchange = NULL,
                      quantCore = NULL,
                      dataAdapter = NULL,
                      accountPerformance = list(),
                      
                      preRun = function() {
                        
                        # Initialiaze Accounts Here
                        private$broker$createAccount(DEFAULT.ACCOUNT.NAME, 
                                                     ACCOUNT.TYPE.STANDARD,
                                                     INITIAL.BUDGET.VALUE)
                        
                        private$accountPerformance[[DEFAULT.ACCOUNT.NAME]] <- AccountPerformance$new(DEFAULT.ACCOUNT.NAME)
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
                        
                        
                        
                        while(dt <= as.Date(END.DATE, format = DATE.PATTERN) ) {
                          
                          # rollover accoints Tn -> Tn+1
                          private$broker$rolloverAccounts()
                          
                          dt <- private$exchange$getNextTradingDate(dt)
                          Previous.Date <<- Current.Date
                          Current.Date <<- dt
                          print("________________________")
                          print(dt)
                          
                          tradingOrders <- private$quantCore$getTradingOrders(date = dt, adapter = private$dataAdapter)
                          
                          if(!is.null(tradingOrders)) {
                            private$broker$processTradingOrders(exchange = private$exchange, 
                                                                orders = tradingOrders,
                                                                account_id = DEFAULT.ACCOUNT.NAME)
                          }
                          
                          print(private$broker$getAccountValue(DEFAULT.ACCOUNT.NAME, SLICE.T0))
                          #print(private$broker$getAccountTaxLiability(DEFAULT.ACCOUNT.NAME))
                        
                          # EOD states
                          states <- private$broker$getAccountStates()
                          private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$addState(states[[DEFAULT.ACCOUNT.NAME]])
                          
                        }
                        
                        # draw results
                        private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$plotDynamics()
                      }
                    )
)
