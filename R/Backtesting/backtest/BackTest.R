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
                        
                        Current.Date <<- dt
                        
                        while(dt <= as.Date(END.DATE, format = DATE.PATTERN) ) {
                          

                          
                          dt <- private$exchange$getNextTradingDate(dt)
                          Previous.Date <<- Current.Date
                          Current.Date <<- dt
                          print("________________________")
                          print(dt)
                          
                          # rollover accoints Tn -> Tn+1
                          private$broker$rolloverAccounts()
                          
                          # SOD states
                          states <- private$broker$getAccountStates()
                          
                          tradingOrders <- private$quantCore$getTradingOrders(date = dt, states[[DEFAULT.ACCOUNT.NAME]]$limits[[SLICE.T2]])
                          
                          if(!is.null(tradingOrders)) {
                            private$broker$processTradingOrders(exchange = private$exchange, 
                                                                orders = tradingOrders,
                                                                account_id = DEFAULT.ACCOUNT.NAME)
                          }
                          
                          print(private$broker$getAccountValue(DEFAULT.ACCOUNT.NAME, SETTLEMENT.T0))
                          #print(private$broker$getAccountTaxLiability(DEFAULT.ACCOUNT.NAME))
                        
                          # EOD states
                          states <- private$broker$getAccountStates()
                          if(states[[DEFAULT.ACCOUNT.NAME]]$limits[[SLICE.T2]]$get_value() <= 100) {
                              print("=== TOTAL LOSS ===")
                            break()
                          }
                          
                          if(states[[DEFAULT.ACCOUNT.NAME]]$limits[[SLICE.T2]]$get_UDS() < 0) {
                            print("=== MARGIN CALL ===")
                            break()
                          }
                          
                          private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$addState(states[[DEFAULT.ACCOUNT.NAME]])
                          
                        }
                        
                        # draw results
                        private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$plotDynamics()
                        
                        # dump csv
                        private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$save()
                        
                        private$accountPerformance[[DEFAULT.ACCOUNT.NAME]]$print_statistics()
                      }
                    )
)
