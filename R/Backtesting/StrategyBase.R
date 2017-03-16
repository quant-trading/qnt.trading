source("PerformanceAnalyzer.R")
source("Dao.Base.R")


StrategyBase <- R6Class("StrategyBase",
                  public = list(
                    name = NULL,
                    initialize = function(name = NA) {
                      self$name <- name
                      
                      private$analyzer = PerformanceAnalyzer$new()
                      private$dao.quotes <- DAO.Base$new()
                    },
                    
                    setLocalUniverse = function(names){
                      private$assets$names = names
                      private$assets$quotes = private$dao.quotes$loadQuotes(asset = names)
                    },
                    
                    createTradingSignals = function() {
                      
                      dvi <- DVI(Ad(private$assets$quotes))
                      private$tradingSignals <- Lag(ifelse(dvi$e1 < DIV.THRESHOLD, 1, ifelse(dvi$e1 > 1 - DIV.THRESHOLD, -1, 0)))
                      
                    },
                    
                    simulate = function(start, end) {
                      
                      self$createTradingSignals()
                      
                      private$returns <- ROC(Cl(private$assets$quotes), n = HOLDING.PERIOD )*private$tradingSignals
                      private$returns <- private$returns[paste0(start,"/",end)]
                      
                      # calc benchmark performance
                      private$benchmark <- ROC(Cl(getSymbols(BENCHMARK, auto.assign = F)),  n = 1)
                      private$benchmark <- private$benchmark[paste0(start,"/",end)]
                    },
                    
                    showResults = function() {
                      private$analyzer$analyze(private$returns, private$benchmark)
                    }
                  ),
                    private = list(
                      dao.quotes = NULL,
                      assets = NULL,
                      analyzer = NULL,
                      tradingSignals = NULL,
                      returns = NULL,
                      benchmark = NULL
                    )
                  )
