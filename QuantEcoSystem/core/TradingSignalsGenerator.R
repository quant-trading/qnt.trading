source("core/TradingSignal.R")

TradingSignalsGeneratorBase <- R6Class("TradingSignalsGeneratorBase",
                                       
                                       public = list(
                                         initialize = function() {},
                                         generate_trading_signals = function(strategy_id) {}
                                       ))