source("core/TradingSignalsGenerator.R")

CrytpoTest01.SignalsGenerator = R6Class("CrytpoTest01.SignalsGenerator",
                                       inherit = TradingSignalsGeneratorBase,
                                       
                                       public = list(
                                         initialize = function() {},
                                         
                                         generate_trading_signals = function() {
                                           coins.data <- crypto_prices()
                                           coins.data <- coins.data[order(coins.data$percent_change_24h),]
                                           coins.data <- coins.data[coins.data$`24h_volume_usd` >= 10000,]
                                           
                                           N <- min(5, NROW(coins.data))
                                           
                                           signals <- list()
                                           
                                           for(k in seq(1, N)) {
                                             signal <- TradingSignal$new(asset = coins.data[k,]$symbol, 
                                                                         q = 1, 
                                                                         dir = SIGNAL.BUY, 
                                                                         scr = k)
                                             
                                             signal$method <- "Lowest24h.Return.1.0"
                                             signal$predicted_change <- NULL
                                             signal$predicted_price <- NULL
                                             signal$current_price = coins.data[k,]$price_btc
                                             signal$horizon <- 1
                                             
                                             signals[[k]] <- signal
                                           }
                                           
                                           signals
                                         }
                                       ))



