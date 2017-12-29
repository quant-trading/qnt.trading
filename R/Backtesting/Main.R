rm(list = ls() )

library(R6)
library(quantmod)
library(PerformanceAnalytics)

source("config/StrategyConfig.R")
source("backtest/BackTest.R")


BENCHMARK = "MICEXINDEXCF.ME" #"^GSPC"
DATE.PATTERN = '%Y-%m-%d'
START.DATE = '2013-03-15'
END.DATE = '2013-04-17'

backtest <- BackTest$new()

backtest$run()
