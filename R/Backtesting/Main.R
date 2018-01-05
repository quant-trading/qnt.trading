rm(list = ls() )

library(R6)
library(quantmod)
library(PerformanceAnalytics)

source("config/StrategyConfig.R")
source("utils/system/SystemUtils.R")
source("backtest/BackTest.R")

warning()

BENCHMARK = "MICEXINDEXCF.ME" #"^GSPC"
DATE.PATTERN = '%Y-%m-%d'
START.DATE = '2013-01-03'
END.DATE =   '2017-12-27'

backtest <- BackTest$new()

backtest$run()
