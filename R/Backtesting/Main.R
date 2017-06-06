library(R6)
library(quantmod)
library(PerformanceAnalytics)


source("StrategyBase.R")
source("Universe.R")

BENCHMARK = "MICEXINDEXCF.ME" #"^GSPC"
START.DATE = '2013-03-15'
END.DATE = '2017-03-15'
HOLDING.PERIOD = 1
DIV.THRESHOLD = 0.1


universe <- Universe$new() 

strategy <- StrategyBase$new("Test strategy")

strategy$setLocalUniverse(universe$getAssets())


strategy$simulate(start = START.DATE, end = END.DATE)

strategy$showResults()