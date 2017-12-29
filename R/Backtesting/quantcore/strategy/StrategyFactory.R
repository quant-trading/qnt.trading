# Mikhail Andreev (c) 2017
# Strategy Factory

source("quantcore/strategy/StrategyTest.R")

StrategyFactory <- R6Class("StrategyFactory",
                           
                           public = list(
                             
                             initialize = function() {
                               
                             },
                             
                             createStrategy = function(strategy.id) {
                               if(strategy.id == "Test") {
                                 return(StrategyTest$new())
                               }
                               return(NULL)
                             }
                             
                           )
                           
                           )