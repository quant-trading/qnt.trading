# Mikhail Andreev (c) 2017
# Trading Account

ACCOUNT.TYPE.STANDARD   = 1
ACCOUNT.TYPE.TAX.EXEMPT = 2

source("holdings/Holding.R")

Account <- R6Class("Account",
                   
                               private = list(
                                 accountID = NULL,
                                 accountType = NULL,
                                 holdings = list(),
                                 currency = NULL,
                                 cash = 0
                               ),
                               public = list(
                                 
                                 initialize = function(account_id, account_type, initial_cash = 0) {
                                   private$accountID = account_id
                                   private$accountType = account_type
                                   private$cash = initial_cash
                                 },
                                 
                                 
                                 getMarketValue = function() {
                                   return(0)
                                 },
                                 
                                 
                                 getCashAmount = function(mode) {
                                   return(private$cash)
                                 },
                                 
                                 
                                 getHoldings = function(mode) {
                                   private$holdings
                                 },
                                 
                                 
                                 expenseCosts = function(cost) {
                                   private$cash = private$cash - cost
                                 },
                                 
                                 
                                 updateHoldings = function(taxLots) {
                                   
                                 },
                                 
                                 
                                 getUnrealizedPnL = function() {
                                   
                                 }
                               )
)