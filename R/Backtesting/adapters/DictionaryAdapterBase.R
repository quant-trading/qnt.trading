# Mikhail Andreev (c) 2017
# Dictionary for Financial Instruments

source("adapters/FinancialInstrument.R")

DICTIONARY.FILE = 'data/instruments/instruments.csv'

SETTLEMENT.T0 = 0
SETTLEMENT.T1 = 1
SETTLEMENT.T2 = 2
SETTLEMENT.T3 = 3

DictionaryAdapterBase <- R6Class("DictionaryAdapterBase",
                             
                             private = list(
                               instruments = list()
                             ),
                             
                             public = list(
                               initialize = function() {
                                 data <- read.csv(file = DICTIONARY.FILE, header = T, stringsAsFactors = F)
                                 
                                 for(i in seq(1, NROW(data))) {
                                   instrument <- FinancialInstrument$new(data$Ticker)
                                   
                                   instrument$type <- data$Type
                                   instrument$lotSize <- data$Lot
                                   instrument$minStep <- data$MinStep
                                   
                                   #print(instrument)
                                   private$instruments[[data$Ticker]] <- instrument
                                 }
                               },
                               
                               getLotSize = function(assetID) {
                                 return(private$instruments[[assetID]]$lotSize)
                               },
                               
                               
                               getSettlementMode = function(assetID) {
                                return(SETTLEMENT.T2)  
                               },
                               
                               getAssetType = function(assetID) {
                                 return(private$instruments[[assetID]]$type)
                               },
                               
                               get_d_init_margin_long = function(asset_id) {
                                 return(0.14)
                               },
                               
                               get_d_init_margin_short = function(asset_id) {
                                 return(0.14)
                               },
                               
                               get_d_min_margin_long = function(asset_id) {
                                 return(0.072638)
                               },
                               
                               get_d_min_margin_short = function(asset_id) {
                                 return(0.067708)
                               },
                               
                               is_marginal = function(asset_id) {
                                 return(1)
                               }
                             )
)