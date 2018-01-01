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
                               }
                             )
)