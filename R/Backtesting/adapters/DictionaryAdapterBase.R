# Mikhail Andreev (c) 2017
# Dictionary for Financial Instruments

source("adapters/FinancialInstrument.R")

DICTIONARY.FILE = 'data/instruments/instruments.csv'

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
                               }
                             )
)