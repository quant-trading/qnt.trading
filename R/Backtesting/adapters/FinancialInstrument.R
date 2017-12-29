# Mikhail Andreev (c) 2017
# Financial Instrument

FinancialInstrument <- R6Class("FinancialInstrument",
                        
                               public = list(
                                 
                                 assetID = NULL,
                                 type = NULL,
                                 lotSize = NULL,
                                 minStep = NULL,
                                 
                                 initialize = function(ID) {
                                   self$assetID = ID
                                 }
                               )
)