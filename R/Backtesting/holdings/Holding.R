# Mikhail Andreev (c) 2017
# Single Holding

source("holdings/TaxLot.R")

Holding <- R6Class("Holding",
                   
                   private = list(
                     ID = NULL,
                     assetType = NULL,
                     taxLots = list()
                   ),
                   
                   public = list (
                     initialize = function(ID) {
                       private$ID = ID
                     }
                   )
)