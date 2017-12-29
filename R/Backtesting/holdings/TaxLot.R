# Mikhail Andreev (c) 2017
# Single Tax Lot

TaxLot <- R6Class("TaxLot",
                   
                   public = list(
                     ID = NULL,
                     qty = NULL,
                     price = NULL,
                     openDate = NULL,
                     accountId = NULL,

                     initialize = function(ID) {
                       self$ID = ID
                     }
                   )
)