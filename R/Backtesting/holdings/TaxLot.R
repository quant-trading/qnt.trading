# Mikhail Andreev (c) 2017
# Single Tax Lot

TaxLot <- R6Class("TaxLot",
                   
                   public = list(
                     ID = NULL,
                     
                     qty = NULL,
                     price = NULL,
                     openDate = NULL,
                     closeDate = NULL,
                     
                     realized.pnl = as.numeric(0),
                     tax.liability = as.numeric(0),

                     initialize = function(ID, qty, price, date) {
                       self$ID = ID
                       self$qty = as.numeric(qty)
                       self$price = as.numeric(price)
                       self$openDate = date
                     },
                     
                     
                     getHoldingPeriod = function() {
                       return(0)
                     },
                     
                     
                     closeLot = function(offset.qty, price, date) {
                       
                       self$realized.pnl = self$realized.pnl + sign(self$qty) * (as.numeric(price) - self$price) * offset.qty * Global.Dictionary.Adapter$getLotSize(self$ID)
                       self$qty = self$qty - offset.qty * sign(self$qty)
                       self$tax.liability = self$realized.pnl * DEFAULT.TAX.RATE
                       
                       #print(paste("Close lot", self$realized.pnl, price, self$price))
                       #print(self$qty)
                       # check if lot is closed
                       if(self$qty == 0) {
                         self$closeDate = Current.Date
                         # TODO: fix for date
                       }
                       
                     }
                   )
)