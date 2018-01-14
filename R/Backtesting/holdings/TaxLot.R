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
                       
                       # check if lot is closed
                       if(self$qty == 0) {
                         self$closeDate = Current.Date
                         # TODO: fix for date
                       }
                     },
                     
                     
                     get_unr_pnl = function() {
                       if(self$qty == 0) {
                         return(0)
                       } else {
                         return( sign(self$qty) * (Global.Quote.Adapter$getQuote(self$ID, Current.Date) - self$price) * self$qty * Global.Dictionary.Adapter$getLotSize(self$ID))
                       }
                     }
                   )
)