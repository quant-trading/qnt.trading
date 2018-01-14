# Mikhail Andreev (c) 2017
# Single Holding

source("holdings/TaxLot.R")

Holding <- R6Class("Holding",
                   
                   private = list(
                     ID = NULL,
                     assetType = NULL,
                     taxLots = list(),
                     
                     cumTaxLiability = 0,
                     
                     update_FIFO = function(qty, price, date) {
                       
                       for(k in seq(1, length(private$taxLots))) {
                         
                         if(private$taxLots[[k]]$qty != 0) {
                           
                           if(sign(private$taxLots[[k]]$qty) != sign(qty)) {
                             offset.q = min(abs(qty), abs(private$taxLots[[k]]$qty))
                             
                             private$taxLots[[k]]$closeLot(offset.q, price, date)
                             
                             qty = qty - offset.q * sign(qty)
                           }
                         }
                         
                         if(qty == 0) { break }
                       }
                       
                       # check if we need to create new lot
                       if(qty != 0) {
                         #print("Create New Lot")
                         lot = TaxLot$new(private$ID, qty, price, date)
                         private$taxLots[[length(private$taxLots) + 1]] = lot
                       }
                       
                     }
                     
                   ),
                   
                   public = list (
                     
                     initialize = function(ID) {
                       private$ID = ID
                       private$assetType = Global.Dictionary.Adapter$getAssetType(ID)
                     },
                     
                     
                     getID = function() {
                       private$ID
                     }, 
                     
                     
                     update = function(qty, price, date) {
                       
                       if(length(private$taxLots) < 1) {
                         # create first tax lot
                         lot = TaxLot$new(private$ID, qty, price, date)
                         private$taxLots[[1]] = lot
                         
                       } else {
                         # update existing tax lots
                         if(DEFAULT.TAX.LOT.ACCOUNTING == FIFO.TAX.LOTS) {
                           private$update_FIFO(qty, price, date)
                         }
                       }
                     },
                     
                     
                     get_quantity = function() {
                       q = as.numeric(0)
                       
                       if(length(private$taxLots) >= 1) {
                         for(k in seq(1, length(private$taxLots))) {
                           q = q + private$taxLots[[k]]$qty  
                         }
                       }
                       return(q)
                     },
                     
                     # getNetMarketValue = function() {
                     #   
                     #   if(self$get_quantity() < 0) {
                     #     return(self$get_unrealized_pnl())
                     #   } else {
                     #     return(self$get_quantity() * Global.Dictionary.Adapter$getLotSize(private$ID) * Global.Quote.Adapter$getQuote(private$ID, Current.Date))
                     #   }
                     # },
                     # 
                     # getMarketExposure = function() {
                     #     return(self$get_quantity() * Global.Dictionary.Adapter$getLotSize(private$ID) * Global.Quote.Adapter$getQuote(private$ID, Current.Date))
                     # },
                     # 
                     # getGrossMarketValue = function() {
                     #   return(abs(self$getMarketExposure()))
                     # },
                     
                     
                     getTaxLiability = function() {
                       q = as.numeric(0)
                       
                       if(length(private$taxLots) >= 1) {
                         for(k in seq(1, length(private$taxLots))) {
                           q = q + private$taxLots[[k]]$tax.liability
                         }
                       }
                       return(q)
                     },
                     
                     
                     get_unrealized_pnl = function() {
                       q = as.numeric(0)
                       
                       if(length(private$taxLots) >= 1) {
                         for(k in seq(1, length(private$taxLots))) {
                           q = q + private$taxLots[[k]]$get_unr_pnl()
                         }
                       }
                       q
                     }
                     
                   )
)