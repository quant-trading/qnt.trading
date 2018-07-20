library(quantmod)

source("core/DataAdapterBase.R")

DataAdapterYahoo <- R6Class("DataAdapterYahoo",
                       inherit = DataAdapterBase,
                       
                       public = list(
                         initialize = function() {
                           
                         },
                         
                         get_current_price = function(asset_id, currency = DEFAULT.CURRENCY) {
                           quote <- getQuote(asset_id)
                           quote$Last
                         }
                       ),
                       
                       private = list()
)