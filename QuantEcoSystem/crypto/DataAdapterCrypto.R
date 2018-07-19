library(crypto)

source("core/DataAdapterBase.R")

DataAdapterCrypto <- R6Class("DataAdapterCrypto",
                             
                             inherit = DataAdapterBase,
                             
                             public = list(
                               initialize = function() {
                                 self$update()
                               },
                               
                               get_current_price = function(asset_id, currency = DEFAULT.CURRENCY) {
                                 private$data[private$data$symbol == asset_id,]$price_usd
                               },
                               
                               update = function() {
                                 private$data <- crypto_prices()
                               }
                             ),
                             
                             private = list(
                               data = NULL
                             ))