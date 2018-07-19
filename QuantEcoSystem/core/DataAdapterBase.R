DataAdapterBase <- R6Class("DataAdapterBase",
                           
                           public = list(
                             initialize = function() {},
                             
                             get_current_price = function(asset_id, currency = DEFAULT.CURRENCY) {
                               print("Implement in subclass!")
                               exit(0)
                             }
                           ),
                           
                           private = list()
                           
                           )