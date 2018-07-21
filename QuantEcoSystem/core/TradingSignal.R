SIGNAL.BUY = 1
SIGNAL.SELL = -1

TradingSignal <- R6Class("TradingSignal",
                         
                         public = list(

                           method = NULL,
                           predicted_change = NULL,
                           predicted_price = NULL,
                           current_price = NULL,
                           horizon = NULL,
                           
                           initialize = function(asset, q, dir, scr = 0) {
                             private$s_asset_id = asset
                             private$s_qty = q
                             private$s_direction = dir
                             private$s_score = scr
                           }
                         ),
                         
                         active = list(
                           asset = function() private$s_asset_id,
                           qty = function() private$s_qty,
                           dir = function() private$s_direction,
                           score = function() private$s_score
                         ),
                         
                         private = list(
                           s_asset_id = NULL,
                           s_qty = NULL,
                           s_direction = NULL,
                           s_score = NULL
                         ) 
                         )