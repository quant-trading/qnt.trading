
Universe <- R6Class("Universe",
                    
                    private = list(
                      assets = NULL,
                      cash = NULL
                    ),
                    public = list(
                      
                      initialize = function(){
                        private$cash = c("RUB")
                        private$assets = c("GAZP.ME")
                      },
                      
                      getAssets = function() {private$assets}
                    )
)