require("RPostgreSQL")

MongoDbConnector <- R6Class("MongoDBConnector.R",
                            
                            public = list(
                              initialize = function() {
                                # create a connection
                                # save the password that we can "hide" it as best as we can by collapsing it
                                pw <- {
                                  "kalinovmost19842006"
                                }
                                
                                # loads the PostgreSQL driver
                                private$drv <- dbDriver("PostgreSQL")
                                
                                # creates a connection to the postgres database
                                # note that "con" will be used later in each connection to the database
                                private$con <- dbConnect(private$drv, 
                                                         dbname = "postgres",
                                                         host = "localhost", port = 5432,
                                                         user = "postgres", password = pw)
                                rm(pw) # removes the password
                              },
                              
                              load_strategy_profile = function(strategy_id) {
                                query <- paste0("SELECT * FROM public.strategy s WHERE s.strategy_id = '",strategy_id,"'")
                                curr_rec <- dbGetQuery(private$con, query)
                                curr_rec
                              },
                              
                              load_portfolio = function(pid) {
                                portfolio <- Portfolio$new(pid)
                                
                                query <- paste0("select * from public.portfolio_holdings p where p.portfolio_id = '",pid,"' and p.date = 
                                  (select max(b.date) from public.portfolio_holdings b where b.portfolio_id = '",pid,"')")
                                recs <- dbGetQuery(private$con, query)
                                
                                for(i in NROW(recs)) {
                                  hld <- Holding$new(recs[i,]$asset_id, recs[i,]$qty, TYPE.CRYPTO)
                                  portfolio$add_holding(hld)
                                }
                                portfolio
                              },
                              
                              save_portfolio = function(portfolio) {
                                
                                ts <- Sys.time()
                                
                                # save holdings
                                private$save_portfolio_holdings(portfolio, ts)
                                
                                # save dynamics
                                private$save_portfolio_dynamics(portfolio, ts)
                              },
                              
                              destroy = function() {
                                # close the connection
                                dbDisconnect(private$con)
                                dbUnloadDriver(private$drv)
                              }
                            ),
                            
                            private = list(
                              con = NULL,
                              drv = NULL,
                              
                              save_portfolio_holdings = function(portolio, time) {
                                #
                                for(i in seq(1, NROW(portfolio$holdings))) {
                                  
                                  data <- data.frame(portfolio_id = portfolio$id,
                                                    date = time,
                                                    asset_id = portfolio$holdings[[i]]$a_id,
                                                    qty = portfolio$holdings[[i]]$q)
                                  
                                  dbWriteTable(private$con, "portfolio_holdings", value = data, append = TRUE, row.names = FALSE)
                                }
                                
                              },
                              
                              save_portfolio_dynamics = function(portfolio, time) {
                                
                                data <- data.frame(portfolio_id = portfolio$id,
                                                   date = time,
                                                   value_usd = portfolio$market_value,
                                                   value_rub = portfolio$market_value / DATA.ADAPTER.YAHOO$get_current_price(USD.RUB),
                                                   value_btc = portfolio$market_value / DATA.ADAPTER.YAHOO$get_current_price(USD.BTC)
                                                   )
                                
                                
                                
                                dbWriteTable(private$con, "portfolio_dynamics", value = data, append = TRUE, row.names = FALSE)
                              }
                            )
)