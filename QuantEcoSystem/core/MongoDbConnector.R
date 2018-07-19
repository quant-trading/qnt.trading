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
                                drv <- dbDriver("PostgreSQL")
                                
                                # creates a connection to the postgres database
                                # note that "con" will be used later in each connection to the database
                                con <- dbConnect(drv, dbname = "postgres",
                                                 host = "localhost", port = 5432,
                                                 user = "postgres", password = pw)
                                rm(pw) # removes the password
                              },
                              
                              load_strategy = function(strategy) {
                                query <- paste0("SELECT * FROM public.strategy s WHERE s.strategy_id = '",strategy$id,"'")
                                curr_rec <- dbGetQuery(con, query)
                                print(curr_rec)
                              },
                              
                              load_portfolio = function(strategy_id) {
                                NULL
                              },
                              
                              save_portfolio = function(strategy_id, portfolio) {
                                
                              }
                            ),
                            
                            private = list(
                              con = NULL
                            )
)