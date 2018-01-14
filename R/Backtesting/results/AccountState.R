# Mikhail Andreev (c) 2017
# EOD Account State

source("broker/Portfolio.R")

AccountState <- R6Class("Account State",
                        
                        public = list(
                          accountId = "",
                          date = NULL,
                          
                          limits = list(),
                          portfolio = list(),

                          cumulative.tax.liability = 0,

                          initialize = function(id, dt) {
                            self$accountId = id
                            self$date = dt
                          },
                          
                          saveToCsv = function() {
                            
                            data <- data.frame(
                              ID = self$accountId,
                              dt = format(self$date, DATE.PATTERN),
                              value = self$limits[[SLICE.T2]]$get_value(),
                              cash = self$limits[[SLICE.T2]]$get_cash(),
                              cum.tax.liabiity = self$cumulative.tax.liability

                            )

                            write.csv(file = paste0('logs/Account_',self$accountId, "_",self$date,".csv"), x = data,
                                      col.names = F)
                          }
                        )
)