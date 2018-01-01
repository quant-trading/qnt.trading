# Mikhail Andreev (c) 2017
# EOD Account State

AccountState <- R6Class("Account State",
                        
                        public = list(
                          accountId = "",
                          date = NULL,
                          
                          cash.blocked = 0,
                          cash.available = 0,
                          cash.marginal = 0,
                          cash.total = 0,
                          total.mv.T0 = 0,
                          cumulative.tax.liability = 0,
                          
                          initialize = function(id, dt) {
                            self$accountId = id
                            self$date = dt
                          },
                          
                          saveToCsv = function() {
                            
                            data <- data.frame(
                              ID = self$accountId,
                              dt = format(self$date, DATE.PATTERN),
                              total.mv.T0 = self$total.mv.T0,
                              cash.blocked = self$cash.blocked,
                              cash.available = self$cash.available,
                              cash.marginal = self$cash.marginal,
                              cash.total = self$cash.total,
                              cum.tax.liabiity = self$cumulative.tax.liability

                            )

                            write.csv(file = paste0('logs/Account_',self$accountId, "_",self$date,".csv"), x = data,
                                      col.names = F)
                          }
                        )
)