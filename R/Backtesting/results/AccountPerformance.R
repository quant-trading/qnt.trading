# Mikhail Andreev (c) 2017
# Account Performance

library(PerformanceAnalytics)

AccountPerformance <- R6Class("AccountPerformance",
                              
                              public = list(
                                
                                accountID = NULL,
                                
                                performance = NULL,
                                
                                initialize = function(ID) {
                                  self$accountID = ID
                                },
                                
                                addState = function(state) {
                                  
                                  rec <- data.frame(
                                    dt = state$date, 
                                    gross.value = as.numeric(state$limits[[SLICE.T2]]$get_value()), 
                                    net.value = as.numeric(state$limits[[SLICE.T2]]$get_value() - state$cumulative.tax.liability),
                                    gross.period.r = as.numeric(0), 
                                    net.period.r = as.numeric(0),
                                    cash = as.numeric(state$limits[[SLICE.T2]]$get_cash()),
                                    longs = as.numeric(state$limits[[SLICE.T2]]$get_long_mv()),
                                    shorts = as.numeric(state$limits[[SLICE.T2]]$get_short_mv()),
                                    UDS = as.numeric(state$limits[[SLICE.T2]]$get_UDS()),
                                    UDS = as.numeric(state$limits[[SLICE.T2]]$get_leverage())
                                  )
                                
                                  if(is.null(self$performance)) {
                                    self$performance <- rec
                                  } else {
                                    self$performance <- rbind(self$performance, rec)
                                  }
                                  
                                  
                                  # update returns
                                  N <- NROW(self$performance)
                                  if(N > 1) {
                                    self$performance$gross.period.r[N] <- as.numeric(self$performance$gross.value[N] / self$performance$gross.value[N-1] - 1)
                                    self$performance$net.period.r[N] <- as.numeric(self$performance$net.value[N] / self$performance$net.value[N-1] - 1)
                                  }
                                },
                                
                                plotDynamics = function() {
                                  #eq <- exp(cumsum(self$performance$net.period.r))
                                  #plot(eq)
                                  
                                  # Step 5: Evaluate strategy performance
                                  # table.Drawdowns(ret, top=10)
                                  # table.DownsideRisk(ret)
                                  
                                  net.r <- xts(x = self$performance$net.period.r, order.by = self$performance$dt)
                                  charts.PerformanceSummary(net.r)
                                  
                                                                
                                  # chart.RelativePerformance(ret, b.ret)
                                  # 
                                  # chart_Series(cumsum(ret))
                                  # add_TA(cumsum(b.ret), on = 1, col = "black")
                                  #free.cash <- xts(x = self$performance$free.cash, order.by = self$performance$dt)
                                  #plot(free.cash)
                                },
                                
                                save = function() {
                                  write.csv(x = self$performance, file = "logs/Results.csv")
                                },
                                
                                print_statistics = function() {
                                  net.r <- xts(x = self$performance$net.period.r, order.by = self$performance$dt)
                                  table.AnnualizedReturns(R = net.r,  Rf = RISK.FREE.RATE / 365)
                                  #table.Drawdowns(R = net.r)
                                  
                                }
                                
                              )
)