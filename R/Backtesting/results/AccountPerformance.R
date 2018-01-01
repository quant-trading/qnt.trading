# Mikhail Andreev (c) 2017
# Account Performance

library(PerformanceAnalytics)

AccountPerformance <- R6Class("AccountPerformance",
                              
                              public = list(
                                
                                accountID = NULL,
                                
                                performance = data.frame(dt = c(), gross.value = c(), net.value = c(),
                                                         gross.period.r = c(), net.period.r = c(),
                                                         free.cash = c()),
                                
                                initialize = function(ID) {
                                  self$accountID = ID
                                },
                                
                                addState = function(state) {
                                  
                                  rec <- data.frame(
                                    dt = state$date, 
                                    gross.value = as.numeric(state$total.mv.T0), 
                                    net.value = as.numeric(state$total.mv.T0 - state$cumulative.tax.liability),
                                    gross.period.r = as.numeric(0), 
                                    net.period.r = as.numeric(0),
                                    free.cash = state$cash.available
                                  )
                                
                                  self$performance <- rbind(self$performance, rec)
                                  
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
                                }
                                
                              )
)