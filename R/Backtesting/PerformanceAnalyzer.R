PerformanceAnalyzer <- R6Class("Performance.Analyzer",
  public = list(
    
    initialize = function() {},
    analyze = function(ret, b.ret) {
      
      #eq <- exp(cumsum(ret))
      #plot(eq)

      # Step 5: Evaluate strategy performance
      table.Drawdowns(ret, top=10)
      table.DownsideRisk(ret)
      #charts.PerformanceSummary(ret)
      chart.RelativePerformance(ret, b.ret)
      
      chart_Series(cumsum(ret))
      add_TA(cumsum(b.ret), on = 1, col = "black")
    }
  )
)

