require(quantmod)
require(forecast)
require(PerformanceAnalytics)
require(fpp)


# settings
tickers = read.csv("export.csv", header=T, sep = ";")

stocks_num = length(tickers$Ticker)

name_ = rep("",stocks_num)
win_ = rep(NA, stocks_num)

results = data.frame(name = name_, win = win_, stringsAsFactors = F)

k = 0

for(stock_name in tickers$Ticker)
{  
  k = k+1

  # loading stock data
  tryCatch({
    stk <- getSymbols(paste(stock_name,".ME", sep =""), env=NULL)
  
    # split data into test and train settings
    N = length(Ad(stk))
  
    sig = as.data.frame(cbind(time(stk), rep(0,N)))
    names(sig) = c("Data","Signal")
    sig$Data = time(stk)
  
    start_idx = N*0.5
  
    for(i in start_idx:N) {
      train = Ad(stk)[1:i]
      m.fit = ets(Ad(train))
      m.pred = forecast(m.fit, h = 1)
    
      if(Ad(stk)[[i]] < m.pred$mean[1]) {
         signal = 0
      }
      else {
        signal = 1
      }
      #signal = ifelse(Ad(stk)[i] > m.pred$mean[1], 1, 0)
      #cat(signal)# , Ad(stk)[i],m.pred$mean[1])
      sig$Signal[which(sig$Data == time(Ad(stk)[i]))] = signal
    }
  
  
    # results
    #tr.signals = xts(sig)
  
    # Step 3: Construct your trading rule
    tr.sig <- Lag(ifelse(sig$Signal == 1, 1, 0))
  
    # Step 4: The trading rules/equity curve
    #ret <- ROC(Cl(GSPC))*sig
    ret <- Delt(Ad(stk),Ad(stk),k=1) * tr.sig
    ret <- ret['2011-01-01/2016-09-17']
    eq <- exp(cumsum(ret))
    #plot(eq)
  
    # Step 5: Evaluate strategy performance
    #table.Drawdowns(ret, top=10)
    #table.DownsideRisk(ret)
    #charts.PerformanceSummary(ret)
  
    names(ret) = "return"
    outcomes = subset(ret, ret$return != 0)
    
  
    real.r = Delt(Ad(stk),Ad(stk),k=1)[start_idx:N]
    profit = mean(outcomes)*100
    alpha = mean(outcomes)*100 - mean(real.r[!is.na(real.r)])*100
    win = length(subset(outcomes, outcomes$return > 0)) / length(outcomes)*100
    
    results$name[k] = stock_name
    results$win[k] = win 
    },
    error = function(e)
    {
      print(e)
      results$name[k] = stock_name
      results$win[k] = -1 
    }
  )
}

