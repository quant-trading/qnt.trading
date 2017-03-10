
source("QuotesLoader.R")

DAYS_COUNT = 252
MAX_EXPENSE_RATIO_THRESHOLD = 0.2
CHEAP_ONLY = T
RELOAD_DATA = F
RISK_TOLERANCE = 8.5

if(RELOAD_DATA) {
universe <- read.csv("etf_list.csv", sep = ";",  stringsAsFactors = F)

universe <- universe %>% as_tibble()

universe <- universe %>% filter(Ticker.Symbol != "")

universe <- universe %>%
  mutate(
    stock.prices = map(Ticker.Symbol, 
                       function(.x) get_stock_prices(.x, 
                                                     return_format = "tibble",
                                                     from = "2013-01-01",
                                                     to = "2017-03-06")
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )  
}



# Correlation

etfs_cheap <- as.vector(t(universe %>% filter(ExpenseRatio < MAX_EXPENSE_RATIO_THRESHOLD) %>% select(Ticker.Symbol)))

universe_unnest <- universe %>%
  select(Ticker.Symbol, log.returns) %>%
  unnest()

universe_spread <- universe_unnest %>%
  spread(key = Ticker.Symbol, value = Log.Returns) %>%
  na.omit()

universe_cor <- universe_spread %>%
  select(-Date) %>%
  cor() 

library(corrplot)
universe_cor %>%
  corrplot(order   = "hclust", 
           addrect = 11)



#------------------------------------------------------------------
# Analyze actual Portfolio

etfs <- c("VBTLX", "VTIAX", "VEMAX", "VFIAX", "VGSLX")
w <- c(0.1, 0.1, 0.05, 0.6, 0.15)

act_p <- data.frame(t(w))
colnames(act_p) <- etfs

R <- universe_unnest %>%
  spread(key = Ticker.Symbol, value = Log.Returns)  %>%
  na.omit()

rownames(R) <- as.factor(universe_spread$Date)
R <- R %>%select(-Date) %>% as.xts()

actual_p <- as.xts(apply(R[,etfs], 1, function(x) crossprod(x , w)))
actual_p  %>% cumsum() %>% chartSeries()

#------------------------------------------------------------------
# Analyze optimal Portfolio
# Optimize Portfolio
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

#----- cheap etf only------------
if(CHEAP_ONLY = T) {
universe_unnest <- universe %>% 
  select(Ticker.Symbol, log.returns) %>% filter(Ticker.Symbol %in% etfs_cheap) %>%
  unnest()

universe_spread <- universe_unnest %>%
  spread(key = Ticker.Symbol, value = Log.Returns) %>%
  na.omit()
}
#-----------------------------------------

R <- universe_spread[-1] 
rownames(R) <- as.factor(universe_spread$Date)


R <- as.xts(R)


init <- portfolio.spec(assets=colnames(R))
print.default(init)
# Add the full investment constraint that specifies the weights must sum to 1.
init <- add.constraint(portfolio=init, type="leverage",
                       min_sum=0.99, max_sum=1.00)
init <- add.constraint(portfolio=init, type="box", min=0.00, max=0.35)


meanETL <- add.objective(portfolio=init, type="return", name="mean")
meanETL <- add.objective(portfolio=meanETL, type="risk", name="var", risk_aversion=RISK_TOLERANCE)


opt_maxret <- optimize.portfolio(R=R, portfolio=meanETL,
                                 optimize_method="ROI",
                                 trace=TRUE)

# opt_maxret <- optimize.portfolio(R=as.xts(R), portfolio=meanETL,
#                                        optimize_method="DEoptim",
#                                        search_size=2000,
#                                        trace=FALSE)



opt_maxret$weights[opt_maxret$weights > 0.001]
opt_maxret$objective_measures$mean * DAYS_COUNT
opt_maxret$objective_measures$StdDev * sqrt(DAYS_COUNT)

#universe$Investment.Name[which(universe$Ticker.Symbol  %in% names(opt_maxret$weights[opt_maxret$weights > 0.001]))]
etfs_o <- universe$Ticker.Symbol[which(universe$Ticker.Symbol  %in% names(opt_maxret$weights[opt_maxret$weights > 0.001]))]
w_o <- opt_maxret$weights[opt_maxret$weights > 0.001]

res = rbind(opt_maxret$weights[opt_maxret$weights > 0.001],
      universe$Investment.Name[which(universe$Ticker.Symbol  %in% names(opt_maxret$weights[opt_maxret$weights > 0.001]))]
)




opt_p <- as.xts(apply(R[,etfs_o], 1, function(x) crossprod(x , w_o)))
#opt_p   %>% cumsum() %>% chartSeries()
#actual_p   %>% cumsum() %>% addTA(on=1)

as.xts(merge(opt_p, actual_p)) %>% na.omit() %>% cumsum() %>% chartSeries()
actual_p   %>% cumsum() %>% addTA(on=1)

#-------------------------------------------------------------------------------------------------
# Scatter Plot
attach(universe)

plot(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), main="Risk/Return Chart", 
     xlab="Risk, StDv", ylab="Return,%", 
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2, xlim = c(-0.03,0.3), ylim = c(-0.03, 0.3))

text(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), labels=Ticker.Symbol, cex= 0.7,  adj = c(0.2,-1.0))
points(x = sd(actual_p) * sqrt(DAYS_COUNT), y = mean(actual_p) * DAYS_COUNT, pch = 19, col = "red", lty = "solid")
points(x = opt_maxret$objective_measures$StdDev * sqrt(DAYS_COUNT) , y = opt_maxret$objective_measures$mean * DAYS_COUNT , pch = 19, col = "green", lty = "solid")


print("Expense Ratio for Optimal Portfolio:")
crossprod(opt_maxret$weights[opt_maxret$weights > 0.001], universe$ExpenseRatio[which(universe$Ticker.Symbol  %in% etfs_o)])

print("Expense Ratio for Actual Portfolio:")
crossprod(w, universe$ExpenseRatio[which(universe$Ticker.Symbol  %in% etfs)])
