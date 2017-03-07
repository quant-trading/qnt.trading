
source("QuotesLoader.R")

DAYS_COUNT = 252


universe <- read.csv("etf_list.csv", sep = ";",  stringsAsFactors = F)

universe <- universe %>% as_tibble()

universe <- universe %>% filter(Ticker.Symbol != "")

universe <- universe %>%
  mutate(
    stock.prices = map(Ticker.Symbol, 
                       function(.x) get_stock_prices(.x, 
                                                     return_format = "tibble",
                                                     from = "2016-01-01",
                                                     to = "2017-03-06")
    ),
    log.returns  = map(stock.prices, 
                       function(.x) get_log_returns(.x, return_format = "tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )  


# Scatter Plot
attach(universe)
plot(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), main="Risk/Return Chart", 
     xlab="Risk, StDv", ylab="Return,%", 
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

text(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), labels=Ticker.Symbol, cex= 0.7,  adj = c(0.2,-1.0))


# Correlation

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

# Optimize Portfolio
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

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
meanETL <- add.objective(portfolio=meanETL, type="risk", name="var", risk_aversion=0.25)




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
