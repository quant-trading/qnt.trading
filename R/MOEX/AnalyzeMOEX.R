source("QuotesLoader.R")

DAYS_COUNT = 252

RELOAD_DATA = T


if(RELOAD_DATA) {
  universe <- read.csv("rates.csv", sep = ";",  stringsAsFactors = F)
  
  universe$SECID <- paste0(universe$SECID, ".ME")
  
  universe <- universe %>% as_tibble()
  
  universe <- universe %>% filter(SECID != "")

  for
  
  
  f <- function(x) {
    print(x[[1]])
    tryCatch(
      {
        getSymbols(Symbols = x[[1]], from = "2017-03-10")
        x[[1]]
      }, 
      error = function(err){
        print(paste("MY_ERROR:  ",err))
        return("")}
    )
    
  }
  
  universe$isExist <- apply(universe, 1, f)
  
  universe <- universe %>% filter(isExist != "")
  
  
  universe <- universe %>%
    mutate(
      stock.prices = map(SECID, 
                         function(.x) get_stock_prices(.x, 
                                                       return_format = "tibble",
                                                       from = "2016-01-01",
                                                       to = "2017-03-10"
                                                       
      )),
      log.returns  = map(stock.prices, 
                         function(.x) get_log_returns(.x, return_format = "tibble")),
      mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
      sd.log.returns   = map_dbl(log.returns, ~ sd(.$Log.Returns)),
      n.trade.days = map_dbl(stock.prices, nrow)
    )  
}


#-------------------------------------------------------------------------------------------------
# Scatter Plot
attach(universe)

plot(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), main="Risk/Return Chart", 
     xlab="Risk, StDv", ylab="Return,%", 
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2, xlim = c(-0.5,1.3), ylim = c(-0.5, 1.3))

text(y = mean.log.returns * DAYS_COUNT, x =  sd.log.returns * sqrt(DAYS_COUNT), labels=SECID, cex= 0.7,  adj = c(0.2,-1.0))
#-------------------------------------------------------------------------------------

mydata <- cbind(universe$mean.log.returns * DAYS_COUNT, universe$sd.log.returns * sqrt(DAYS_COUNT))

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 8) # 5 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(universe$SECID, universe$NAME, mydata, fit$cluster)

plot(aggregate(mydata$X1, list(mydata$fit.cluster), mean))

print(mydata %>% filter(fit.cluster == 5))
