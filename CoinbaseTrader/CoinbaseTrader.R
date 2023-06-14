# https://help.coinbase.com/en/prime/trading-and-funding/supported-cryptocurrencies-and-trading-pairs

rm(list = ls())
source("RetrieveQuotes.R")
library(quantmod)


pairs <- read.csv(file = "Coinbase Supported Assets - May 19, 2023.csv", header = T, stringsAsFactors = F)
pairs <- subset(pairs, pairs$Exchange.Supported == "Yes" & pairs$Quote.Currency == "USD")
pairs <- as.list(unique(paste0(pairs$Base.Currency, "-", pairs$Quote.Currency)))

prices <- list()
i <- 0
for(ticker in pairs) {
  i <- i+1
  print(ticker)
  tryCatch({
    p <- retreive_daily_data(ticker)
    p$unix <- as.Date(as.POSIXct(p$unix, origin="1970-01-01"))
    names(p) <- c("unix", "low", "high", "open", "close", "volume")
    prices[[ticker]] <- p
    #if(i > 20) break
  },
  
  error = function(e) { 
    print(e)
    #if(asset == "USDC") p <<- data.frame(asset = asset, unix = Sys.Date(), close = 1.)
  }
  )
}


price_to_xts <- function(p) {
  quotes <- data.frame(Open = p$open,
                       High = p$high,
                       Low = p$low,
                       Close = p$close,
                       Volume = p$volume
  )
  
  rownames(quotes) <- p$unix
  
  as.xts(quotes)
}

advanced <- 0
declined <- 0

for(id in names(prices)) {
  if(length(prices[[id]]$close) == 0) next()
  #print(id)
  xts.p <- price_to_xts(prices[[id]])
  
  # Donchian signal
  donchian <- DonchianChannel(xts.p[,c("High","Low")], n=5)
  
  if(xts.p[NROW(xts.p)]$Close > donchian[NROW(donchian)]$high) {
    breakout <- 100*(xts.p[NROW(xts.p)]$Close/donchian[NROW(donchian)]$high - 1)
    print(paste("Broken Donchian Up:", id, "by", round(breakout,2),"%"))
  }
  
  # Momentum
  roc <- momentum(xts.p, n = 1)
  if(roc[NROW(roc)]$Close > 0 ) {
    advanced <- advanced + 1
  } else {
    declined <- declined + 1
  }
  
  
}

print(paste("Advance-Decline Ratio:", round(advanced/declined,2)))




# chartSeries(xts.p, name= id)
# plot(addTA(donchian$high, on=1, col='red', legend.name = "UpperD"))
# plot(addTA(donchian$low, on=1, col='red', legend.name = "LowerD"))

