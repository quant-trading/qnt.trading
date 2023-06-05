#rm(list = ls())

library(jsonlite)
library(glue)

library(R6)




# create a function to retrieve daily data
retreive_daily_data <- function(pair) {
  url = glue("https://api.pro.coinbase.com/products/{pair}/candles?granularity=86400")
  columnNames <- c('unix', 'low', 'high', 'open', 'close', glue('{pair} volume'))
  mydata <- fromJSON(url)
  df <- as.data.frame(mydata)
  colnames(df) <- columnNames  # rename the columns
  df
}



file.name <- "raw/Coinbase-0-CB-RAWTX_.csv"

raw.data <- read.csv(file = file.name, header = T, stringsAsFactors = F)
names(raw.data) <- c("tr.id", "tr.type", "date", "asset.open", "qty.open", "cost.basis", "source", 
                     "asset.close", "qty.close", "proceeds")
raw.data$date <- as.Date(substr(raw.data$date,1,10))

assets <- setdiff(unique(union(raw.data$asset.open, raw.data$asset.close)),"")
prices <- NULL

# --------------------- Get Prices
for(asset in assets) {
  print(paste("Getting Coibase price for", asset, "..."))
  if(asset == "ETH2") {
    newPair <- paste0("ETH-USD")
  } else {
    newPair <- paste0(asset,"-USD")
    
  }
  p <- NULL
  tryCatch({
    p <- retreive_daily_data(newPair)
    p$unix <- as.Date(as.POSIXct(p$unix, origin="1970-01-01"))},
    error = function(e) { 
      if(asset == "USDC") p <<- data.frame(asset = asset, unix = Sys.Date(), close = 1.)
    }
  )
  if(NROW(p) > 0){
    if(!is.null(prices)) {
      print(p[1,])
      prices <- rbind(prices, data.frame(asset = asset, date = p$unix[1], price = p$close[1]))
    } else {
      prices <- data.frame(asset = asset, date = p$unix[1], price = p$close[1])
    }
  } else {
    print("ERROR!")
  }
}

# --------------------- Process Tax Lots

# TODO: handle tax lots
# TODO: handle convert as a new tax lot

source("Stack.R")
total.bal.usd <- 0

debit <- c("Buy", "Receive", "Reward", "Rewards", "Card rebate reward", "Airdrop", "Converted to","Advanced trade trade", "Incoming")
credit <- c("Send", "Sell", "Spend", "Converted from")
ignore <- c("Stake")

stacks <- list()

for(asset in assets) {
  t <- subset(raw.data, raw.data$asset.open == asset | raw.data$asset.close == asset)
  
  stacks[[asset]] <- HifoStack$new()
  
  balance <- 0
  
  if(NROW(t) > 0) 
    for(k in 1:NROW(t)) {
      if(t$tr.type[k] %in% debit ) {
        balance <- balance + t$qty.open[k]
        stacks[[asset]]$push(t[k,])
      }
      if(t$tr.type[k] %in% credit) {
        balance <- balance - t$qty.close[k]
        stacks[[asset]]$pop(t[k,])
      }
      
      if(t$tr.type[k] == "Convert"){
        if(t$asset.open[k] == asset) {
          balance <- balance + t$qty.open[k]
          stacks[[asset]]$push(t[k,])
        }
        
        if(t$asset.close[k] == asset){ 
          balance <- balance - t$qty.close[k]
          stacks[[asset]]$pop(t[k,])
        }
      }
      #print(paste(t$Timestamp[k], balance))
    }
  
  print(paste(asset, round(balance,digits = 6)))
  print(paste(asset, round(stacks[[asset]]$get_balance(), digits = 6)))
  total.bal.usd <- total.bal.usd + balance * prices$price[prices$asset == asset]
}

#print(paste("Portfolio Balance USD:", total.bal.usd))

loss.report <- NULL
pnl.report <-NULL
wash.report <- NULL

for(asset in assets) {

  
  tmp <- stacks[[asset]]$get_pnl_report(prices$price[prices$asset == asset])
  if(!is.null(tmp) && tmp$mv > 2)
    pnl.report <- rbind(pnl.report, cbind(asset, tmp))
    
  tmp <- stacks[[asset]]$get_wash_report()
  if(!is.null(tmp))
    wash.report <- rbind(wash.report, cbind(asset, tmp))
  
  tmp <- stacks[[asset]]$get_loss_report(prices$price[prices$asset == asset])
  if(!is.null(tmp) && tmp$total.losses < -10){
    loss.report <- rbind(loss.report, cbind(asset, tmp))
  }
}


print("**************************************************")
print(pnl.report[order(-pnl.report$pnl.pct),])
print("**************************************************")
print(loss.report[order(-loss.report$tax.efficiency ),])
print("**************************************************")
print(wash.report)