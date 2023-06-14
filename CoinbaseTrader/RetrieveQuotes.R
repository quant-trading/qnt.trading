library(jsonlite)
library(glue)

# create a function to retrieve daily data
retreive_daily_data <- function(pair) {
  url = glue("https://api.pro.coinbase.com/products/{pair}/candles?granularity=86400")
  columnNames <- c('unix', 'low', 'high', 'open', 'close', glue('{pair} volume'))
  mydata <- fromJSON(url)
  df <- as.data.frame(mydata)
  colnames(df) <- columnNames  # rename the columns
  df
}