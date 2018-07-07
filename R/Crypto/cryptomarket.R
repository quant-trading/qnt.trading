library(coinmarketcapr)

plot_top_5_currencies(currency = "USD")

global_cap <- get_global_marketcap(currency = "USD")
coins_data <- get_marketcap_ticker_all(currency = "USD")

coins_data[which(coins_data$percent_change_7d == max(coins_data$percent_change_7d)),c("symbol", "percent_change_7d")]
coins_data[which(coins_data$percent_change_7d == min(coins_data$percent_change_7d)),c("symbol", "percent_change_7d")]