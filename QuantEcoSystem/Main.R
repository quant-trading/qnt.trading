rm(list = ls())

library(R6)

source("Config.R")
source("core/Portfolio.R")
source("connectivity/MongoDbConnector.R")

print("Run Main")
tryCatch({
db_con <- MongoDbConnector$new()

# initialize strategy class
strategy_profile <- db_con$load_strategy_profile(STRATEGY.ID)
strategy <- Strategy$new(strategy_profile)

# load portfolio
portfolio <- db_con$load_portfolio(strategy$port_id)

# generate trading signals
signals <- strategy$get_trading_signals()
#db_con$save_trading_signals(STRATEGY.ID, signals)

# trade signals
# calculate trading orders
orders <- strategy$get_trading_orders(portfolio, signals)
#db_con$save_trading_orders(STRATEGY.ID, orders)

# create exchange and trade orders
# exchange <- ExchangeCryptoTest$new()
# trades <- exchange$trade(orders)
# portfolio <- portfolio$get_new_state(trades)

# save portfolio
#db_con$save_portfolio(portfolio)

},
finally = {
  db_con$destroy()
}
)

portfolio$holdings[[1]]$q
portfolio$id



# send the report

#-----------------------------------------------------------------------------

# portfolio <- Portfolio$new()
# 
# h1 <- Holding$new("ETH", 1.0, TYPE.CRYPTO)
# h2 <- Holding$new("BTC", 1.0, TYPE.CRYPTO)
# 
# portfolio$add_holding(h1)
# portfolio$add_holding(h2)
# 
# portfolio$update()
# 
# portfolio$market_value







