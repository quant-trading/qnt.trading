rm(list = ls())

library(R6)

source("Config.R")
source("core/Portfolio.R")
source("core/MongoDbConnector.R")

print("Run Main")

db_con <- MongoDbConnector$new()

# initialize strategy class
strategy <- Strategy$new(STRATEGY.ID)


# load portfolio
portfolio <- db_con$load_portfolio(strategy$id)

# generate trading signals

# trade signals

# save portfolio
db_con$save_portfolio(strategy$id, portfolio)

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




