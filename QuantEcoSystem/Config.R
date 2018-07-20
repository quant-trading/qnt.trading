source("Constants.R")
source("connectivity/DataAdapterCrypto.R")
source("connectivity/DataAdapterYahoo.R")


STRATEGY.ID <- "CryptoTest01"

source(paste0("strategies/", STRATEGY.ID, ".R"))

DATA.ADAPTER.CRYPTO <- DataAdapterCrypto$new()
DATA.ADAPTER.YAHOO  <- DataAdapterYahoo$new()

DEFAULT.CURRENCY <- "USD"
SECONDARY.CURRENCY <- "BTC"

USD.RUB <- "RUBUSD=X"
USD.BTC <- "BTC-USD"