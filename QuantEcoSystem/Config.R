source("Constants.R")
source("crypto/DataAdapterCrypto.R")

STRATEGY.ID <- "CryptoTest01"

source(paste0("strategies/", STRATEGY.ID, ".R"))

DATA.ADAPTER.CRYPTO <- DataAdapterCrypto$new()

DEFAULT.CURRENCY <- "USD"
SECONDARY.CURRENCY <- "BTC"
