# Initialize Adapters

source("adapters/QuoteAdapterBase.R")
source("adapters/DictionaryAdapterBase.R")

Global.Quote.Adapter = QuoteAdapterBase$new()
Global.Dictionary.Adapter = DictionaryAdapterBase$new()