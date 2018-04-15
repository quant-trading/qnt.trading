trading = require "trading"
params = require "params"

_maximumExchangeFee = params.add "Maximum exchange fee %", .25
_maximumOrderAmount = params.add "Maximum order amount", 1
_orderTimeout = params.add "Order timeout", 30

MINIMUM_AMOUNT = .01

init: ->
    #This runs once when the bot is started

handle: ->
    #This runs once every tick or bar on the graph
    storage.botStartedAt ?= data.at
    primaryInstrument = data.instruments[0]
    assetsAvailable = @portfolios[primaryInstrument.market].positions[primaryInstrument.asset()].amount
    currencyAvailable = @portfolios[primaryInstrument.market].positions[primaryInstrument.curr()].amount
    debug "The current price: #{primaryInstrument.price}"

    maximumBuyAmount = (currencyAvailable/primaryInstrument.price) * (1 - (_maximumExchangeFee/100))
    maximumSellAmount = (assetsAvailable * (1 - (_maximumExchangeFee/100)))

    if (maximumBuyAmount >= MINIMUM_AMOUNT)
        trading.buy primaryInstrument, 'limit', Math.min(_maximumOrderAmount, maximumBuyAmount), primaryInstrument.price, _orderTimeout
    if (maximumSellAmount >= MINIMUM_AMOUNT)
        trading.sell primaryInstrument, 'limit', Math.min(_maximumOrderAmount, maximumSellAmount), primaryInstrument.price, _orderTimeout

onRestart: ->
    debug "Bot restarted at #{new Date(data.at)}"

onStop: ->
    debug "Bot started at #{new Date(storage.botStartedAt)}"
    debug "Bot stopped at #{new Date(data.at)}"