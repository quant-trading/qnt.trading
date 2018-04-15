startingParameters = require "params"
_name = startingParameters.add "What is your name?", "CryptoTrader Developer" #This line will ask what their name is when starting the bot and it will default it to CryptoTrader Developer

init: ->
    storage.name = _name
handle: ->
    #This code will run once per tick interval or bar on the graph
    debug "Hello #{storage.name}!" #This will print in black
    info "Hello #{storage.name}! - in green" #This will print in green
    warn "Hello #{storage.name}! - in red" #This will print in red