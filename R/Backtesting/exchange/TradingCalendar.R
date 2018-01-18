# Mikhail Andreev (c) 2017
# Trading Calendar

TRADING.CALENDAR.FILE = "data/calendar/MICEXINDEXCF.ME.csv"


TradingCalendar <- R6Class("TradingCalendar",
                           
                           private = list(
                             tradingDates = NULL
                           ),
                           
                           public = list(
                             initialize = function() {
                               calendar <- read.csv(file = TRADING.CALENDAR.FILE, header = T, stringsAsFactors = F, sep = ';')
                               private$tradingDates <- as.Date(as.character(calendar[,1]), format = "%Y%m%d")
                             },
                             
                             getNextTradingDate = function(dt) {
                               private$tradingDates[min(which(private$tradingDates > dt))]
                             }
                           )
)