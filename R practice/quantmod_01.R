library("quantmod")
####################
#                  #
#    Exercise 1    #
#                  #
####################

# data
fb.p <- getSymbols("FB", env=NULL)

####################
#                  #
#    Exercise 2    #
#                  #
####################
Cl(to.monthly(fb.p["2015::2015-12-31"]))


####################
#                  #
#    Exercise 3    #
#                  #
####################
plot(weeklyReturn(fb.p, subset="2016::"), main="Weekly return of Facebook")

####################
#                  #
#    Exercise 4    #
#                  #
####################

candleChart(fb.p, subset="2016::2016-12-31", name="Facebook", theme="white")



####################
#                  #
#    Exercise 5    #
#                  #
####################

chartSeries(fb.p, subset="2016::2016-12-31", type="line", name="Facebook", theme="white")
addBBands()
addRSI()

####################
#                  #
#    Exercise 6    #
#                  #
####################

getFX("EUR/USD", from=Sys.Date()-1, env = NULL)


####################
#                  #
#    Exercise 7    #
#                  #
####################

fb.f <- getFin("FB", env=NULL)
viewFin(fb.f)

####################
#                  #
#    Exercise 8    #
#                  #
####################

fb.bs <- viewFin(fb.f, "BS","A")
fb.bs["Total Current Assets",c("2013-12-31", "2014-12-31", "2015-12-31")]/fb.bs["Total Current Liabilities",c("2013-12-31", "2014-12-31", "2015-12-31")]


####################
#                  #
#    Exercise 9    #
#                  #
####################

price <- Cl(fb.p[NROW(fb.p)])
fb.is <- viewFin(fb.f, "IS", "a")
EPS <- fb.is["Diluted Normalized EPS", "2015-12-31"]

(price/EPS)


####################
#                  #
#    Exercise 10   #
#                  #
####################

getROA <- function(symbol, year)
{
  symbol.f <- getFin(symbol, env=NULL)
  symbol.ni <- viewFin(symbol.f, "IS", "A")["Net Income", paste(year, sep="", "-12-31")]
  symbol.ta <- viewFin(symbol.f, "BS", "A")["Total Assets", paste(year, sep="", "-12-31")]
  
  symbol.ni/symbol.ta*100
}

getROA("FB", "2015")
