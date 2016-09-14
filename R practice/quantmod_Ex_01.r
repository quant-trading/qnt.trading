 # QuantMod examples

library(quantmod)

getSymbols("AAPL",src="google") # from google finance  
getSymbols("GOOG",src="yahoo") # from yahoo finance 
getSymbols("DEXJPUS",src="FRED") # FX rates from FRED 
#getSymbols("XPT/USD",src="Oanda") # Platinum from Oanda 


# Specify lookup parameters, and save for future sessions. 
setSymbolLookup(AAPL='google',GOOG='yahoo') 
setSymbolLookup(DEXJPUS='FRED') 
setSymbolLookup(XPTUSD=list(name="XPT/USD",src="oanda")) 
saveSymbolLookup(file="mysymbols.rda") 

# new sessions call loadSymbolLookup(file="mysymbols.rda") 
getSymbols(c("AAPL","GOOG","DEXJPUS","XPTUSD")) 

# Add multi-coloring and change background to white 
candleChart(GOOG,multi.col=TRUE,theme="white") 

getSymbols("XPT/USD",src="oanda") 
chartSeries(XPTUSD,name="Platinum (.oz) in $USD") 
chartSeries(to.weekly(XPTUSD),up.col='white',dn.col='blue') 


require(TTR) 
getSymbols("GOOG") 
chartSeries(GOOG) 
addMACD() 
addBBands()



# Create a quantmod object for use in 
# in later model fitting. Note there is 
# no need to load the data before hand. 
   
setSymbolLookup(SPY='yahoo', VXN=list(name='^VIX',src='yahoo')) 

mm <- specifyModel(Next(OpCl(SPY)) ~ OpCl(SPY) + Cl(VIX)) 
modelData(mm)


getSymbols("GS") #Goldman OHLC from yahoo 
is.OHLC(GS) # does the data contain at least OHL and C? 
has.Vo(GS) # how about volume? 
Op(GS) # just the Open column please. 
seriesHi(GS) # where and what was the high point


OpCl(GS) #daily percent change open to close 
OpOp(GS) #one period open to open change 
HiCl(GS) #the percent change from high to close


Lag(Cl(GS)) #One period lag of the close 
Lag(Cl(GS),c(1,3,5)) #One, three, and five period lags 
Next(OpCl(GS)) #The next periods open to close - today! 
 
# Open to close one-day, two-day and three-day lags 
Delt(Op(GS),Cl(GS),k=1:3)

GS['2007'] #returns all Goldman's 2007 OHLC 
GS['2008'] #now just 2008 
GS['2008-01'] #now just January of 2008 
GS['2007-06::2008-01-12'] #Jun of 07 through Jan 12 of 08 
 
GS['::'] # everything in GS 
GS['2008::'] # everything in GS, from 2008 onward 
non.contiguous <- c('2007-01','2007-02','2007-12') 
GS[non.contiguous]



last(GS) #returns the last obs. 
last(GS,8) #returns the last 8 obs. 
 
# let's try something a bit cooler. 
last(GS, '3 weeks') 
last(GS, '-3 weeks') # all except the last 3 weeks 
last(GS, '3 months') 
last(first(GS, '2 weeks'), '3 days')


periodicity(GS) 
unclass(periodicity(GS)) 
to.weekly(GS) 
to.monthly(GS) 
periodicity(to.monthly(GS)) 
ndays(GS); nweeks(GS); nyears(GS) 

# Let's try some non-OHLC to start 
getFX("USD/EUR") 
periodicity(USDEUR) 
to.weekly(USDEUR) 
periodicity(to.weekly(USDEUR))



endpoints(GS,on="months") 
 
# find the maximum closing price each week 
apply.weekly(GS,FUN=function(x) { max(Cl(x)) } ) 
 
# the same thing - only more general 
period.apply(GS,endpoints(GS,on='weeks'), FUN=function(x) { max(Cl(x)) } ) 

# same thing - only 50x faster! 
as.numeric(period.max(Cl(GS),endpoints(GS,on='weeks')))


# Quick returns - quantmod style 
 
getSymbols("SBUX") 
dailyReturn(SBUX) # returns by day 
weeklyReturn(SBUX) # returns by week 
monthlyReturn(SBUX) # returns by month, indexed by yearmon 

# daily,weekly,monthly,quarterly, and yearly 
allReturns(SBUX) # note the plural


getSymbols("GS") #Goldman OHLC from yahoo 
chartSeries(GS) 


barChart(GS,theme='white.mono',bar.type='hlc') 
candleChart(GS,multi.col=TRUE,theme='white') 
lineChart(GS,line.type='h',TA=NULL) 

chartSeries(GS)
candleChart(GS,subset='2007-12::2008') 
candleChart(GS,theme='white', type='candles') 
reChart(major.ticks='months',subset='first 16 weeks') 


chartSeries(GS, TA=NULL) 
chartSeries(GS, theme="white",TA="addVo();addBBands();addCCI()") 


getSymbols("GS") #Yahoo! OHLC from yahoo 
# addTA allows you to add basic indicators 
# to your charts - even if they aren't part 
# of quantmod. 
chartSeries(GS, TA=NULL) 

#Then add the Open to Close price change 
#using the quantmod OpCl function 
addTA(OpCl(GS),col='blue', type='h') 
addOpCl <- newTA(OpCl,col='green',type='h') 
addOpCl() 
