library(quantmod)

# load Magnit data
STK <- getSymbols("GAZP.ME", env=NULL)

SPY <- getSymbols("SPY", env=NULL)


#charting
chartSeries(STK,name="Magnet, RUB") 
addRSI(n=10)

RSI3<-RSI(Ad(STK),n=10)
RSI3 = ifelse(RSI3 > 80,1,ifelse(RSI3<20,-1,0))

CCI20<-CCI(Cl(STK),n=20)
DEMA10<-DEMA(Cl(STK),n = 10, v = 1, wilder = FALSE)

RSI3_SP<-RSI(Ad(SPY),n=10)
RSI3_SP = ifelse(RSI3_SP > 80,1,ifelse(RSI3_SP<20,-1,0))


#Delt(Cl(STK),Cl(STK),k=1:3)
dV = Vo(STK) - Lag(Vo(STK))

#target = OpCl(STK)
trg = Delt(Cl(STK),Cl(STK),k=1)

#quantile(OpCl(STK), probs = seq(0, 1, 0.25), na.rm = FALSE)


ts = buildData(trg ~ Lag(RSI3) + Lag(RSI3_SP) + Lag(dV)+Lag(DEMA10) + Lag(CCI20), na.rm=TRUE)

#model = specifyModel(target ~ Lag(RSI3) + Lag(RSI3_SP) + Lag(dV), na.rm=TRUE)
#m.built = buildModel(model, method = "randomForest", training.per = c('2011-01-01','2016-09-05'))
#z =tradeModel(m.built, plot.model = TRUE)
#getModelData(m.built)

# build a simple model 
library(randomForest)

sd = as.data.frame(ts)
names(sd) = c("trg","RSI","RSI_SPY","DV", "DEMA10", "CCI20")
attach(sd)

sd$trg = as.factor(ifelse(sd$trg>0,1,0))

train = sample(1:nrow(ts), nrow(ts)*0.75)
sd.test=  sd [-train ,]
#ts.train = ts[train,] 

tree.stk =randomForest(sd$trg~., sd , subset =train, mtry = 1, importance = TRUE)
#plot(tree.stk)
summary(tree.stk)
tree.stk$confusion

#?randomForest
# TEST
tree.pred = predict ( tree.stk , sd.test)
res =table ( tree.pred , sd.test$trg)
#??predict.randomForest
#UP acc:
res[2,2] / (res[2,2] + res[1,2])

#DOWN acc:
res[1,1] / (res[1,1] + res[2,1])

# ALL
tree.pred = predict ( tree.stk , sd)
res =table ( tree.pred , sd$trg)
#??predict.randomForest
#UP acc:
res[2,2] / (res[2,2] + res[1,2])

#DOWN acc:
res[1,1] / (res[1,1] + res[2,1])


#??randomForest
#result=rfcv(sd[,-1],sd$trg,  cv.fold=10)
#with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))

#result <- replicate(5, rfcv(sd[,-1],sd$target,  cv.fold=10), simplify=FALSE)
#error.cv <- sapply(result, "[[", "error.cv")
#matplot(result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type="l",
#        lwd=c(2, rep(1, ncol(error.cv))), col=1, lty=1, log="x",
#        xlab="Number of variables", ylab="CV Error")



# TS analysis
class(STK)
ts_stk = ts(Ad(STK))


library(forecast)
plot(naive(ts_stk, h = 20))



lag.plot(ts_stk, lags=9, do.lines=FALSE)

Acf(ts_stk)

m.fit =  naive(window(ts_stk, start = 1, end = 999), h = 20)
nd = window(ts_stk, start = 1000, end = 1600)
accuracy(m.fit, nd)

res <- residuals(m.fit)
plot(res)

Box.test(res, lag = 10, fitdf = 0)
Box.test(res, lag = 10, fitdf = 0, type = "Lj")

# ETS
m.fit2 = ets(ts_stk)
plot(forecast(m.fit2, h = 200))
