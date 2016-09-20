# FPP practice
library(fpp)

# Chapter 01

## Section 1.4
# Example 1.1 Car emissions
subset(fuel, Litres<2)[, c(1,3,5,6,8)]

# Example Beer production
beer2 <- window(ausbeer, start = 1992)
plot(forecast(beer2), xlab = "Year", ylab = "megaliters", main = "")

# Practice
class(dowjones)
plot(dowjones)
?rwf
plot(rwf(dowjones, drift = TRUE))
fc = rwf(dowjones, drift = TRUE)
Y = c(fc$x[1],fc$x[NROW(fc$x)])
X = c(1, NROW(fc$x))
lines(x=X, y=Y)
plot(snaive(dowjones,h = 10))


# Chapter 02

library(fpp)

## Section 2.1 

plot(melsyd[,"Economy.Class"],
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")

plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")

seasonplot(a10,ylab="$ million", xlab="Year",
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

plot(jitter(fuel[,5]), jitter(fuel[,8]), xlab="City mpg", ylab="Carbon footprint")

pairs(fuel[,-c(1:2,4,7)], pch=19)

## Section 2.2

fuel2 <- fuel[fuel$Litres<2,]
summary(fuel2[,"Carbon"])
sd(fuel2[,"Carbon"])

cor(fuel2[,"Carbon"], fuel2[,"City"])

beer2 <- window(ausbeer, start=1992, end=2006-.1)

lag.plot(beer2, lags=9, do.lines=FALSE)

Acf(beer2)

set.seed(30)
x <- ts(rnorm(50))
plot(x, main="White noise")

Acf(x)

## Section 2.3 

beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)

plot(beerfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean, col=2)
lines(beerfit3$mean, col=3)
legend("topright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

dj2 <- window(dj,end=250)
plot(dj2,main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="",xlab="Day",xlim=c(2,290))
lines(meanf(dj2,h=42)$mean,col=4)
lines(rwf(dj2,h=42)$mean,col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

## Section 2.4

plot(log(elec), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)

lambda <- BoxCox.lambda(elec) # = 0.27
plot(BoxCox(elec,lambda))

monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] <- 29

par(mfrow=c(2,1))
plot(milk, main="Monthly milk production per cow",
     ylab="Pounds",xlab="Years")
plot(milk/monthdays, main="Average milk production per cow per day", 
     ylab="Pounds", xlab="Years")

## Section 2.5

beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2,h=11)
beerfit2 <- rwf(beer2,h=11)
beerfit3 <- snaive(beer2,h=11)

par(mfrow=c(1,1))
plot(beerfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean, col=2)
lines(beerfit3$mean, col=3)
lines(ausbeer)
legend("topright", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

beer3 <- window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

dj2 <- window(dj, end=250)

plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", 
     ylab="", xlab="Day", xlim = c(2, 290))
lines(meanf(dj2, h = 42)$mean, col = 4)
lines(rwf(dj2, h = 42)$mean, col = 2)
lines(rwf(dj2, drift = TRUE, h = 42)$mean, col = 3)
legend("topleft", lty = 1, col = c(4, 2, 3), 
       legend = c("Mean method", "Naive method", "Drift method"))
lines(dj)

dj3 <- window(dj, start=251)
accuracy(meanf(dj2,h=42), dj3)
accuracy(rwf(dj2,h=42), dj3)
accuracy(rwf(dj2,drift=TRUE,h=42), dj3)

## Section 2.6

dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", 
     ylab="", xlab="Day")
res <- residuals(naive(dj2))
plot(res, main="Residuals from naive method", 
     ylab="", xlab="Day")
Acf(res, main="ACF of residuals")
hist(res, nclass="FD", main="Histogram of residuals")

Box.test(res, lag = 10, fitdf = 0)
Box.test(res, lag = 10, fitdf = 0, type = "Lj")
?Box.test

## Section 2.10
forecast(ausbeer)




require(fpp)


# Simple exponential smoothing

oildata <- window(oil, start = 1996, end = 2007)
plot(oildata, ylab = "Oil (millions of tonnes)", xlab = "Year")

fit1 <- ses(oildata, alpha = 0.2, initial = "simple", h = 3)
fit2 <- ses(oildata, alpha = 0.6, initial = "simple", h = 3)
fit3 <- ses(oildata, h = 3)

plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

# Table 7.2
tab7.2 <- cbind(0:15, c(NA, oildata, NA, NA, NA), c(fit1$fitted, fit1$mean[1], fit1$mean), 
                c(fit2$fitted, fit2$mean[1], fit2$mean), c(fit3$model$state, fit3$mean))
tab7.2 <- rbind(tab7.2, cbind(rep(NA,4),rep(NA,4),
                              c(accuracy(fit1)[,c("MAE","RMSE","MAPE")],sum(residuals(fit1)^2)),
                              c(accuracy(fit2)[,c("MAE","RMSE","MAPE")],sum(residuals(fit2)^2)),
                              c(accuracy(fit3)[,c("MAE","RMSE","MAPE")],sum(residuals(fit3)^2))))
colnames(tab7.2) <- c("Time","Observed","Level a????=0.2","Level a????=0.6","Level a????=0.89")
rownames(tab7.2) <- c(1995:2010,"MAE","RMSE","MAPE","SSE")
round(tab7.2,1)

## Section 7.2
# Example 7.2: Air passengers

air <- window(ausair, start = 1990, end = 2004)
fit1 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple", h = 5)
fit2 <- holt(air, alpha = 0.8, beta = 0.2, initial = "simple", exponential = TRUE, h = 5)
fit3 <- holt(air, alpha = 0.8, beta = 0.2, damped = TRUE, initial = "simple", h = 5)


# Table 7.3
tab7.3 <- cbind(0:20, c(NA, air, NA, NA, NA, NA, NA), c(fit1$model$state[, 1], NA, NA, 
                                                        NA, NA, NA), c(fit1$model$state[, 2], NA, NA, NA, NA, NA), c(NA, fit1$fitted, NA, 
                                                                                                                     NA, NA, NA, NA), c(fit2$model$state[, 1], NA, NA, NA, NA, NA), c(fit2$model$state[, 
                                                                                                                                                                                                       2], NA, NA, NA, NA, NA), c(NA, fit2$fitted, NA, NA, NA, NA, NA))
tab7.3[17:21, 1] <- 1:5
tab7.3[17:21, 5] <- fit1$mean
tab7.3[17:21, 8] <- fit2$mean
rownames(tab7.3) <- 1989:2009
colnames(tab7.3) <- c("t","y","a„“","b","A·","a„“","b","A·")
round(tab7.3,2)


plot(fit2, type = "o", ylab = "Air passengers in Australia (millions)", xlab = "Year", 
     fcol = "white", plot.conf = FALSE)
lines(fitted(fit1), col = "blue")
lines(fitted(fit2), col = "red")
lines(fitted(fit3), col = "green")
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")
legend("topleft", lty = 1, col = c("black", "blue", "red", "green"), 
       legend=c("Data", "Holt's linear trend", "Exponential trend", "Additive damped trend"))


## Section 7.3
# Example 7.3. Sheep in Asia
livestock2 <- window(livestock, start = 1970, end = 2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2, exponential = TRUE)
fit4 <- holt(livestock2, damped = TRUE)
fit5 <- holt(livestock2, exponential = TRUE, damped = TRUE)

# Results for first model:
fit1$model
accuracy(fit1) # training set
accuracy(fit1,livestock) # test set

tab74col <- function(x) {
  out <- rep(NA, 5)
  names(out) <- c("alpha", "beta", "phi", "l", "b")
  out[names(x$model$par)] <- x$model$par
  out <- c(out,RMSE=accuracy(x)[1,"RMSE"])
  out <- c(out,SSE=sum((x$x-fitted(x))^2))
  out <- c(out,accuracy(x,livestock)[2,c("MAE","RMSE","MAPE","MASE")])
  return(out)
}
tab7.4 <- cbind(tab74col(fit1), tab74col(fit2), tab74col(fit3), tab74col(fit4), tab74col(fit5))
round(tab7.4,2)

states <- cbind(fit2$model$state, fit4$model$state)
colnames(states) <- c("level","slope","level","slope")
plot(states, main="")

plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
     flwd=1, plot.conf=FALSE)
lines(window(livestock,start=2001),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential","Additive Damped","Multiplicative Damped"))


## Section 7.5
# Example 7.4. International tourist visitor nights in Australia
aust <- window(austourists, start=2005)
plot(aust)

fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

plot(fit2,ylab="International visitor night in Australia (millions)",
     plot.conf=FALSE,type="o",fcol="white",xlab="Year")
lines(fitted(fit1),col="red",lty=2)
lines(fitted(fit2),col="green",lty=2)
lines(fit1$mean,type="o",col="red")
lines(fit2$mean,type="o",col="green")
legend("topleft",lty=1,pch=1,col=c(1,2,3),c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

states <- cbind(fit1$model$states[,1:3],fit2$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states,main="Additive  Seasonality                            Multiplicative  Seasonality",
     xlab="Year")

# Tables 7.5 and 7.6
fit1$model$state[,1:3]
fitted(fit1)
fit1$mean

fit2$model$state[,1:3]
fitted(fit2)
fit2$mean


## Section 7.7
# Example 7.1 revisited
oildata <- window(oil, start = 1996, end = 2007)
fit <- ets(oildata, model = "ANN")
plot(forecast(fit, h=3), ylab="Oil (millions of tones)")
summary(fit)
ls(fit)  #list names of the objects in the specified environment
fit$par

# Example 7.4 revisited
vndata <- window(austourists, start = 2005)
fit <- ets(vndata)
summary(fit)

plot(fit)

plot(forecast(fit, h = 8), ylab = "International visitor night in Australia (millions)")



