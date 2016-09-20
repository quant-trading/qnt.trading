#install.packages("quantmod")
library(quantmod)
#Gives us access to the technical indicators we need
#install.packages("e1071")
library(e1071)
#Includes the Naive Bayes classifier
#install.packages("ggplot2")
library(ggplot2)
#The charting functions we will use

Data<-read.csv("USDCAD.csv")
#Our dataset
CCI20<-CCI(Data[,3:5],n=20)
#A 20-period Commodity Channel Index calculated of the High/Low/Close of our data

RSI3<-RSI(Cl(Data),n=3)
#A 3-period RSI calculated off the close
DEMA10<-DEMA(Cl(Data),n = 10, v = 1, wilder = FALSE)
DEMA10c<-Cl(Data) - DEMA10
#A 10-period Double Exponential Moving Average (DEMA), with standard parameters. And we will be looking at the difference between the price and the DEMA.
DEMA10c<-DEMA10c/.0001
#Convert into pips

Indicators<-data.frame(RSI3,DEMA10c,CCI20)
Indicators<-round(Indicators,2)
Indicators<-Indicators[-nrow(Data),]
#Removing the most recent data point for calculating the indicators, and lining it up with the direction of that bar effectively shifts our indicator calculations back by one and prevents us from accessing information we couldn’t know.

Price<-Cl(Data)-Op(Data)
Class<-ifelse(Price > 0 ,"UP","DOWN")
Class<-Class[-1]
#Remove the oldest data point to match up our predicted Class with the indicators.
DataSet<-data.frame(Indicators,Class)
DataSet<-DataSet[-c(1:19),]
#Remove the instances where the indicators are still being calculated.

Training<-DataSet[1:2760,];Test<-DataSet[2761:3680,];Validation<-DataSet[3681:4600,]
#Separate into a training set (60% of the data), test set (20% of the data), and validation set (20% of the data).

NB<-naiveBayes(Class ~ RSI3 + CCI20 + DEMA10c, data=Training)
#Using our three technical indicators to predict the class off the training set

table(predict(NB,Test,type="class"),Test[,4],dnn=list('predicted','actual'))

TrainingPredictions<-predict(NB,Training,type="class")
#Get a list of all our predictions
TrainingCorrect<-ifelse(TrainingPredictions==Training[,4],"Correct","Incorrect")
#See if these predictions are correct
TrainingData<-data.frame(Training,TrainingPredictions,TrainingCorrect)
#Build one data set we can use for all of our plots


TestPredictions<-predict(NB,Test,type="class")
#Get a list of all our predictions
TestCorrect<-ifelse(TestPredictions==Test[,4],"Correct","Incorrect")
#See if these predictions are correct
TestData<-data.frame(Test,TestPredictions,TestCorrect)
#Build one data set we can use for all of our plots

ggplot(TrainingData,aes(x=CCI20,fill=TrainingPredictions))+geom_histogram(binwidth=15,position="fill")+labs(title="Training Predictions: CCI", x = "20-Period CCI", y= "Up/Down Ratio",fill="Predictions")+scale_fill_manual(values=c("#FF6737","#00B204"))

ggplot(TestData,aes(x=RSI3,fill=TestCorrect))+geom_histogram(binwidth=5,position="fill")+labs(title="Test Accuracy: RSI", x = "3-Period RSI", y= "Correct/Incorrect Ratio",fill="Accuracy")+scale_fill_manual(values=c("#0066FF","#FF4747"))

Long<-which(Validation$RSI3 < 30 & Validation$CCI > -290 & Validation$CCI < -100 & Validation$DEMA10c > -40 & Validation$DEMA10c < -20)
#Isolating the trades
LongTrades<-Validation[Long,]
#Creating a dataset of those trades
LongAcc<-ifelse(LongTrades[,4]=="UP",1,0)
#Testing the accuracy
(sum(LongAcc)/length(LongAcc))*100
#And our long accuracy
Short<-which(Validation$DEMA10c > 10 & Validation$DEMA10c < 40 & Validation$CCI > 185 & Validation$CCI < 325 & Validation$RSI3 > 50)
ShortTrades<-Validation[Short,]
ShortAcc<-ifelse(ShortTrades[,4]=="DOWN",1,0)
(sum(ShortAcc)/length(ShortAcc))*100
#Our short accuracy

length(LongAcc)+length(ShortAcc)
#Total number of trades
((sum(ShortAcc)+sum(LongAcc))/(length(LongAcc)+length(ShortAcc)))*100
#Total accuracy

