FluTrain <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 2/FluTrain.csv")

FluTrain$Week[which.max(FluTrain$ILI)]  
FluTrain$Week[which.max(FluTrain$Queries)]    

hist(FluTrain$ILI)

plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

FluTest <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 2/FluTest.csv")

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
Est_ILI = PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
Est_ILI

Obs_ILI = FluTest$ILI[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
(Obs_ILI-Est_ILI)/Obs_ILI

SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
sapply(ILILag2 , function(x) sum(is.na(x)))

plot(log(ILILag2),log(FluTrain$ILI))

FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
sapply(ILILag2 , function(x) sum(is.na(x)))

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE2 = sum((PredTest2-FluTest$ILI)^2)
RMSE2 = sqrt(SSE2 / nrow(FluTest))
RMSE2

RMSE
