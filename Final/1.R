rate = read.csv("federalFundsRate.csv",stringsAsFactors=FALSE)

mean(rate$RaisedFedFunds)

table(rate$Chairman)

rate$Chairman = as.factor(rate$Chairman)
rate$DemocraticPres = as.factor(rate$DemocraticPres)
rate$RaisedFedFunds = as.factor(rate$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(rate$RaisedFedFunds, 0.7)
train = subset(rate, spl == TRUE)
test = subset(rate, spl == FALSE)

model1 = glm(RaisedFedFunds~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, family = "binomial")
summary(model1)

predictTest = predict(model1, newdata = test, type="response")
table(train$RaisedFedFunds)
table(predictTest>=0.5)

library(ROCR)
pred = prediction(predictTest, test$RaisedFedFunds)
as.numeric(performance(pred, "auc")@y.values)


ROCRpred = prediction(predictTest, qualityTrain$PoorCare)
ROCRperf = performance(pred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

set.seed(201)
library(caret)
numFolds <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.cp = seq(0.001,0.05, 0.001))
train(RaisedFedFunds~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, method="rpart", trControl=numFolds, tuneGrid=grid)

model2 = rpart(RaisedFedFunds~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = train, method="class", cp = 0.016)
library(rpart)
library(rpart.plot)
prp(model2)

PredictCART = predict(model2, newdata = test, type = "class")
table(test$RaisedFedFunds, PredictCART)
