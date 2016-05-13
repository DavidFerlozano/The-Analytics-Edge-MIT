census <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 4/census.csv")
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

model1 = glm(over50k ~., data=train, family="binomial")
summary(model1)

PredictTest = predict(model1, newdata=test, type="response")
table(test$over50k, PredictTest >= 0.5)

table(test$over50k)

library(ROCR)
predLog = prediction(PredictTest, test$over50k)
as.numeric(performance(predLog, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~., data=train, method="class")
prp(CARTmodel)

PredictCART = predict(CARTmodel, newdata=test, type="class")
table(test$over50k, PredictCART)

PredictROC = predict(CARTmodel, newdata = test)
predCART = prediction(PredictROC[,2], test$over50k)
perfCART = performance(predCART, "tpr", "fpr")
perfLog = performance(predLog, "tpr", "fpr")
par(mfrow=c(1,2)) 
plot(perfCART)
plot(perfLog)

as.numeric(performance(pred, "auc")@y.values)

library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
RFmodel = randomForest(over50k ~., data=trainSmall)
PredictRF = predict(RFmodel, newdata = test)
table(PredictRF, test$over50k)

#How many splits
vu = varUsed(RFmodel, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
par(mfrow=c(1,1)) 
dotchart(vusorted$x, names(RFmodel$forest$xlevels[vusorted$ix]))

varImpPlot(RFmodel)

library(caret)
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
numFolds = trainControl( method = "cv", number = 10 )
train(over50k ~., data=train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

NewCART = rpart(over50k ~., data=train, method="class", cp = 0.002)
PredictNewCART = predict(NewCART, newdata=test, type="class")
table(test$over50k, PredictNewCART)

prp(NewCART)
