stocks <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 6/StocksCluster.csv")

mean(stocks$PositiveDec)

cor(stocks)

summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

model1 = glm(PositiveDec~., data = stocksTrain, family = "binomial")
predicted_return = predict(model1, type="response")
table(stocksTrain$PositiveDec, predicted_return >= 0.5)

predicted_return = predict(model1, newdata = stocksTest, type="response")
table(stocksTest$PositiveDec, predicted_return >= 0.5)

summary(stocksTest$PositiveDec)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
clusterGroups_Kmeans = kmeans(normTrain, centers=3)
cluster_kmeans = split(normTrain, clusterGroups_Kmeans$cluster)
cluster1_kmeans = cluster_kmeans[[1]]
cluster2_kmeans = cluster_kmeans[[2]]
cluster3_kmeans = cluster_kmeans[[3]]

library(flexclust)
km.kcca = as.kcca(clusterGroups_Kmeans, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
predicted_return1 = predict(StocksModel1, newdata = stocksTest1, type="response")
table(stocksTest1$PositiveDec, predicted_return1 >= 0.5)
predicted_return2 = predict(StocksModel2, newdata = stocksTest2, type="response")
table(stocksTest2$PositiveDec, predicted_return2 >= 0.5)
predicted_return3 = predict(StocksModel3, newdata = stocksTest3, type="response")
table(stocksTest3$PositiveDec, predicted_return3 >= 0.5)

AllPredictions = c(predicted_return1, predicted_return2, predicted_return3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
