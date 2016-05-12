letters <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 4/letters_ABPR.csv")

letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
library(caTools)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
table(test$isB)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
PredictTest1 = predict(CARTb, newdata = test, type = "class")
table(test$isB, PredictTest1)

library(randomForest)
isBForest = randomForest(isB ~ . - letter, data = train)
PredictForest1 = predict(isBForest, newdata = test)
table(test$isB, PredictForest1)

letters$letter = as.factor( letters$letter )
set.seed(2000)
split2 = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, split2 == TRUE)
test2 = subset(letters, split2 == FALSE)
table(test2$letter)

CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
PredictTest2 = predict(CARTletter, newdata = test2, type = "class")
table(test2$letter, PredictTest2)
sum(diag(table(test2$letter, PredictTest2)))/1558

letterForest = randomForest(letter ~ . - isB, data = train2)
PredictForest2 = predict(letterForest, newdata = test2)
table(test2$letter, PredictForest2)
sum(diag(table(test2$letter, PredictForest2)))/1558
