parole <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 3/parole.csv")
nrow(parole)

table(parole$violator)

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model1 = glm(violator~., data=train, family="binomial")
summary(model1)

predictTest = predict(model1, newdata = test, type="response")
max(predictTest)

table(test$violator, predictTest >= 0.5)

table(test$violator)

library(ROCR)
pred = prediction(predictTest, test$violator)
as.numeric(performance(pred, "auc")@y.values)


