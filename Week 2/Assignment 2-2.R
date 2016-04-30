test <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 2/pisa2009test.csv")
train <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 2/pisa2009train.csv")

nrow(train)

tapply(train$readingScore,train$male,mean)

which(train==NA)

sapply(train, function(x) sum(is.na(x)))

train = na.omit(train)
test = na.omit(test)
nrow(train)
nrow(test)

str(train$raceeth)
str(train$male)
str(train$grade)

train$raceeth = relevel(train$raceeth, "White")
test$raceeth = relevel(test$raceeth, "White")
lmScore = lm(readingScore~., data=train)
summary(lmScore)

SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(train))
RMSE

lmScore$coefficients

summary(lmScore)

predTest = predict(lmScore, newdata=test)
max(predTest)-min(predTest)

SSE_test = sum((predTest-test$readingScore)^2)
SSE_test

RMSE_test = sqrt(SSE_test/length(predTest))
RMSE_test

pred_baseline = mean(train$readingScore)
pred_baseline

SSE_baseline = sum((pred_baseline-test$readingScore)^2)
SSE_baseline

R2_pred = 1 - SSE_test/SSE_baseline
R2_pred
