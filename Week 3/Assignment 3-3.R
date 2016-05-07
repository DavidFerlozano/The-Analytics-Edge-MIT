loans <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 3/loans.csv")
table(loans$not.fully.paid)

sapply(loans, function(x) sum(is.na(x)))

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
model1 = glm(not.fully.paid~., data = train, family = "binomial")
summary(model1)

test$predicted.risk = predict(model1, newdata = test, type="response")
table(test$not.fully.paid, test$predicted.risk >= 0.5)
table(test$not.fully.paid)

library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

predictTest = predict(bivariate, newdata = test, type="response")
max(predictTest)
table(test$not.fully.paid, predictTest >= 0.5)

pred_biv = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(pred_biv, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)*10

high_int = subset(test, int.rate >= 0.15)
mean(high_int$profit)
table(high_int$not.fully.paid)

cutoff = sort(high_int$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(high_int, predicted.risk <= cutoff)
nrow(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)

