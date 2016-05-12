gerber <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 4/gerber.csv")

summary(gerber$voting)

tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$self ,mean)
tapply(gerber$voting, gerber$neighbors, mean)

model1 = glm(voting ~ civicduty+hawthorne+self+neighbors, data=gerber, family="binomial")
summary(model1)

pred_vote = predict(model1, type="response")
table(gerber$voting, pred_vote>0.3)

table(gerber$voting, pred_vote>0.5)

library(ROCR)
pred = prediction(pred_vote, gerber$voting)
as.numeric(performance(pred, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTcontrol_sex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
abs(0.34-0.296638)

prp(CARTcontrol_sex, digits=6)
abs(0.290456-0.334176)-abs(0.302795-0.345818)

model2 = glm(voting ~ sex + control, data=gerber, family="binomial")
summary(model2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model2, newdata=Possibilities, type="response")
abs(0.290456-0.2908065)

model3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(model3)

predict(model3, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)
