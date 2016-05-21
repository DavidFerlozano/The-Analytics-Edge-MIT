Sys.setlocale("LC_ALL", "C")
emails = read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 5/emails.csv", stringsAsFactors=FALSE)

table(emails$spam)

max(nchar(emails$text))

which.min(nchar(emails$text))

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam

ham = subset(emailsSparse, spam == 0)
which(colSums(ham)>5000)

spam = subset(emailsSparse, spam == 1)
which(colSums(spam)>=1000)

emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)
spamLog = glm(spam~., data = train, family = "binomial")
spamCART = rpart(spam~., data = train, method = "class")
set.seed(123)
spamRF = randomForest(spam~., data=train)
trainPredictLog = predict(spamLog, type = "response")
trainPredictCART = predict(spamCART)[,2]
trainPredictRF = predict(spamRF, type = "prob")[,2]

length(which(trainPredictLog<0.00001))
length(which(trainPredictLog>0.99999))
length(which(trainPredictLog>0.00001 & trainPredictLog<0.99999))

summary(spamLog)

prp(spamCART)

table(train$spam, trainPredictLog > 0.5)

library(ROCR)
pred = prediction(trainPredictLog, train$spam)
as.numeric(performance(pred, "auc")@y.values)

table(train$spam, trainPredictCART > 0.5)

pred = prediction(trainPredictCART, train$spam)
as.numeric(performance(pred, "auc")@y.values)

table(train$spam, trainPredictRF > 0.5)
pred = prediction(trainPredictRF, train$spam)
as.numeric(performance(pred, "auc")@y.values)

testPredictLog = predict(spamLog, newdata = test, type = "response")
testPredictCART = predict(spamCART, newdata = test)[,2]
testPredictRF = predict(spamRF, newdata = test, type = "prob")[,2]

table(test$spam, testPredictLog > 0.5)
pred = prediction(testPredictLog, test$spam)
as.numeric(performance(pred, "auc")@y.values)

table(test$spam, testPredictCART > 0.5)
pred = prediction(testPredictCART, test$spam)
as.numeric(performance(pred, "auc")@y.values)

table(test$spam, testPredictRF > 0.5)
pred = prediction(testPredictRF, test$spam)
as.numeric(performance(pred, "auc")@y.values)
