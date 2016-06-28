energy = read.csv("energy.csv")

energy[which.max(energy$GenTotalRenewable),]

temp1 = energy[complete.cases(energy$AllSourcesCO2),]
sub1 = temp1[which(temp1$presidential.results==0),]
mean(sub1$AllSourcesCO2)
sub2 = temp1[which(temp1$presidential.results==1),]
mean(sub2$AllSourcesCO2)

temp2 = energy[complete.cases(energy$AllSourcesNOx),]
sub1 = temp2[which(temp2$presidential.results==0),]
mean(sub1$AllSourcesNOx)
sub2 = temp2[which(temp2$presidential.results==1),]
mean(sub2$AllSourcesNOx)

cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use="complete")

cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use="complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use="complete")
cor(energy$AllSourcesCO2, energy$EsalesCommercial, use="complete")


boxplot(tapply(energy$EPriceTotal, energy$STATE, mean))
which.min(tapply(energy$EPriceTotal, energy$STATE, mean))
boxplot(energy$EPriceTotal)

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
mod = glm(GenSolarBinary~GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data = train, family = "binomial")
summary(mod)

predictTest = predict(mod, newdata = test, type="response")
table(test$GenSolarBinary, predictTest>=0.5)
test_rep = test[which(test$presidential.results==0),]
test_dem = test[which(test$presidential.results==1),]
predictTest_rep = predict(mod, newdata = test_rep, type="response")
predictTest_dem = predict(mod, newdata = test_dem, type="response")
table(test_rep$GenSolarBinary, predictTest_rep>=0.5)
table(test_dem$GenSolarBinary, predictTest_dem>=0.5)


#second part
set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
train.limited = subset(train, select = c(CumlRegulatory, CumlFinancial, presidential.results, Total.salary,Import))
test.limited = subset(test, select = c(CumlRegulatory, CumlFinancial, presidential.results, Total.salary,Import))
library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)
set.seed(144)
km = kmeans(train.norm, centers=2,iter.max=1000)
library(flexclust)
km.kcca = as.kcca(km, train.norm)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=test.norm)
train1 = subset(train, clusterTrain == 1)
train2 = subset(train, clusterTrain == 2)
test1 = subset(test, clusterTest == 1)
test2 = subset(test, clusterTest == 2)

length(which(train1$presidential.results == 0))
length(which(train2$presidential.results == 0))
mean(train1$CumlRegulatory)
mean(train2$CumlRegulatory)
temp1 = train1[complete.cases(train1$AllSourcesCO2),]
temp2 = train2[complete.cases(train2$AllSourcesCO2),]
mean(temp1$AllSourcesCO2)
mean(temp2$AllSourcesCO2)
temp1 = train1[complete.cases(train1$AllSourcesSO2),]
temp2 = train2[complete.cases(train2$AllSourcesSO2),]
mean(temp1$AllSourcesSO2)
mean(temp2$AllSourcesSO2)
temp1 = train1[complete.cases(train1$AllSourcesNOx),]
temp2 = train2[complete.cases(train2$AllSourcesNOx),]
mean(temp1$AllSourcesNOx)
mean(temp2$AllSourcesNOx)

model1 = glm(GenSolarBinary ~ GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data=train1, family=binomial)
summary(model1)

PredictTest1 = predict(model1, newdata = test1, type="response")
table(test1$GenSolarBinary, PredictTest1 > 0.5)
PredictTest1_mod = predict(mod, newdata = test1, type="response")
table(test1$GenSolarBinary, PredictTest1_mod >= 0.5)

model2 = glm(GenSolarBinary ~ GenHydro+GenSolar+CumlFinancial+CumlRegulatory+Total.salary+Import, data=train2, family=binomial)
summary(model2)
PredictTest2 = predict(model2, newdata = test2, type="response")
table(test2$GenSolarBinary, PredictTest2 > 0.5)
PredictTest2_mod = predict(mod, newdata = test2, type="response")
table(test2$GenSolarBinary, PredictTest2_mod >= 0.5)

AllPredictions = c(PredictTest1, PredictTest2)
AllOutcomes = c(test1$GenSolarBinary, test2$GenSolarBinary)
table(AllOutcomes, AllPredictions > 0.5)
