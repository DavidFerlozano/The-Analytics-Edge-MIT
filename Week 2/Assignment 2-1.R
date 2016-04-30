climate <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 2/climate_change.csv")
train <- subset(climate, Year <= 2006)
test <- subset(climate, Year > 2006)
climatelm <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)

cor(train)

LinReg <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(LinReg)

StepModel <- step(climatelm)
summary(StepModel)

tempPredict <- predict(StepModel, newdata = test)
SSE <- sum((tempPredict - test$Temp)^2)
SST <- sum( (mean(train$Temp) - test$Temp)^2)
R2 <- 1 - SSE/SST
