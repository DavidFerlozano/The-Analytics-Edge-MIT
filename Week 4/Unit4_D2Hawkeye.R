# Unit 4 - "Keeping an Eye on Healthcare Costs" Lecture


# VIDEO 6

# Read in the data
Claims <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 4/ClaimsData.csv", header=TRUE)

str(Claims)

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# Split the data
library(caTools)

set.seed(88)

spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)

ClaimsTrain = subset(Claims, spl==TRUE)

ClaimsTest = subset(Claims, spl==FALSE)


# VIDEO 7

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)


# VIDEO 8

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
ClaimsTree1 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree1)


# Make predictions
PredictTest1 = predict(ClaimsTree1, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest1)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest1))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest1))*PenaltyMatrix)/nrow(ClaimsTest1)

# New CART model with loss matrix
ClaimsTree2 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))

# Redo predictions and penalty error
PredictTest2 = predict(ClaimsTree2, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest2)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest2)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest2))*PenaltyMatrix)/nrow(ClaimsTest)
