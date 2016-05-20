rm(list=ls())

library(e1071)
library(rpart)
# Basic concept
# Usage in R

load('~/Desktop/R/SVMKernel/svmdoc.RData')
data(Glass, package='mlbench')


# Split data into train and test
index <- 1:nrow(Glass)
testIndex <- sample(index, trunc(length(index) / 3))
testSet <- Glass[testIndex,]
trainSet <- Glass[-testIndex,]

# SVM
svm.model <- svm(Type ~ ., data=trainSet, cost=100, gamma=1)
svm.pred <- predict(svm.model, testSet[, -10])

# rpart
rpart.model <- rpart(Type ~ ., data=trainSet)
rpart.pred <- predict(rpart.model, testSet[, -10], type='class')

# Compute SVM and rpart confusion matrices
table(pred=svm.pred, true=testSet[,10])
table(pred=rpart.pred, true=testSet[,10])



# Non-linear Epsilon-Regression
data(Ozone, package='mlbench')

# split into training/test
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index) / 3))
testset <- na.omit(Ozone[testindex, -3])
trainset <- na.omit(Ozone[-testindex, -3])

# SVM
svm.model <- svm(V4 ~ ., data=trainset, cost=1000, gamma=0.0001)
svm.pred <- predict(svm.model, testset[, -3])
crossprod(svm.pred - testset[, 3]) / length(testindex)

# rpart
rpart.model <- rpart(V4 ~ ., data=trainset)
rpart.pred <- predict(rpart.model, testset[, -3])
crossprod(rpart.pred - testset[, 3]) / length(testindex)


# Elements of the svm object
# Other Main Features


save.image('~/Desktop/R/SVMKernel/svmdoc.RData')