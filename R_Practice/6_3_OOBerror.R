rm(list = ls())

setwd("C:/Users/Win10/Desktop/code")
library(randomForest)
library(rpart)

spambase
attach(spambase)
# 3) (ESL Exercise 15.6) Fit a series of random-forest classifiers to the SPAM
# data, to explore the sensitivity to m (the number of randomly selected inputs for
# each tree). Plot both the OOB error as well as the test error against a suitably
# chosen range of values for m. 
set.seed(100)

index = sample(nrow(spambase), nrow(spambase)*0.8)
train <- spambase[index,]
test <- spambase[-index,]



spam.random = randomForest(V58~., data = train, n.tree = 500)
spam.random
varImpPlot(spam.random)
spam_predict = predict(spam.random, test, type = "class")
summary(spam_predict)
spam_mse = mean((test$V58 - spam_predict)^2)
spam_mse   #0.04318207


OOB <- randomForest(V58~., data = train, mtry = 10, importance = TRUE, do.trace = 50)
OOB
plot(OOB)
