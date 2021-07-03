# 1) (a) Apply bagging, boosting, and random forests to a data set 
# of your choice.
# Fit the models on a training set and evaluate them on a test set.
# b) How accurate are these results compared to more simplistic 
# (non-ensemble) methods (e.g., logistic regression, kNN, etc)? 
# Use the same test/training as in part A.
# c) What are some advantages (and disadvantages) do committee 
# machines have related to the data set that you selected?

rm(list = ls())

library(ISLR)
library(randomForest)
library(gbm)
library(tree)
dim(Hitters)
summary(Hitters)
names(Hitters)
head(Hitters)


###########################################################


#Erase NA
Hitters <- Hitters[-which(is.na(Hitters$Salary)),]

dim(Hitters)

Hitters$Salary <- log(Hitters$Salary)

split = sample(nrow(Hitters), nrow(Hitters)*.8)
train = Hitters[split,]
test = Hitters[-split,]

#Decision tree
tree.hitters = tree(Salary~., data = train, split = "gini")
summary(tree.hitters)

predict_tree = predict(tree.hitters, newdata = train)
summary(predict_tree)
cv.fit = cv.tree(tree.hitters, FUN = prune.tree)
summary(cv.fit)
which.min(cv.fit$dev) #24
pruned_tree = prune.tree(tree.hitters, best = 10)
plot(pruned_tree)
text(pruned_tree, pretty = 0, cex = 0.5)

#pruned vs unpruned error rate
prune.pred = predict(pruned_tree, newdata = test)
mean((test$Salary - prune.pred)^2)   #0.1542393
unprune.pred = predict(tree.hitters, newdata = test)
mean((test$Salary - unprune.pred)^2) #0.1709158

###



#Bagging
bagging_model = randomForest(Salary~., data=train, mtry=19, n.trees = 500)
varImpPlot(bagging_model) #check importance: CAtBat, CRuns, CHits, ...
bagging_predict = predict(bagging_model, test, type = "Class")
bagging_predict
bagging_mse_test = mean((test$Salary-bagging_predict)^2)
bagging_mse_test #0.2094662

#Boosting
boost_model = gbm(Salary~., data = train, distribution = "gaussian", n.trees = 500, interaction.depth = 4)
summary(boost_model)
boost_predict = predict(boost_model, test, n.trees = 500)
boosting_mse = mean((test$Salary-boost_predict)^2)
boosting_mse  #0.2206112

#Random Forest model
rf_model = randomForest(Salary~., data = train)
varImpPlot(rf_model)
rf_model
rf_predict = predict(rf_model, test, type = "Class")
rf_mse = mean((test$Salary - rf_predict)^2)
rf_mse  #0.1996369

#Linear regression
lr_model = lm(formula = Salary~., data = train)
summary(lr_model)
lr_pred = predict(lr_model, test)
lr_pred
lr_mse = mean((test$Salary-lr_pred)^2)
lr_mse #0.3567222

