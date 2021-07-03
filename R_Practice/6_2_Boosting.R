# Consider the pima data. Use boosting, random forests and a single tree (CART
# model). Comment on your performance. Explore the partial dependence plots for
# those variables that are have high ranking “variable importance”. 
rm(list = ls())

library(e1071)
library(tree)
library(caret)
dim(pima)
pima
attach(pima)
pima <-pima[,-8]
set.seed(100)

index = sample(1:nrow(pima), 2/3*nrow(pima))
test = pima[index,]
train = pima[-index,]

tree_model = tree(class~., data = train, split = "gini")
summary(tree_model)

pred_test = predict(tree_model, newdata = test, type = "class")
summary(pred_test)

confusionMatrix(pred_test, test$class)

cv.fit = cv.tree(tree_model, FUN = prune.tree)
summary(cv.fit)
cv.fit
which.min(cv.fit$dev) #13

pruned_tree = prune.tree(tree_model, best = 13)
pruned_tree

plot(pruned_tree)
text(pruned_tree, pretty = 0)

#Boosting
bagg_model = randomForest(class~., data = train, mtry = 7, n.trees = 500)
varImpPlot(bagg_model)
bagg_predict = predict(bagg_model, test, type = "Class")
bagg_predict
summary(bagg_model)
test$class = as.data.frame(class)
bagg_mse_test = mean((class - bagg_predict)^2)
bagg_mse_test


#random forest
rf_model = randomForest(class~., data = train)
varImpPlot(rf_model)
rf_model
rf_predict = predict(rf_model, test, type = "Class")
rf_mse = mean((test$class - rf_predict)^2)
rf_mse

