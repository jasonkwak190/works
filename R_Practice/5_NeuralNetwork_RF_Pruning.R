rm(list = ls())
#install.packages("neuralnet")
library(nnet)
library(neuralnet)
library(caret)
library(rpart)
library(randomForest)
library(tree)
library(gbm)
head(cleveland)
attach(cleveland)

#check for missing data
which(is.na(cleveland) == TRUE)  #integer(0) no missing data
table(cleveland$diag1)
prop.table(table(cleveland$diag1))
# buff      sick 
# 160        136 
# 0.5405405 0.4594595 


#divide the data
set.seed(100)
cleveland$diag1 <- as.numeric(cleveland$diag1)
table(cleveland$diag1) #buff:1 , sick: 2
index <- sample(1:length(cleveland[,1]), 2/3*length(cleveland[,1]))
train <- cleveland[index, ]
train <- train[1:14]
test <- cleveland[-index, ]
test <- test[1:14]
head(train)
head(test)
head(cleveland)


#train neural network
nn0 <- neuralnet(diag1 ~ age + trestbps + chol + thalach + oldpeak, data = train,
                hidden = 1, err.fct = "sse", linear.output = FALSE)

plot(nn0)

#controlling the neurons
nn1 <- neuralnet(diag1 ~ age + trestbps + chol + thalach + oldpeak, data = train,
                hidden = 2, err.fct = "sse", linear.output = FALSE)
plot(nn1)


#controlling the neurons
nn2 <- neuralnet(diag1 ~ age + trestbps + chol + thalach + oldpeak, data = train,
                 hidden = c(2,3), stepmax = 10^9, err.fct = "sse", linear.output = FALSE)
plot(nn2)

#make prediction with the neural network
?predict.nn
pred <- predict(nn1, newdata = train)
y_hat_train <- round(pred)
train_err <- length(which(train$diag1!=y_hat_train))/length(y_hat_train)
train_err #[1] 0.4568528

pred <- predict(nn1, newdata = test)
y_hat_test <- round(pred)
test_err <- length(which(test$diag1!=y_hat_test))/length(y_hat_test)
test_err #[1] 0.4646465

#put in a loop for tuning
train_err_store <- c()
test_err_store <- c()
for(i in 1:4){
  nn1 <- neuralnet(diag1 ~ age + trestbps + chol + thalach + oldpeak, data = train,
                   hidden = i, stepmax = 10^9, err.fct = "sse", linear.output = FALSE)
  
  pred <- predict(nn1, newdata = train)
  y_hat_train <- round(pred)
  train_err <- length(which(train$diag1!=y_hat_train))/length(y_hat_train)
  train_err #[1] 0.4365482
  train_err_store <- c(train_err_store, train_err)
  
  pred <- predict(nn1, newdata = test)
  y_hat_test <- round(pred)
  test_err <- length(which(test$diag1!=y_hat_test))/length(y_hat_test)
  test_err #[1] 0.5050505
  test_err_store <- c(test_err_store, test_err)
}

train_err_store
test_err_store

#compare to cart random forest
#CART
tree.cl = tree(diag1~., data = train, split = "gini")
summary(tree.cl)

predict_tree = predict(tree.cl, newdata = train)
summary(predict_tree)

cv.fit = cv.tree(tree.cl, FUN = prune.tree)
summary(cv.fit)
which.min(cv.fit$dev) #12

pruned_tree = prune.tree(tree.cl, best = 10)

plot(pruned_tree)
text(pruned_tree, pretty = 0, cex = 0.5)

#pruned error rate
prune.pred = predict(pruned_tree, newdata = train)
y_hat_prune = round(prune.pred)
prune.train = length(which(train$diag1 != y_hat_prune))/length(y_hat_prune)
prune.train #0.1370558

prune.pred.test = predict(pruned_tree, newdata = test)
y_hat_prune = round(prune.pred.test)
prune.test = length(which(test$diag1 != y_hat_prune))/length(y_hat_prune)
prune.test #0.2727273

#Boosting
boost_model = gbm(diag1~., data = train, distribution = "gaussian", n.trees = 500, interaction.depth = 4)
summary(boost_model)
boost_predict = predict(boost_model, test, n.trees = 500)
boosting_mse = mean((test$diag1-boost_predict)^2)
boosting_mse  #0.1740702

#Random Forest model
rf_model = randomForest(diag1~., data = train)
varImpPlot(rf_model)
rf_model
rf_predict = predict(rf_model, test, type = "Class")
rf_mse = mean((test$diag1 - rf_predict)^2)
rf_mse  #0.1196024

