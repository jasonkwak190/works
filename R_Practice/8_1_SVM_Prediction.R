rm(list = ls())
library(ISLR)
library(e1071)
library(caret)
dim(OJ)
names(OJ)
# 1)	This problem involves the “OJ” data set in the ISLR package.  
# (Data can also be found on the book website).
# 
# a)(5pts) Create a training set containing a random sample of 2/3 of the data, 
# and a test set of the remaining 1/3.
set.seed(100)
index = sample(nrow(OJ), nrow(OJ)*2/3)
train = OJ[index,]
test = OJ[-index,]
dim(train) #[1] 713  18
dim(test)  #[1] 357  18


# b)(10pts) Fit a support vector classifier (aka as a support vector machine 
# with a linear kernel) using a cost = 0.01, with “Purchase” as the response variable.  
# Describe the results, what are the test and training errors?  
# How many support vectors are there?
oj.svm = svm(Purchase~., data = train, kernel ='linear', cost = 0.01, scale = FALSE)
oj.svm #Number of Support Vectors: 566
summary(oj.svm)
tune.vector = tune(svm, Purchase~., data = train, kernel = "linear", range = 0.01)
summary(tune.vector) #Error estimation of ‘svm’ using 10-fold cross validation: 0.1811033
bestmod.vector = tune.vector$best.model
bestmod.vector #Number of Support Vectors:  303

#test vec
y_hat.vec = predict(bestmod.vector, newdata = test)
y_true.vec = test$Purchase
acc = length(which(y_hat.vec == y_true.vec))/length(y_true.vec)
acc  #[1] 0.8403361
confusionMatrix(y_hat.vec, y_true.vec)

#train vec
y_hat.vec.train = predict(bestmod.vector, newdata = train)
y_true.vec.train = train$Purchase
acc = length(which(y_hat.vec.train == y_true.vec.train))/length(y_true.vec.train)
acc  #[1] 0.8345021
confusionMatrix(y_hat.vec.train, y_true.vec.train)


# c)(10pts) Use the “tune()” function to select an optimal cost.   
# Consider values in the range of 0.01 to 10.  Report the training 
# and test error rates for the optimal cost.  
tune.model = tune(svm, Purchase~., data = train, kernel = "linear",
                  ranges = seq(0.01,10, length.out = 20))
summary(tune.model) #best parameters: 0.01 | best performance: 0.1711463
bestmod <- tune.model$best.model
bestmod #Number of Support Vectors:  303

#predict test data
y_hat = predict(bestmod, newdata = test)
y_true = test$Purchase
acc1 = length(which(y_hat == y_true))/length(y_true)
acc1  #[1] 0.8403361
confusionMatrix(y_hat, y_true)

#predict train data
y_hat_train = predict(bestmod, newdata = train)
y_true_train = train$Purchase
acc2 = length(which(y_hat_train == y_true_train))/length(y_true_train)
acc2  #[1] 0.8345021
confusionMatrix(y_hat_train, y_true_train)



# 2) EXTRA CREDIT
# For the same test and training used in Q1.
# a) (10 pts) Fit a Support vector machine with a radial kernel, using the default value for
# gamma and a cost = 0.01. Describe the results, what are the test and training
# errors? How many support vectors are there?
oj.svm.rad = svm(Purchase~., data = train, kernel ='radial', cost = 0.01, scale = FALSE)
oj.svm.rad #Number of Support Vectors:  573
summary(oj.svm.rad)

#test
tune.rad = tune(svm, Purchase~., data = train, kernel = "radial", range = 0.01)
summary(tune.rad) #Error estimation of ‘svm’ using 10-fold cross validation: 0.1852113
best.tune.rad = tune.rad$best.model
best.tune.rad #Number of Support Vectors:  349

yrt = predict(best.tune.rad, newdata = test)
ytt = test$Purchase
acc3 = length(which(yrt == ytt))/length(yrt)
acc3 #[1] 0.8403361

#train
yrtt = predict(best.tune.rad, newdata = train)
yttt = train$Purchase
acc4 = length(which(yrtt == yttt))/length(yrtt)
acc4 #[1] 0.8471248

#   d) (10 pts) Use the “tune()” function to select an optimal gamma and cost. Report the
# training and test error rates for the optimal cost.
tune.model.rad = tune(svm, Purchase~., data = train, kernel= "radial",
                      ranges = list(cost = c(0.001, 0.01, 1, 5, 10), gamma = c(0.1, 0.5, 1, 2, 3, 4)))
summary(tune.model.rad)
bestmodrad = tune.model.rad$best.model
bestmodrad 
#best parameters -> cost:1 gamma 0.1
#best performance: 0.1936228 
#cost:  1 | Number of Support Vectors: 358

#test
y_hat_test_rad = predict(bestmodrad, newdata = test)
y_true_test_rad = test$Purchase
acc5 = length(which(y_hat_test_rad == y_true_test_rad))/length(y_true_test_rad)
acc5 #[1] 0.8431373
confusionMatrix(y_hat_test_rad, y_true_test_rad)

#train
y_hat_train_rad = predict(bestmodrad, newdata = train)
y_true_train_rad = train$Purchase
acc6 = length(which(y_hat_train_rad == y_true_train_rad))/length(y_true_train_rad)
acc6 #[1] 0.8457223
confusionMatrix(y_hat_train_rad, y_true_train_rad)

