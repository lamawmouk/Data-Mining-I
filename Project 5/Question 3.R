# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW5 Data mining")
library(ISLR)
library(e1071) #install.packages("e1071")
# load the data
data(OJ)
OJ = data.frame(OJ)
#divide into test and training
set.seed(12345)
test_indis= sample(1:nrow(OJ), 2/3*nrow(OJ))
test= OJ[test_indis,]
training= OJ[-test_indis,]
####################################
# SVM with a linear kernel
####################################
test_error_linear <- c()
train_error_linear <- c()
for (i in c(0.01,0.1, 1, 5, 10)){ #predict purchase
  tune.model <- tune(svm, Purchase~., data = training, kernel = "linear",ranges = list(cost = i))
  bestmod <- tune.model$best.model #Use it for the predict---tells you the best model--use it to predict the test data
  bestmod
  
  ### predict test data ###
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_err <- length(which(y_hat == y_true))/length(y_true) 
  test_error_linear<-c(test_error_linear,test_err)
  ### predict train data ###
  y_hat1 <- predict(bestmod, newdata = training)
  y_true1 <- training$Purchase
  train_err <- length(which(y_hat1 == y_true1))/length(y_true1) 
  train_error_linear<-c(train_error_linear,train_err)
  
}

test_error_linear
train_error_linear

UB = max(test_error_linear, train_error_linear)
LB = min(test_error_linear, train_error_linear)

x11()
plot(c(0.01,0.1, 1, 5, 10),train_error_linear, type = "o", lty = 2, col = "red", ylim = c(LB -1, UB +1) , xlab = "cost", ylab = "Error", main = "Linear Kernel test and training Errors")
lines(c(0.01,0.1, 1, 5, 10),test_error_linear, type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","blue"))

####################################
# SVM with a radial kernel
####################################
test_error_radial <- c()
train_error_radial <- c()
for (i in c(0.01,0.1, 1, 5, 10)){
  
  tune.model <- tune(svm, Purchase~., data = training, kernel = "radial",ranges = list(cost = i))
  bestmod <- tune.model$best.model #Use it for the predict---tells you the best model--use it to predict the test data
  bestmod
  
  ### predict test data ###
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_err <- length(which(y_hat == y_true))/length(y_true) 
  test_error_radial<-c(test_error_radial,test_err)
  ### predict train data ###
  y_hat <- predict(bestmod, newdata = training)
  y_true <- training$Purchase
  train_err <- length(which(y_hat == y_true))/length(y_true) 
  train_error_radial<-c(train_error_radial,train_err)
  
}

test_error_radial
train_error_radial

UB = max(test_error_radial, train_error_radial)
LB = min(test_error_radial, train_error_radial)

x11()
plot(c(0.01,0.1, 1, 5, 10),train_error_radial, type = "o", lty = 2, col = "red", ylim = c(LB -1, UB +1) , xlab = "cost", ylab = "Error", main = "Radial Kernel test and training Errors")
lines(c(0.01,0.1, 1, 5, 10),test_error_radial, type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","blue"))

####################################
# SVM with a polyn kernel
####################################
test_error_polynomial <- c()
train_error_polynomial <- c()
for (i in c(0.01,0.1, 1, 5, 10)){
  
  tune.model <- tune(svm, Purchase~., data = training, degree = 2,kernel = "polynomial",ranges = list(cost = i))
  bestmod <- tune.model$best.model #Use it for the predict---tells you the best model--use it to predict the test data
  bestmod
  
  ### predict test data ###
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_err <- length(which(y_hat == y_true))/length(y_true) 
  test_error_polynomial<-c(test_error_polynomial,test_err)
  ### predict train data ###
  y_hat <- predict(bestmod, newdata = training)
  y_true <- training$Purchase
  train_err <- length(which(y_hat == y_true))/length(y_true) 
  train_error_polynomial<-c(train_error_polynomial,train_err)
  
}

test_error_polynomial
train_error_polynomial


UB = max(test_error_polynomial, train_error_polynomial)
LB = min(test_error_polynomial, train_error_polynomial)

x11()
plot(c(0.01,0.1, 1, 5, 10),train_error_polynomial, type = "o", lty = 2, col = "red", ylim = c(LB -1, UB +1) , xlab = "cost", ylab = "Error", main = "Polynomial Kernel test and training Errors")
lines(c(0.01,0.1, 1, 5, 10),test_error_polynomial, type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","blue"))




