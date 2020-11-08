# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW3 Data Mining")
#Load the library
library(MASS)
data("Boston")
Boston$crime_binary = rep(0,nrow(Boston))
med_crime=median(Boston$crim)
Boston$crime_binary =ifelse(Boston$crim>median(Boston$crim),"1","0")
Boston$crime_binary=as.numeric(Boston$crime_binary )
summary(Boston$crime_binary )
Boston <- Boston[,-1 ]
# Create a test and training dataset
library(caret)
set.seed(12345)
indices = sample(1:nrow(Boston), nrow(Boston)*.75)
Boston_train = Boston[indices, ]
Boston_test = Boston[-indices, ]
dim(Boston_train)
dim(Boston_test)
library(corrplot)
x11()
corrplot.mixed(cor(Boston_train[, -14]), upper="circle")
correlation= cor(Boston_train[,-14])
#correlation
highcorrelation= findCorrelation(correlation, cutoff = 0.75)
highcorrelation
Boston_train= Boston_train[,-(c(9))]
Boston_test = Boston_test[,-(c(9))]
#####################Logistic Regression##################################################
#Fit 1
glm.fit <- glm(crime_binary ~ nox +rad+dis, data = Boston_train, family = "binomial")
#summary(glm.fit);names(glm.fit)
# Predict
glm.probs.test <- predict(glm.fit, newdata = Boston_test, type = "response")
y_hat_test <- round(glm.probs.test)
glm.probs.train <- predict(glm.fit, newdata = Boston_train, type = "response")
y_hat_train <- round(glm.probs.train)
#  Calculate the error rates
test_err <- sum(abs(y_hat_test- Boston_test$crime_binary))/length(Boston_test$crime_binary)
test_err
train_err <- sum(abs(y_hat_train- Boston_train$crime_binary))/length(Boston_train$crime_binary)
train_err
#  Confusion Matrix
conf <- confusionMatrix(as.factor(y_hat_test), as.factor(Boston_test$crime_binary))
conf
conf$table
#################LDA#################################################### 
#Fit 1
lda.fit <- lda(crime_binary ~ nox +rad+dis, data = Boston_train)
lda.pred.train <- predict(lda.fit, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1
# Compute the error
lda_train_error <- sum(abs(y_hat_train- Boston_train$crime_binary))/length(Boston_train$crime_binary)
lda_test_error <- sum(abs(y_hat_test- Boston_test$crime_binary))/length(Boston_test$crime_binary)
lda_train_error
lda_test_error
#  Confusion Matrix
confusionMatrix(as.factor(y_hat_test), as.factor(Boston_test$crime_binary))      


############K-Nearest Neighobour###################################################################
#Creating kNN classification model to determine the testing error 
require(class)
xtrain = Boston_train[, c('dis','nox','rad')]
xtest= Boston_test[, c('dis','nox','rad')]
knntesterror=sapply(1:10,function(k){
  knn_test_predict <- knn(xtrain, xtest,Boston_train$crime_binary,k)
  mean(knn_test_predict!=Boston_test$crime_binary)})
knntesterror



