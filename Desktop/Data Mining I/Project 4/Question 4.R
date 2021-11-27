# Delete all the objects in the workspace 
rm(list=ls())
#set working directory
setwd("C:/Users/lama_/Desktop/Data mining/HW4_Datamining")
#load the data
library(kernlab)
data("spam")
names(spam)
#View(spam)
# divide into test and training
set.seed(12345)
test_indis <- sample(1:nrow(spam), 2/3*nrow(spam))
test <- spam[test_indis,]
train <- spam[-test_indis,]
y_true <- as.numeric(test$type)-1 # convert the spam & non-spam into 0=non-spam/email or 1=spam 
###############################################
# Random Forest 
###############################################
#install.packages("randomForest")
library(randomForest)
modelName = c()
test_error = c()
for(i in seq(1:50)){
  rf.fit = randomForest(type~.,data = train, ntree =100, mtry = i)
  rf.fit.pred_test <- predict(rf.fit, newdata = test,type="response")
  y_hat <- predict(rf.fit, newdata = test, type = "response")
  y_hat <- as.numeric(y_hat)-1
  misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
  
  modelName = c(modelName,i)
  test_error = c(test_error,misclass_rf)
}
x11()
errorDF = data.frame(m = modelName, Test_Error = test_error)
plot(errorDF[,1],errorDF[,2],type='o', xlab="m", ylab="Test error")
library(randomForest)
modelName = c()
test_error = c()
for(i in seq(1:20)){
  rf.fit = randomForest(type~.,data = train, ntree =100, mtry = i)
  rf.fit.pred_test <- predict(rf.fit, newdata = test,type="response")
  y_hat <- predict(rf.fit, newdata = test, type = "response")
  y_hat <- as.numeric(y_hat)-1
  misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
  
  modelName = c(modelName,i)
  test_error = c(test_error,misclass_rf)
}
x11()
errorDF = data.frame(m = modelName, Test_Error = test_error)
plot(errorDF[,1],errorDF[,2],type='o', xlab="m", ylab="Test error")
##################################################################
store = c()
for(i in seq(1,8,2)){
  rf.fit = randomForest(type~., data  = train, ntree = 100, mtry = i)
  rf.predictions = predict(rf.fit, test, type = "class")
  store = cbind(store, rf.fit$err.rate[,c(1)])
}

OOBerror = data.frame(store)
names(OOBerror) = c("m2", "m4","m6","m8")
OOBerror$NTrees = seq(1, 100)

#plott OOB error
library(ggplot2)
ggplot(OOBerror, aes(NTrees)) + 
  geom_line(aes(y = m2, color = "m2")) + geom_point(aes(y = m2), size = 0.2) + 
  geom_line(aes(y = m4, color = "m4")) + geom_point(aes(y = m4), size = 0.2) + 
  geom_line(aes(y = m6, color = "m6")) + geom_point(aes(y = m6), size = 0.2) + 
  geom_line(aes(y = m8, color = "m8")) + geom_point(aes(y = m8), size = 0.2) + 
  ggtitle("OOB error") + xlab("Number of trees") + ylab("OOB Error") 
 


store = c()
for(i in seq(0,20,5)){
  rf.fit = randomForest(type~., data  = train, ntree = 100, mtry = i)
  rf.predictions = predict(rf.fit, test, type = "class")
  store = cbind(store, rf.fit$err.rate[,c(1)])
}

OOBerror = data.frame(store)
names(OOBerror) = c("m5", "m10","m15","m20")
OOBerror$NTrees = seq(1, 100)

#plott OOB error
x11()
library(ggplot2)
ggplot(OOBerror, aes(NTrees)) + 
  geom_line(aes(y = m5, color = "m5")) + geom_point(aes(y = m5), size = 0.2) + 
  geom_line(aes(y = m10, color = "m10")) + geom_point(aes(y = m10), size = 0.2) + 
  geom_line(aes(y = m15, color = "m15")) + geom_point(aes(y = m15), size = 0.2) + 
  geom_line(aes(y = m20, color = "m20")) + geom_point(aes(y = m20), size = 0.2) + 
  ggtitle("OOB error") + xlab("Number of trees") + ylab("OOB Error") 
