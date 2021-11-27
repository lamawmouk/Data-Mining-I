# Delete all the objects in the workspace 
rm(list=ls())
#set working directory
setwd("C:/Users/lama_/Desktop/Data mining/HW4_Datamining")
#https://www.kaggle.com/ronitf/heart-disease-uci
heart <- read.csv("heart.csv")
#divide train and test. 
set.seed(1234)
index = sample(1:nrow(heart), 2/3*nrow(heart))
train = heart[index,]
test = heart[-index,]
y_true = test$target # track the test error ///RV =target - have disease or not (1=yes, 0=no)
###############################################
# Random Forest 
###############################################
library(randomForest)
set.seed(1234)
rf.fit <- randomForest(target~., data = train, n.tree = 10000)
x11()
varImpPlot(rf.fit)#Asses how important each vrabule is on the tree-gives ranked importance-less important on the bottom-bigests indicator on the top--measure of impurity/average decrease in gini indiex --when the ranked variable is left out the accuracy suffers
importance#Get the same info in table form
y_hat <- predict(rf.fit, newdata = test, type = "response")
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf # 0.2754411
###############################################
# Bagging
###############################################
bag.fit <- randomForest(target~., data = train, n.tree = 10000, mtry = 10)
x11()
varImpPlot(bag.fit)# fives the ranked
importance(bag.fit)
y_hat <- predict(bag.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)
misclass_bag <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_bag # 0.2821142

###############################################
# Boosting
###############################################
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.train <- train;
boost.train$target <- as.numeric(train$target)
boost.test <- test;
boost.test$target <- as.numeric(test$target)
boost.fit <- gbm(target~., data = boost.train, n.trees = 1000, shrinkage = .1, interaction.depth = 3, distribution = "adaboost")
boost.fit2 <- gbm(target~., data = boost.train, n.trees = 1000, shrinkage = .6, interaction.depth = 3, distribution = "adaboost")
summary(boost.fit) #shows the relative importance of the variables
# Look at the error for shrinkage = .1
y_hat <- predict(boost.fit, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.1 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.1 
# Look at the error for shrinkage = .6
y_hat <- predict(boost.fit2, newdata = boost.test, n.trees = 1000, type = "response")
misclass_boost.6 <- sum(abs(y_hat - y_true))/ length(y_true)
misclass_boost.6 

##########################################
logistic<-glm(target ~., data = train,family = "binomial")
predict_logistic<-predict(logistic,newdata = test, type = "response")
y_hat_test <- round(predict_logistic)
misclass_lm<-sum(abs(y_hat_test - y_true))/length(y_true)
# Confusion Matrix
confusionMatrix(as.factor(y_hat_test), as.factor(test$target))   

require(class)
knntesterror=sapply(1:20,function(k){
  knn_test_predict <- knn(train, test,train$target,k)
  mean(knn_test_predict!=train$target)})
plot(knntesterror,type="o",lty=2,col = "blue",ylim = c(0.5,0.65),xlab = "k",ylab="error",main="KNN error")

