# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW5 Data mining")
library(ElemStatLearn)
library(neuralnet)
#load spam data
data(spam)
#convert spam and email into 0,1
spam$spam=ifelse(spam$spam == "spam",1,0)
# cv
cv.error=c()
cv.func=function(spam,hidden_1=c(5)){ 
    for(i in 1:5)  {
    test_indis= sample(1:nrow(spam), 0.75*nrow(spam)) # Divide data into train and test set
    test= spam[test_indis,]
    train= spam[-test_indis,]
    # nn fit 
    network=neuralnet(spam~.,data=train,hidden=hidden_1,err.fct='ce', linear.output=FALSE,threshold=0.5) 
    predict.network=compute(network,test[,1:57])
    #predict nn
    class=train$spam
    predict=round(predict.network$net.result)
    #class
    cv.error[i]=sum((class - predict)/nrow(predict))}
  return(mean(cv.error))
}
#Determine error for nn usin cv
set.seed(123)
error_train <-c()
error_test <-c()
for(i in 1:5){network<- neuralnet(spam~.,data=spam,hidden=c(i),err.fct='ce', linear.output=FALSE,threshold=0.5)
  error_train[i]=sum(((round(network$net.result[[1]])-(spam$spam))^2)/nrow(spam))
  error_test[i]=cv.func(spam,hidden=c(i))}
abs(error_train)
abs(error_test)
UB = max(error_test, error_train)
LB = min(error_test, error_train)
plot(abs(error_train), type = "o", lty = 2, col = "red", xlab = "Number of neurons for hidden layers", ylab = "Error", ylim = c(LB -1, UB +1),main = "Error for the number of neurons for hidden layers")
lines(abs(error_test), type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","blue"))

## neural networks with least test error
test_indis= sample(1:nrow(spam), 0.75*nrow(spam)) # Divide data into train and test set
test= spam[test_indis,]
train= spam[-test_indis,]
network=neuralnet(spam~.,data=train,hidden=5,err.fct='ce', linear.output=FALSE,threshold=0.5)
nncompute=compute(network,test[,1:57])
true_class=test$spam
pred_class<- round(nncompute$net.result)
nn.err=abs(sum(true_class-pred_class))/length(pred_class) 
nn.err
#confusion matrix
table(true_class,pred_class)

# Logistic regression model
library(caret)
set.seed(12345)
glm.fit <- glm(spam ~., data = train, family = "binomial")
# Predict
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_hat_test <- round(glm.probs.test)
glm.probs.train <- predict(glm.fit, newdata = train, type = "response")
y_hat_train <- round(glm.probs.train)
#  Calculate the error rates
test_err <- sum(abs(y_hat_test- test$spam))/length(test$spam)
test_err
train_err <- sum(abs(y_hat_train- train$spam))/length(train$spam)
train_err
#  Confusion Matrix
conf <- confusionMatrix(as.factor(y_hat_test), as.factor(test$spam))
conf$table
