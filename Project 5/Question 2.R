# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW5 Data mining")
heart <- read.csv("heart.csv")
#Divide train and test. 
set.seed(1234)
index = sample(1:nrow(heart), 0.75*nrow(heart))
train = heart[index,]
outlier_train=heart[index,];outlier_train[1,"trestbps"]=400
test = heart[-index,]

#After the insertion of outlier
boxplot(outlier_train$trestbps,horizontal=TRUE,xlab=" Amount of Trestbps",col="orange",main="Boxplot: Trestbps Univariate outlier")

#Fit separate singlehidden-layer neural networks to the original learning-set data 
library(neuralnet)  
network=neuralnet(target~.,data=train,hidden=1,err.fct='ce', linear.output=FALSE,threshold=0.5)
  nncompute=compute(network,test[,1:13])
  true_class=test$target
  predict_class= round(nncompute$net.result)
  nn.error=abs(sum(true_class-predict_class))/length(predict_class) 
  plot(network)
#Fit separate singlehidden-layer neural networks  the learningset data with the outlier
  network=neuralnet(target~.,data=outlier_train,hidden=1,err.fct='ce', linear.output=FALSE,threshold=0.5)
  nncompute=compute(network,test[,1:13])
  true_class=test$target
  predict_class= round(nncompute$net.result)
  nn.error=abs(sum(true_class-predict_class))/length(predict_class) 
    plot(network)
###########################cv
# cv
cv.error=c()
cv.func=function(heart,hidden_1=c(5)){ 
   for(i in 1:5)  {
         test_indis= sample(1:nrow(heart), 0.75*nrow(heart)) # Divide data into train and test set
         test= heart[test_indis,]
         train= heart[-test_indis,]
         # nn fit 
           network=neuralnet(target~.,data=train,hidden=hidden_1,err.fct='ce', linear.output=FALSE,threshold=0.5) 
           predict.network=compute(network,test[,1:13])
           #predict nn
             class=train$target
             predict=round(predict.network$net.result)
             #class
             cv.error[i]=sum((class - predict)/nrow(predict))
             }
    return(mean(cv.error))}
#Determine error for nn usin cv
set.seed(123)
error_train <-c()
error_test <-c()
for(i in 1:5){network<- neuralnet(target~.,data=train,hidden=c(i),err.fct='ce', linear.output=FALSE,threshold=0.5)
error_train[i]=sum(((round(network$net.result[[1]])-(train$target))^2)/nrow(heart))
error_test[i]=cv.func(heart,hidden=c(i))}

UB = max(abs(error_train), error_train)
LB = min(error_test, error_train)
plot(abs(error_train), type = "o", lty = 2, col = "red", xlab = "Number of neurons used in the layer", ylab = "Errors", ylim = c(LB -1, UB +1),main = "Errors for the number of neurons in the layer")
lines(abs(error_test), type = "o", lty = 1, col = "blue")
legend("topright", c("training", "test"), lty = c(2,1), col = c("red","blue"))

########
heart <- read.csv("heart.csv")
#Divide train and test
set.seed(1234)
index = sample(1:nrow(heart), 0.75*nrow(heart))
train = heart[index,]
test = heart[-index,]

network=neuralnet(target~.,data=train,hidden=3,err.fct='ce', linear.output=FALSE,threshold=0.5)
nncompute=compute(network,test[,1:13])
true_class=test$target
predict_class= round(nncompute$net.result)
nn.error=abs(sum(true_class-predict_class))/length(predict_class) 
nn.error 

outlier_train=heart[index,];outlier_train[1,"trestbps"]=180 

network=neuralnet(target~.,data=outlier_train,hidden=3,err.fct='ce', linear.output=FALSE,threshold=0.5)
nncompute=compute(network,test[,1:13])
true_class=test$target
predict_class= round(nncompute$net.result)
nn.error1=abs(sum(true_class-predict_class))/length(predict_class) 
nn.error1 

outlier_train2=heart[index,];outlier_train2[1,"trestbps"]=400

network=neuralnet(target~.,data=outlier_train2,hidden=3,err.fct='ce', linear.output=FALSE,threshold=0.5)
nncompute=compute(network,test[,1:13])
true_class=test$target
predict_class= round(nncompute$net.result)
nn.error2=abs(sum(true_class-predict_class))/length(predict_class) 
nn.error2
