# Delete all the objects in the workspace 
rm(list=ls())
#set directory
# install.packages("ElemStatLearn")
library("ElemStatLearn")
ls("package:ElemStatLearn")        
?zip.train
?zip.test

#######################################################################################
#Linear model regression  
######################################################################################
#load the zip data
data(zip.train)
data(zip.test)
#To ensure consistent results
set.seed(1)
#subset selection of training data by taking out digits 2 and 3 from the dataset
y_train=zip.train
my2_train=which(y_train==2)
my3_train=which(y_train==3)
new_train=y_train[c(my2_train,my3_train),]

#subset selection of testing data by taking out digits 2 and 3 from the dataset
y_test=zip.test
my2_test=which(y_test==2)
my3_test=which(y_test==3)
new_test=y_test[c(my2_test,my3_test),]
##Build linear regression model for training data and testing data
fittrain=as.data.frame(new_train)
fittest=as.data.frame(new_test)
fitln=lm(V1~., data=fittrain)
#Predict the outcomes based on your training sample and testing sample
predicttrain=predict(fitln,fittrain)
predicttest=predict(fitln,fittest)
predicttest[predicttest<=2.5]= 2;predicttrain[predicttrain<=2.5]= 2
predicttest[predicttest>2.5] = 3;predicttrain[predicttrain>2.5]=3
#install.packages("mclust")
library("mclust")
k=c(1,3,5,7,9,11,13,15)
lm_trainingerror= sapply(k, function(k){classError(predicttrain,new_train[,1])$errorRate})
lm_testerror <- sapply(k, function(k){classError(predicttest, new_test[,1])$errorRate})
lm_trainingerror
lm_testerror 
#######################################################################################
#KNN classification
######################################################################################
data(zip.train)
data(zip.test)
#To ensure consistent results
set.seed(1)
#subset selection of training data by taking out digits 2 and 3 from the dataset
y_train=zip.train
my2_train=which(y_train==2)
my3_train=which(y_train==3)
new_train=y_train[c(my2_train,my3_train),]

#subset selection of testing data by taking out digits 2 and 3 from the dataset
y_test=zip.test
my2_test=which(y_test==2)
my3_test=which(y_test==3)
new_test=y_test[c(my2_test,my3_test),]
#Making predicted labels for traning and testing data set

new_train[,1] = as.factor(new_train[,1])
new_test[,1] = as.factor(new_test[,1])

#Creating kNN classification model to determine the training error 
require(class)
train_true=new_train[,1]
k=c(1,3,5,7,9,11,13,15)
error_store_train=c()
for(i in 1:8){
  knn_train_predict=knn(new_train[,2:257],new_train[,2:257],new_train[,1],k[i])
  knn_train_predict<-as.numeric(knn_train_predict) 
  knn_train_true <-as.numeric(train_true) 
  err=(1/length(new_train[,1]))*sum((knn_train_predict-knn_train_true)^2)
  error_store_train=c(error_store_train,err)
}
#Output the knn traning rate error
error_store_train

#Creating kNN classification model to determine the testing error 
require(class)
test_true=new_test[,1]
k=c(1,3,5,7,9,11,13,15)
error_store_test=c()
for(i in 1:8){
  knn_test_predict=knn(new_train[,2:257],new_test[,2:257],new_train[,1],k[i])
  knn_test_predict<-as.numeric(knn_test_predict) 
  knn_test_true <-as.numeric(test_true) 
  err=(1/length(new_test[,1]))*sum((knn_test_predict-knn_test_true)^2)
  error_store_test=c(error_store_test,err)
}
#Output the knn testing rate error
error_store_test

#Plot
x11()
plot(k, error_store_test, type="o", col="blue", pch="o", lty=1, ylim=c(0,0.05), ylab="Error" )
points(k, error_store_train, col="red", pch="o")
lines(k, error_store_train, col="red",lty=2)

points(k, lm_trainingerror, col="orange",pch="+")
lines(k, lm_trainingerror, col=" orange", lty=3)
points(k, lm_testerror, col="green",pch="+")
lines(k, lm_testerror, col="green", lty=3)

legend(7,0.03,legend=c("knn test error","knn train error","linear model training error","linear model testing error"), col=c("blue","red","orange","green"),
       pch=c("o","o","+","+"),lty=c(1,2,3,4), ncol=1)


