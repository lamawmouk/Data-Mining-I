# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW2")
#Create file names
traindata=read.delim("ticdata2000.txt", sep = "\t",  header=FALSE)
#Binding the testing data
missingtestingdata =read.delim("ticeval2000.txt",sep = "\t", header=FALSE)
df1=data.frame(missingtestingdata)
testingreponsedata= read.table("tictgts2000.txt", sep = "\t", header=FALSE)
df2=data.frame(testingreponsedata)
testdata=cbind(df1,df2)
rm(df1); rm(df2);rm(missingtestingdata); rm(testingreponsedata)
#Naming the columns
names(traindata)=c('MOSTYPE','MAANTHUI','MGEMOMV','MGEMLEEF','MOSHOOFD','MGODRK','MGODPR','MGODOV','MGODGE','MRELGE','MRELSA','MRELOV','MFALLEEN','MFGEKIND','MFWEKIND','MOPLHOOG','MOPLMIDD','MOPLLAAG','MBERHOOG','MBERZELF','MBERBOER','MBERMIDD','MBERARBG','MBERARBO','MSKA','MSKB1','MSKB2','MSKC','MSKD','MHHUUR','MHKOOP','MAUT1','MAUT2','MAUT0','MZFONDS','MZPART','MINKM30','MINK3045','MINK45455','MINK45512','MINK123M','MINKGEM','MKOOPKLA','PWAPART','PWABEDR','PWALAND','PPERSAUT','PBESAUT','PMOTSCO','PVRAAUT','PAANHANG','PTRACTOR','PWERKT','PBROM','PLEVEN','PPERSONG','PGEZONG','PWAOREG','PBRAND','PZEILPL','PPLEZIER','PFIETS','PINBOED','PBYSTAND','AWAPART','AWABEDR','AWALAND','APERSAUT','ABESAUT','AMOTSCO','AVRAAUT','AAANHANG','ATRACTOR','AWERKT','ABROM','ALEVEN','APERSONG','AGEZONG','AWAOREG','ABRAND','AZEILPL','APLEZIER','AFIETS','AINBOED','ABYSTAND','CARAVAN')
names(testdata)= c('MOSTYPE','MAANTHUI','MGEMOMV','MGEMLEEF','MOSHOOFD','MGODRK','MGODPR','MGODOV','MGODGE','MRELGE','MRELSA','MRELOV','MFALLEEN','MFGEKIND','MFWEKIND','MOPLHOOG','MOPLMIDD','MOPLLAAG','MBERHOOG','MBERZELF','MBERBOER','MBERMIDD','MBERARBG','MBERARBO','MSKA','MSKB1','MSKB2','MSKC','MSKD','MHHUUR','MHKOOP','MAUT1','MAUT2','MAUT0','MZFONDS','MZPART','MINKM30','MINK3045','MINK45455','MINK45512','MINK123M','MINKGEM','MKOOPKLA','PWAPART','PWABEDR','PWALAND','PPERSAUT','PBESAUT','PMOTSCO','PVRAAUT','PAANHANG','PTRACTOR','PWERKT','PBROM','PLEVEN','PPERSONG','PGEZONG','PWAOREG','PBRAND','PZEILPL','PPLEZIER','PFIETS','PINBOED','PBYSTAND','AWAPART','AWABEDR','AWALAND','APERSAUT','ABESAUT','AMOTSCO','AVRAAUT','AAANHANG','ATRACTOR','AWERKT','ABROM','ALEVEN','APERSONG','AGEZONG','AWAOREG','ABRAND','AZEILPL','APLEZIER','AFIETS','AINBOED','ABYSTAND','CARAVAN')

###########################################################################################################

#Fitting the linear model(OLS)
lm.fit=lm(CARAVAN~.,data=traindata)
summary(lm.fit)
#Training MSE
lmpred.train= predict(lm.fit,traindata)
lm_trainerror= mean((traindata$CARAVAN-lmpred.train)^2);lm_trainerror
#Testing MSE
lmpred.test= predict(lm.fit,testdata)
lm_testerror= mean((testdata$CARAVAN-lmpred.test)^2);lm_testerror
#Determining the prediction from confusion matrix
#install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
#Fitting the linear model(OLS)
lm.fit=lm(CARAVAN~.,data=traindata)
#Predicting  on testdata
p= predict(lm.fit,testdata)
summary(p)
#Use 50% as cutoff for probability
p_class=ifelse(p>0.25,"1","0");table(p_class)
#Creating a freq table 0=false negative and false positve
table(p_class, testdata$CARAVAN)
prediction=factor((p_class), c(0, 1), labels = c("Not Purchased", "Purchased"))
Actual.Outcome=factor((testdata$CARAVAN), c(0, 1), labels = c("Not Purchased", "Purchased"))
confusionMatrix(prediction,Actual.Outcome, positive =  "Purchased")
testprec= posPredValue(prediction, Actual.Outcome, positive="Purchased"); testprec
testrecall=sensitivity(prediction, Actual.Outcome, positive="Purchased");testrecall
testf1=(2 * testprec * testrecall / (testprec+ testrecall)); testf1
###########################################################################################################
#Fitting the forward model
library(leaps)
reg.fwdfit =regsubsets(CARAVAN~., data=traindata, method = "forward",nvmax = 45)
sum.caravan=summary(reg.fwdfit) # look at summary of the output
sum.caravan$outmat
coef(reg.fwdfit,id=45)
#Make a model matrix from the training data to build a matrix from the data 
x_train= model.matrix(CARAVAN ~ ., data = traindata, nvmax = 45)
x_test= model.matrix(CARAVAN ~ ., data = testdata, nvmax = 45)
train_error= rep(NA, 45) # for collecting the training error
test_error= rep(NA, 45) # for collecting the testing error
for (i in 1:45) {
  coefficients= coef(reg.fwdfit, id = i) #Extract the coefficients from the forward model 
  trainpred=x_train[, names(coefficients)] %*% coefficients #To create prediction mutlipy the columns with the corresponding coefficients
  testpred=x_test[, names(coefficients)] %*% coefficients
  train_error[i]=mean((trainpred-traindata$CARAVAN)^2)
  test_error[i]=mean((testpred-testdata$CARAVAN)^2)
}
#Plot the train and the test set MSE associated withthe best model of each size
plot(train_error, col="red",  ylim=c(0.051,0.057),type="b",,ylab="MSE", xlab="Number of predictors", main="Training and Testing MSE for forward model" )
lines(test_error, col="blue", type="b")
legend("topright", legend=c("train error"," test error"), col=c("red","blue"),lty=c(1,1), lwd=c(2.5,2.5))


#Determining the prediction from confusion matrix
reg.fwdfit =regsubsets(CARAVAN~., data=traindata, method = "forward",nvmax = 45)
#Make a model matrix from the training data to build a matrix from the data 
x_test= model.matrix(CARAVAN ~ ., data = testdata, nvmax = 45)
testprec= c()
testrecall= c()
testf1= c()
for (i in 1:45) {
  coefficients= coef(reg.fwdfit, id = i) #Extract the coefficients from the forward model 
  p=x_test[, names(coefficients)] %*% coefficients
  p_class=ifelse(p>0.25,"1","0")
  prediction=factor((p_class), c(0, 1), labels = c("Not Purchased", "Purchased"))
  Actual.Outcome=factor((testdata$CARAVAN), c(0, 1), labels = c("Not Purchased", "Purchased"))
  confusionMatrix(prediction,Actual.Outcome, positive =  "Purchased")
  testprec[i]= posPredValue(prediction, Actual.Outcome, positive="Purchased")
  testrecall[i]=sensitivity(prediction, Actual.Outcome, positive="Purchased")
  testf1[i]= (2 * testprec[i] * testrecall[i]) / (testprec[i] + testrecall[i])
  }
plot(testprec, col="black",type="l",,ylab="test precision", xlab="Number of predictors", main="Forward model" )
plot(testrecall, col="black",type="l",,ylab="test recall", xlab="Number of predictors", main="Forward model" )
plot(testf1, col="black",type="l",,ylab="test f1", xlab="Number of predictors", main="Forward model" )


###########################################################################################################
#Fitting the backward model
library(leaps)
reg.bwdfit =regsubsets(CARAVAN~., data=traindata, method = "backward",nvmax = 45)
sum.caravan=summary(reg.bwdfit) # look at summary of the output
sum.caravan$outmat
coef(reg.bwdfit,id=45)
x_train= model.matrix(CARAVAN ~ ., data = traindata, nvmax = 45)
x_test= model.matrix(CARAVAN ~ ., data = testdata, nvmax = 45)
train_error= rep(NA, 45) # for collecting the training error
test_error= rep(NA, 45) # for collecting the testing error
for (i in 1:45) {
  coefficients= coef(reg.bwdfit, id = i) #Extract the coefficients from the forward model 
  #Make a model matrix from the training data to build a matrix from the data 
  #To create prediction mutlipy the columns with the corresponding coefficients
  trainpred=(x_train[, names(coefficients)]) %*% coefficients 
  testpred=(x_test[, names(coefficients)]) %*% coefficients
  train_error[i]=mean((trainpred-traindata$CARAVAN)^2)
  test_error[i]=mean((testpred-testdata$CARAVAN)^2)
}
#Plot the train and the test set MSE associated withthe best model of each size
plot(train_error, col="red", type="b",ylim=c(0.052,0.056), ylab="MSE", xlab="Number of predictors", main="Training and Testing MSE for backward model" )
lines(test_error, col="blue", type="b")
legend("topright", legend=c("train error"," test error"), col=c("red","blue"),lty=c(1,1), lwd=c(2.5,2.5))

#Determining the prediction from confusion matrix
reg.bwdfit =regsubsets(CARAVAN~., data=traindata, method = "backward",nvmax = 45)
#Make a model matrix from the training data to build a matrix from the data 
x_test= model.matrix(CARAVAN ~ ., data = testdata, nvmax = 45)
testprec= c()
testrecall= c()
testf1= c()
for (i in 1:45) {
  coefficients= coef(reg.bwdfit, id = i) #Extract the coefficients from the forward model 
  p=x_test[, names(coefficients)] %*% coefficients
  p_class=ifelse(p>0.25,"1","0")
  prediction=factor((p_class), c(0, 1), labels = c("Not Purchased", "Purchased"))
  Actual.Outcome=factor((testdata$CARAVAN), c(0, 1), labels = c("Not Purchased", "Purchased"))
  confusionMatrix(prediction,Actual.Outcome, positive =  "Purchased")
  testprec[i]= posPredValue(prediction, Actual.Outcome, positive="Purchased")
  testrecall[i]=sensitivity(prediction, Actual.Outcome, positive="Purchased")
  testf1[i]= (2 * testprec[i] * testrecall[i]) / (testprec[i] + testrecall[i])
}
plot(testprec, col="black",type="l",,ylab="test precision", xlab="Number of predictors", main="Backward model" )
plot(testrecall, col="black",type="l",,ylab="test recall", xlab="Number of predictors", main="Backward model" )
plot(testf1, col="black",type="l",,ylab="test f1", xlab="Number of predictors", main="Backward model" )
###########################################################################################################
# library for ridge regression
library(glmnet) #install.packages("glmnet")
#Create a matrix
train.matrix=as.matrix(traindata[,1:85])
test.matrix=as.matrix(testdata[,1:85])
ytrain=traindata[,86]
# Creating a ridge regression model for training data
ridge.mod=glmnet(train.matrix, ytrain, alpha=0)
#Performing cross validation 
set.seed(12345)
cv.ridge= cv.glmnet(train.matrix,ytrain,alpha=0 )
plot(cv.ridge)
# finding lamdma min which results in mini cv error on training data
bestlam_ridge= cv.ridge$lambda.min
bestlam_ridge
#prediction for the test data
ridgepred= predict(ridge.mod, s= bestlam_ridge, newx= test.matrix)
#test_Error
y_testhat = ridgepred
y_testtrue = testdata$CARAVAN
ridge_testerror=mean((y_testhat -y_testtrue)^2) 
ridge_testerror
#Determining the prediction from confusion matrix
#install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
#Fitting the linear model(OLS)
ridge.mod=glmnet(train.matrix, ytrain, alpha=0)
#Predicting  on testdata
p= predict(ridge.mod, s= bestlam_ridge, newx= test.matrix)
summary(p)
#Use 50% as cutoff for probability
p_class=ifelse(p>0.25,"1","0");table(p_class)
#Creating a freq table 0=false negative and false positve
table(p_class, testdata$CARAVAN)
prediction=factor((p_class), c(0, 1), labels = c("Not Purchased", "Purchased"))
Actual.Outcome=factor((testdata$CARAVAN), c(0, 1), labels = c("Not Purchased", "Purchased"))
confusionMatrix(prediction,Actual.Outcome, positive =  "Purchased")
testprec= posPredValue(prediction, Actual.Outcome, positive="Purchased"); testprec
testrecall=sensitivity(prediction, Actual.Outcome, positive="Purchased");testrecall
testf1=(2 * testprec * testrecall / (testprec+ testrecall)); testf1
###########################################################################################################
# Creating a lasso model for training data
lasso_model= glmnet( train.matrix,ytrain,alpha=1)
#Performing cross validation on model
cv.lasso=cv.glmnet (train.matrix,ytrain,alpha =1)
plot(cv.lasso)
#finding the lambda for mini cv error on training dataset
bestlam_lasso=cv.lasso$lambda.min
bestlam_lasso
#Fitting training model on test set
lassopred=predict(lasso_model,s=bestlam_lasso ,newx=test.matrix)
#test_Error
y_testhat = lassopred
y_testtrue = testdata$CARAVAN
lasso_testerror=mean((y_testhat - y_testtrue)^2) 
lasso_testerror
#Determining the prediction from confusion matrix
#install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
#Fitting the lasso model
lasso_model= glmnet( train.matrix,ytrain,alpha=1)
#Predicting  on testdata
p=predict(lasso_model,s=bestlam_lasso ,newx=test.matrix)
summary(p)
#Use 50% as cutoff for probability
p_class=ifelse(p>0.25,"1","0");table(p_class)
#Creating a freq table 0=false negative and false positve
table(p_class, testdata$CARAVAN)
prediction=factor((p_class), c(0, 1), labels = c("Not Purchased", "Purchased"))
Actual.Outcome=factor((testdata$CARAVAN), c(0, 1), labels = c("Not Purchased", "Purchased"))
confusionMatrix(prediction,Actual.Outcome, positive =  "Purchased")
testprec= posPredValue(prediction, Actual.Outcome, positive="Purchased"); testprec
testrecall=sensitivity(prediction, Actual.Outcome, positive="Purchased");testrecall
testf1=(2 * testprec * testrecall / (testprec+ testrecall)); testf1


