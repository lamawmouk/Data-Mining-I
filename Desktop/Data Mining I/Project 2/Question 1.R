# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW2")
#Importing the datafile and activating the package
library(ISLR) #install.packages("ISLR")
data("College")
college=College
rm(College)
#check for null data
sum(is.na(college))
#Set seed for reproduce
set.seed(1)
# features convert them into categorical features. 
college$Private = as.numeric((college$Private))
#Split the data set into a training set and a test set
subsetting= sample(1:nrow(college),size=0.30*nrow(college))
collegetrain=college[-subsetting,]
collegetrain=data.frame(collegetrain)
collegetest= college[subsetting,]
collegetest=data.frame(collegetest)
###################################################
#Fit the linear model for the training set
lm=lm(Apps~., data=collegetrain)
summary(lm)
lmpred= predict(lm,collegetest)
y_hat=lmpred
y_true=collegetest$Apps
lm_testerror= mean((y_true-y_hat)^2);
lm_testerror 

###################################################
# library for ridge regression
library(glmnet) #install.packages("glmnet")
#Create a matrix
train.matrix=as.matrix(collegetrain[,-2])
test.matrix=as.matrix(collegetest[,-2])
ytrain=collegetrain$Apps
# Creating a ridge regression model for training data
ridge.mod=glmnet(train.matrix, ytrain, alpha=0)
summary(ridge.mod)
#Performing cross validation 
set.seed(12345)
cv.ridge= cv.glmnet(train.matrix,ytrain,alpha=0 )
x11()
plot(cv.ridge)
# finding lamdma min which results in mini cv error on training data
bestlam_ridge= cv.ridge$lambda.min
bestlam_ridge
#prediction for the test data
ridgepred= predict(ridge.mod, s= bestlam_ridge, newx= test.matrix)
#test_Error
y_testhat = ridgepred
y_testtrue = collegetest$Apps
ridge_testerror=mean((y_testhat - y_testtrue)^2) 
ridge_testerror
###################################################
set.seed (123)
# Create a matrix for training and testing data
train.matrix=as.matrix(collegetrain[,-2])
test.matrix=as.matrix(collegetest[,-2])
ytrain=collegetrain$Apps
# Creating a lasso model for training data
lasso_model= glmnet( train.matrix,ytrain,alpha=1)
summary(lasso_model)
#Performing cross validation on model
cv.lasso=cv.glmnet (train.matrix,ytrain,alpha =1)
x11()
plot(cv.lasso)
#finding the lambda for mini cv error on training dataset
bestlam_lasso=cv.lasso$lambda.min
bestlam_lasso
#Fitting training model on test set
lassopred=predict(lasso_model,s=bestlam_lasso ,newx=test.matrix)
predict(lasso_model, s=bestlam_lasso, type="coefficients")
#test_Error
y_testhat = lassopred
y_testtrue = collegetest$Apps
lasso_testerror=mean((y_testhat - y_testtrue)^2) 
lasso_testerror
#Finding the non zero coefficienct estimates
lassocoefficients=predict(lasso_model, s = bestlam_lasso, type = "coefficients")[1:length(lasso_model),]
lassocoefficients[lassocoefficients!=0]
###################################################
#install.packages("pls")
library(pls)
#set the seed since nede to do crossvalidation
set.seed(12345)
pcr_model=pcr(Apps~.,data=collegetrain,scale=TRUE,validation="CV")
summary(pcr_model)
#Plotting Mean square error for the different number of components to look for the minimum compenent
x11()
validationplot(pcr_model,val.type="MSEP")
#Fitting training model on test set
pcrpred= predict(pcr_model,collegetest,ncomp=17)
#test_Error
y_hat = pcrpred
y_true = collegetest$Apps
pcr_testerror=mean((y_hat - y_true)^2) 
pcr_testerror
###################################################
pls_model= plsr(Apps ~ ., data = collegetrain, scale = TRUE, validation = "CV")
summary(pls_model)
#Fitting training model on test set
x11()
validationplot(pls_model, val.type = "MSEP")
plspred= predict(pls_model, collegetest, ncomp = 10)
#test_Error
y_hat = plspred
y_true = collegetest$Apps
pls_testerror=mean((y_hat - y_true)^2) 
pls_testerror
###################################################
#Compare the test errors
testerror=c(lm_testerror, ridge_testerror, lasso_testerror, pcr_testerror, pls_testerror)
names(testerror)=c("lm", "ridge", "lasso", "pcr", "pls")
barplot(testerror)
#Determine Accuracy
y_true = collegetest$Apps
averagetest = mean(y_true)
#Linear model R^2 value
lm.r2= 1-mean((lmpred-y_true)^2)/mean((averagetest-y_true)^2) ;lm.r2
#Ridge model R^2 value
ridge.r2=1-mean((ridgepred-y_true)^2)/mean((averagetest-y_true)^2) ;ridge.r2
#Lasso model R^2 value
lasso.r2= 1-mean((lassopred-y_true)^2)/mean((averagetest-y_true)^2) ;lasso.r2
#PCR model R^2 value
pcr.r2=1-mean((pcrpred-y_true)^2)/mean((averagetest-y_true)^2) ;pcr.r2
#PLS model R^2 value
pls.r2= 1-mean((plspred-y_true)^2)/mean((averagetest-y_true)^2) ; pls.r2
barplot(c(lm.r2, ridge.r2, lasso.r2, pcr.r2, pls.r2), col="gray", ylim=c(0,1), names.arg=c("Lm", "Ridge", "Lasso", "PCR", "PLS"), xlab="Model Type",ylab = "R-squared test error", main="R-squared test error for different models")

