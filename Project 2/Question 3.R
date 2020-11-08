# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW2")
#Create an empty dataset 
set.seed(1)
datasetmatrix= matrix(nrow=1000,ncol=20)
#Fill the empty dataset with random numbers
for (i in 1:20) {
  datasetmatrix[,i]=rnorm(1000)}
x=datasetmatrix
rm(datasetmatrix)
#Create random numbers for epsilon
epsilon= rnorm(1000)
#Create β where some elements that are exactly equal to zero
beta=rnorm(20)
beta_zeroelements=c(2,3,4,7,8,11,12,18)
beta[beta_zeroelements]=0  
#Create response variable y with x matrix, beta, epislon
y=x %*% beta + epsilon
data = data.frame(x,y)
# Split  data set into a training set containing 100 observations and a test set containing 900 observations
observation_training_set = 100
observation_testdata = 900
total_observations=1000
index=sample(1:nrow(x), size=0.1*nrow(x))
traindata =data[index, ] 
testdata =data[-index, ] 

###################################################################
#Perform best subset selection on the training set
library(leaps)#install.packages("leaps")
exhaustivefit= regsubsets( y ~., data=traindata, nvmax = 20)
#Determine the training set MSE associated with the best model of each size
x_train= model.matrix(y ~ ., data = traindata, nvmax = 20) #Make a model matrix from the training data to build a matrix from the data
train_error= rep(NA, 20) # for collecting the training error
for (i in 1:20) {
  #Extract the coefficients from the best model 
  coefficients= coef(exhaustivefit, id = i)
  # To create prediction mutlipy the columns with the corresponding coefficients
  trainpred=x_train[, names(coefficients)] %*% coefficients
  train_error[i]=mean((trainpred-traindata$y)^2)}
#Determine test set MSE associated withthe best model of each size
x_test= model.matrix(y ~ ., data = testdata, nvmax = 20)
test_error= rep(NA, 20)
for (i in 1:20) {
  #Extract the coefficients from the best model 
  coefficients= coef(exhaustivefit, id = i)
  # To create prediction mutlipy the columns with the corresponding coefficients
  testpred=x_test[, names(coefficients)] %*% coefficients
  test_error[i]=mean((testpred-testdata$y)^2)
}
#Plot the train and the test set MSE associated withthe best model of each size
plot(train_error, type="o", col="blue", pch=19, lty=2, ylim=c(0,14),ylab="MSE", xlab="Number of predictors", main="Training and Testing MSE for best model" )
points(test_error, col="red", pch=19)
lines(test_error, col="red",lty=2)
legend(14,8, legend=c("train error"," test error"), col=c("blue","red"),pch=c(19,19),lty=c(2), ncol=1)

#For which model size does the test set MSE take on its minimum value
minipredictors=which.min(test_error)  
minipredictors
#Comment on the coefficient values
coef(exhaustivefit, minipredictors)

