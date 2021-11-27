# Delete all the objects in the workspace 
rm(list=ls())
#set working directory
setwd("C:/Users/lama_/Desktop/Data mining/HW4_Datamining")
#######Load the data#######
library(ElemStatLearn)
data(prostate)
#remove the training data column
prostate = prostate[,-c(10)]

#divide train and test. 
set.seed(1234)
index = sample(1:nrow(prostate), 0.80*nrow(prostate))
X.train = prostate[index,]
X.test = prostate[-index,]
Y.train = prostate$lpsa[index]
Y.test = prostate$lpsa[-index]
names(prostate) # there are 8 predictors with the lspa being the response
######## All possible subsets 
library(leaps)
bestsubset=regsubsets(lpsa~., data = X.train, nbest = 1, nvmax = 8, method = "exhaustive")# see the full model
my_sum=summary(bestsubset) # * means the varibale is  included in the active set
my_sum # see all possible subsets of the variables- we have 8 predictors- tells the best 2 vairbale model are lcavol and lweight predictors
names(my_sum) # cp, bic (bayseian info criteria) are used for  model bestvariableion
my_sum$cp;min(my_sum$cp)# want the min
my_sum$bic;min(my_sum$bic) # want the  mins since it is the best one- in this case it is the most negative

# They both agree the optimum model is with 3 variables
which(my_sum$cp == min(my_sum$cp)) #cp: 3  # more black there is-means the predictor is needed for the model
which.min(my_sum$bic) #bic: 3 
#lcavol  lweight svi are the best 3 variable subset. 
which(my_sum$outmat[3,] == "*")

# Do the selection based on the "hold out method"
library(stats)

select = my_sum$outmat # outmat tells you the best # vairable model 
training.error.store<-c()
test.error.store<-c()
aic.store<-c()
bic.store<-c()

for(i in 1:8){
  temp<-which(select[i,]=="*") #Index is correct 
  
  red.training<-X.train[,c(9,temp)] # keep the repsonse varibale and the important variable
  red.test<-X.test[,c(9,temp)]
  
  red.fit<-lm(lpsa~.,data=red.training)  # reduced fit is a linear model and use the reduced training set
  aic<-AIC(red.fit)
  bic<-BIC(red.fit)
  
  predict.training<-predict(red.fit,newdata=red.training)
  predict.test<-predict(red.fit,newdata=red.test)
  training.error<-(1/length(Y.train))*sum((predict.training-Y.train)^2)
  test.error<-(1/length(Y.test))*sum((predict.test-Y.test)^2)
  training.error.store<-c(training.error.store,training.error)
  test.error.store<-c(test.error.store,test.error)
  aic.store<-c(aic.store,aic)
  bic.store<-c(bic.store,bic)
}
training.error.store
test.error.store
aic.store
bic.store

### Plot the result
#par(mar=c(1,1,1,1))
upper = max(training.error.store, test.error.store)
lower = min(training.error.store, test.error.store)
x11()
plot(training.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "Predictor number", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

upper= max(aic.store,bic.store)
lower= min(aic.store,bic.store)
plot(aic.store,type="o",lty=2,col = "blue",ylim = c(lower-5,upper+5),xlab = "Predictor number",main="AIC & BIC",ylab="Value")
lines(bic.store,type="o",lty=1,col="red")
legend("topright",c("AIC", "BIC"),lty=c(2,1),col=c("blue","red"))

# create functions that feed into "bootpred"
theta.fit <- function(X,Y){lsfit(X,Y)}
theta.predict <- function(fit, X){cbind(1,X)%*%fit$coef}
sq.error <- function(Y,Yhat){(Y-Yhat)^2}

# bootstrap.632
#install.packages("bootstrap")
library(bootstrap)
# Create X and Y
X<-prostate[,1:8]
Y<-prostate[,9]

# Generalize it, and search over the best possible subsets of size "k"
bootstrap.error.store = c()
for (i in 1:8){
  # Pull out the model
  temp = which(my_sum$outmat[i,] == "*") # Best num variable model/see the number of important varibale 
  res = bootpred(X[,temp], Y, nboot = 500, theta.fit = theta.fit, theta.predict = theta.predict, err.meas = sq.error) 
  bootstrap.error.store = c(bootstrap.error.store, res[[3]]) # interested in res 3 as it is 0.632
  
}

bootstrap.error.store
x11()
#Plot the results of test train and boostrap error
plot(training.error.store, type = "o", lty = 2, col = "blue",  ylim = c(0, 1.5) ,xlab = "Predictor Number", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
lines(bootstrap.error.store, type = "o", lty = 3, col = "green")
legend("topright", c("training", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))

set.seed(123)
k=5
X.train=prostate
predict.regsubsets=function(object, newdata,id ,...){
  temp_X= cbind(rep(1,length(newdata[,1])),newdata)
colnames(temp_X)= c("(Intercept)",colnames(newdata))

coefi=coef(object,id=i)
my_pred=as.matrix(temp_X[,names(coefi)])%*%coefi

return(my_pred)
}
folds=sample(1:k,nrow(X.train),replace=TRUE)
cv.errors=matrix(NA,k,8,dimnames=list(NULL,paste(1:8)))

for(j in 1:k){
  best.fit=regsubsets(lpsa~., data=X.train[folds!=j,],nvmax=8)
  for(i in 1:8){
    pred=predict(best.fit,newdata=X.train[folds==j,], id=i)
    cv.errors[j,i]=mean((X.train$lpsa[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors


set.seed(123)
k=10
X.train=prostate
predict.regsubsets=function(object, newdata,id ,...){
  temp_X= cbind(rep(1,length(newdata[,1])),newdata)
  colnames(temp_X)= c("(Intercept)",colnames(newdata))
  
  coefi=coef(object,id=i)
  my_pred=as.matrix(temp_X[,names(coefi)])%*%coefi
  
  return(my_pred)
}
folds=sample(1:k,nrow(X.train),replace=TRUE)
cv.errors=matrix(NA,k,8,dimnames=list(NULL,paste(1:8)))

for(j in 1:k){
  best.fit=regsubsets(lpsa~., data=X.train[folds!=j,],nvmax=8)
  for(i in 1:8){
    pred=predict(best.fit,newdata=X.train[folds==j,], id=i)
    cv.errors[j,i]=mean((X.train$lpsa[folds==j]-pred)^2)
  }
}

mean.cv.errors2=apply(cv.errors,2,mean)
mean.cv.errors2
#Plot the results 5 and 10 CV
x11()
plot(mean.cv.errors, type = "o", lty = 2, col = "blue",  ylim = c(0, 1.5) ,xlab = "Predictor Number", ylab = "error", main = "Model Selection")
lines(mean.cv.errors2, type = "o", lty = 1, col = "red")
legend("topright", c("five-fold cross-validation"," ten-fold cross-validation"), lty = c(2,1), col = c("blue", "red", "green"))

