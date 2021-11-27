# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW3 Data Mining")
#Generate the45 dataset
set.seed(123)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
###########Question 2a)
#Compute the LOOCV errors
set.seed(12345)
dataset=data.frame(x, y)
library(boot)
set.seed(123)
#model 1
glm.fit1<-glm(y~x, data=dataset)
#summary(glm.fit) ?cv.glm
loocv.error=cv.glm(dataset, glm.fit1)
loocv.error$delta[1]
#model 2
glm.fit2<-glm(y~poly(x, 2), data=dataset)
loocv.error=cv.glm(dataset, glm.fit2)
loocv.error$delta[1]
#model 3
glm.fit3<-glm(y~poly(x, 3), data=dataset)
loocv.error<-cv.glm(dataset, glm.fit3)
loocv.error$delta[1]
#model 4
glm.fit4<-glm(y~poly(x, 4), data=dataset)
loocv.error<-cv.glm(dataset, glm.fit4)
loocv.error$delta[1]
x11()
plot(x,y,main= 'Relationship between x and y' )

###########Question 2c)
#Comment on the statistical significance of the coefficient estimates that results from fitting each of the models in (c) using least squares. 
#Do these results agree with the conclusions drawn based on cross-validation results?
lm.fit1<-lm(y~x, data=dataset)
summary(lm.fit1) 
lm.fit2<-lm(y~poly(x, 2), data=dataset)
summary(lm.fit2) 
lm.fit3<-lm(y~poly(x, 3), data=dataset)
summary(lm.fit3) 
lm.fit4<-lm(y~poly(x, 4), data=dataset)
summary(lm.fit4)
