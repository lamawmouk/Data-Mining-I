# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW3 Data Mining")
#Opening the data set
diabetesdata=read.table("C:/Users/lama_/Desktop/HW3 Data Mining/DiabetesAndrews36_1.txt", quote="\"", comment.char="")

#Delete multiple columns 1 to 3 by mapping Null value to them
diabetesdata$V1 <- diabetesdata$V2 <- diabetesdata$V3 <- NULL
names(diabetesdata) <- c("observation","glucose.area", "insulin.area","SSPG","relative.weight","fasting.plasma.glucose","class")

# Question 2a)
#Produce pairwise scatterplots for five variables representing different classes
#install.packages("GGally")
library(GGally)
diabetesdata$class<-as.factor(diabetesdata$class)
x11()
ggpairs(diabetesdata,columns=c(2,3,4,5,6),aes(colour = class))
x11()
pairs(diabetesdata[2:6], main = "Diabetes Data- 3 classes",oma=c(4,4,6,10), pch = 21, bg = c("red", "green3", "blue")[(diabetesdata$class)])
par(xpd=TRUE)
legend(0.95,0.9, as.factor(unique(diabetesdata$class)), fill=c("red", "green3", "blue"))

# Question 2b)
# Create a test and training dataset
diabetesdata=diabetesdata[,-c(1)]

set.seed(12345)
train = sample(1:nrow(diabetesdata), nrow(diabetesdata)*.70)
diabetes_train = diabetesdata[train, ]
diabetes_test = diabetesdata[-train, ]
#Apply linear discriminant analysis (LDA)
library(MASS)
lda.fit= lda(class ~ . , data = (diabetes_train))
summary(lda.fit)
#Compute the training error
lda.pred.train=predict(lda.fit, newdata=diabetesdata)
names(lda.pred.train) #class is actual estimate and posterior probabilities
lda.pred.train
y_hat_train= as.numeric(unlist(lda.pred.train))
y_true=as.numeric(unlist(diabetesdata$class))
lda.trainerror= mean((y_true-y_hat_train)^2)
lda.trainerror #5.25189

#Apply quadratic discriminant analysis (QDA)
set.seed(12345)
qda.fit= qda(class ~., data = diabetesdata)
summary(qda.fit)
qda.pred.train = predict(qda.fit, newdata = diabetesdata)
lda.pred.train
y_hat_train= as.numeric(unlist(qda.pred.train))
y_true=as.numeric(unlist(diabetesdata$class))
qda.trainerror= mean((y_true-y_hat_train)^2)
qda.trainerror #3.549844 
# Question 2c)
glucose.area=c(0.98);insulin.area=c(122);SSPG=c(544);relative.weight=c(186);fasting.plasma.glucose=c(184)   
individ_data=data.frame(glucose.area,insulin.area,SSPG,relative.weight,fasting.plasma.glucose)
##########individ_data
lda_pred=predict(lda.fit,newdata=individ_data)
lda_pred
qda_pred=predict(qda.fit,newdata=individ_data)# prob of class
qda_pred
