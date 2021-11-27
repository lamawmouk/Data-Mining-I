# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/HW1_DataMining")
#activating the package
library(MASS)
#Seeing the dimensions and part of data set
data(Boston)
dim(Boston)
names(Boston)
head(Boston)
#Convert some features to numeric
Boston$chas=as.numeric(Boston$chas)
Boston$rad =as.numeric(Boston$rad)
#Creating a pairs plot for Boston dataset
x11()
pairs(Boston)
#Seeing the correlation coefficents for crime rate
Martix<-cor(Boston)
head(round(Martix,2))
#Viewing the correlation matrix for crime rate
CM<-cor(Boston)
head(round(CM,2))
install.packages('corrplot')
library(corrplot)
x11()
corrplot(CM, method="number")
#Boxplots for crime rate, tax rate and pupil-teacher ratio
x11()
par(mfrow=c(1,3))
boxplot(Boston$crim,xlab="crim",main="Boxplot: Crime rate")
boxplot(Boston$tax,xlab="tax",main="Boxplot: Tax")
boxplot(Boston$ptratio,xlab="ptratio",main="Boxplot: Pupil-teacher ratio")
# Summary and range for crime rate, tax rate and pupil-teacher ratio
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)
range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)
x = boxplot.stats(Boston$crim)$out;x
x = boxplot.stats(Boston$ptratio)$out;x
#suburbs average more than seven rooms per dwelling and more than eight rooms per dwelling
length(Boston$rm[Boston$rm>7]) #64
length(Boston$rm[Boston$rm>8])#13
#Analysing the more than eight rooms per dwelling data set
summary(subset(Boston, rm > 8))
summary(subset(Boston, rm <=8 ))
Boston$rm
