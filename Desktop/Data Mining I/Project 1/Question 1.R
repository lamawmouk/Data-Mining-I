# Delete all the objects in the workspace 
rm(list=ls())

#set directory
setwd("C:/Users/lama_/Desktop/HW1_DataMining")
#load cereal data 
cereal <- read.csv("C:/Users/lama_/Desktop/HW1_DataMining/cereal.csv")

#visualize cereal data
dim(cereal)
str(cereal)
# Converting column variables from int in to numeric and in to factor
cereal$calories= as.numeric(cereal$calories)
cereal$protein= as.numeric(cereal$protein)
cereal$fat= as.numeric(cereal$fat)
cereal$sodium= as.numeric(cereal$sodium)
cereal$vitamins= as.factor(cereal$vitamins)
cereal$shelf= as.factor(cereal$shelf)
cereal$cups= as.factor(cereal$cup)

#Determining the variance of the columns with negative valuesn")
var(cereal$sugars)
var(cereal$potass)
var(cereal$carbo)

#Replace the negative values in the columns with the mean of the column
cereal[21,11]=mean(cereal$potass)
cereal[5,11]=mean(cereal$potass)
cereal[58,9]=mean(cereal$carbo)
cereal[58,10]=mean(cereal$sugars)

#Density plot of rating - Check if the response variable is close to normality
#install.packages("e1071")
library(e1071)
plot(density(cereal$rating), main="Density Plot: Rating", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cereal$rating), 2)))  
polygon(density(cereal$rating), col="white")   

#Histrogram-distrubtion of rating among the different cereals
library(ggplot2)
ggplot(cereal,aes(carbo))+geom_histogram(binwidth=5,color="black",fill="white")+
  ggtitle("Distribution of carbohydrate")+
  ylab("Frequency")+xlab("Grams of complex carbs")+
  xlim(0,30)+ylim(0,50)


# To find outliers 
#fat  
x = boxplot.stats(cereal$fat)$out;x   #No outliers
#sodium 
x = boxplot.stats(cereal$sodium)$out;x #No outliers
#Caolries 
x = boxplot.stats(cereal$caolries)$out;x #No outliers 
cereal$calories=outlier(cereal$calories)
cereal$calories


#Using the capping method to compute outliers
outlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
#Density plot and boxplot for potass
plot(density(cereal$rating), main="Density Plot: Potassium", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cereal$potass), 2)))  
polygon(density(cereal$rating), col="blue")   
boxplot(cereal$potass,horizontal=TRUE,xlab="Milligrams of potassium",col="blue",main="Boxplot: Potassium")
summary(cereal$potass)
#Potassium  capping outliers
x = boxplot.stats(cereal$potass)$out;x # shows the outliers  280 320 330 260 240
cereal$potass= outlier(cereal$potass)
cereal$potas

#Density plot and boxplot for fiber  
plot(density(cereal$fiber), main="Density Plot: Fiber", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cereal$fiber), 2)))  
polygon(density(cereal$fiber), col="orange")   
boxplot(cereal$fiber,horizontal=TRUE,xlab="Grams of Fiber",col="orange",main="Boxplot: Fiber")
#Fibercapping outliers
x = boxplot.stats(cereal$fiber)$out;x # shows the outliers  9 10 14 
cereal$fiber=outlier(cereal$fiber)
cereal$fiber

#Density plot and boxplot for protein 
plot(density(cereal$protein), main="Density Plot: Protein", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cereal$protein), 2)))  
polygon(density(cereal$protein), col="pink")   
boxplot(cereal$protein,horizontal=TRUE,xlab="Grams of protein",col="pink",main="Boxplot: Protein")
#Protein capping outliers
x = boxplot.stats(cereal$protein)$out;x # shows the outliers 6 5 6
cereal$protein=outlier(cereal$protein)
cereal$protein


#After the removal of outliers
boxplot(cereal$fiber,horizontal=TRUE,xlab="Grams of Fiber",col="orange",main="Boxplot: Fiber")
boxplot(cereal$protein,horizontal=TRUE,xlab="Grams of protein",col="pink",main="Boxplot: Protein")
boxplot(cereal$potass,horizontal=TRUE,xlab="Milligrams of potassium",col="blue",main="Boxplot: Potassium")


# Scatterplots with the response variable 
ggplot(cereal,aes(x=protein,y=rating))+geom_point(size=1.5,color="green4")+ggtitle("Correlation of protein and rating")+ ylab("Rating of the cereals")+xlab("Grams of protein")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=sugars,y=rating))+geom_point(size=1.5,color="orangered1")+ggtitle("Correlation of sugars and rating")+ ylab("Rating of the cereals")+xlab("Grams of sugar")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=fat,y=rating))+ geom_point(size=1.5,color="orange1")+ggtitle("Correlation of fat and rating")+ ylab("Rating of the cereals")+xlab("Grams of fat")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=carbo,y=rating))+geom_point(size=1.5,color="green2")+ggtitle("Correlation of complex carbohydrates and rating")+ylab("Rating of the cereals")+xlab("Grams of complex carbohydrates")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=potass,y=rating))+geom_point(size=1.5,color="deepskyblue3")+ggtitle("Correlation of potassium and rating")+ylab("Rating of the cereals")+xlab("Milligrams of potassium")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=sodium,y=rating))+geom_point(size=1.5,color="deepskyblue1")+ggtitle("Correlation of sodium and rating")+ ylab("Rating of the cereals")+xlab("Milligrams of sodium")+
  stat_smooth(method="lm")

ggplot(cereal,aes(x=fiber,y=rating))+geom_point(size=1.5,color="coral")+ ggtitle("Correlation of dietary fiber and rating")+ylab("Rating of the cereals")+ xlab("Grams of dietary fiber")+
  stat_smooth(method="lm")

#Transformation
require(scales) # to access break formatting functions
ggplot(cereal,aes(x=calories,y=rating))+geom_point(size=1.5,color="coral")+scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + stat_smooth(method="lm")

#Saving the new data as R.data
cereal_clean=cereal
save(cereal_clean, file="cereal_clean.Rdata")









