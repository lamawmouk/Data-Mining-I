rm(list = ls())
# set the current working directory
setwd("C:/Users/lama_/Desktop/HW1_DataMining")
#Reading the clean cereal data
load("C:/Users/lama_/Desktop/HW1_DataMining/cereal_clean.Rdata")
#To ensure consistent results
set.seed(1)
# Linear regression model
cereal_clean[,]<-lapply(cereal_clean,as.numeric)
fit <- lm(rating ~.-name,data=cereal_clean)
names(fit)
summary(fit)
#To check the correlation coeeficents of predictors with response variable
cor(cereal_clean$fat,cereal_clean$sugars)
cor(cereal_clean$rating,cereal_clean$sodium)
cor(cereal_clean$rating,cereal_clean$fat)
cor(cereal_clean$rating,cereal_clean$protein)
cor(cereal_clean$fat,cereal_clean$fiber)
cor(cereal_clean$rating,cereal_clean$calories)
#To check the correlation coeeficents of predictors with each other
cor(cereal_clean)
# To check for interactions with the model
my_data <- cereal_clean[, c(4,5,6,7,8,10)]
cor(my_data)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(my_data, histogram=TRUE, pch=19)
#Testing interactions using the summary of the linear model lm()
fit1=lm(rating ~.-name+protein:fiber, data=cereal_clean)
summary(fit1)
fit2=lm(rating ~ protein*fiber, data=cereal_clean)
summary(fit2)
fit3=lm(rating ~.-name+fat:calories, data=cereal_clean)
summary(fit3)
fit4=lm(rating ~.-name+ sugars:calories, data=cereal_clean)
summary(fit4)
fit5=lm(rating ~ sugars*calories, data=cereal_clean)
summary(fit5)
