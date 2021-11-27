# Delete all the objects in the workspace 
rm(list=ls())
#set working directory
setwd("C:/Users/lama_/Desktop/Data mining/HW4_Datamining")
#######Load the data#######
data=read.csv("C:/Users/lama_/Desktop/Data mining/HW4_Datamining/wine.data", header=FALSE)
names(data) = c("Wine", "Alcohol", "Malic_acid", "Ash", "Alcalinity_ash", "Magnesium", "Total_phenols", "Flavanoids", 
                "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity","Hue","OD280/OD315","Proline")
#Split the data in to training and testing
set.seed(1)
index = sample(1:nrow(data), 0.80*nrow(data))
X.train = data[index,]
X.test = data[-index,]
Y.train = data$wine[index]
Y.test = data$wine[-index]
#CART
#install.packages("rpart.plot")
library(rpart.plot)
library(rpart)				    
library(tree)
library(caret)
set.seed(123)
model.control=rpart.control(minsplit = 5, xval = 10, cp = 0)
#fit the model
fit.wine = rpart(Wine~., X.train, method = "class", control=model.control) # method is classification
fit.wine$frame$yval = as.factor(rownames(fit.wine$frame))

#Plot the tree  #The numbers indicate the number of members of each class in that node. 
#So, the label "0 / 1 / 48" tells us that there are 0 cases of category 1 (Barabera, I infer), only one example of category 2 (Barolo), and 48 of category 3 (Grignolino).
x11()
rpart.plot(fit.wine)	
#x11()
plot(fit.wine, branch = .3, compress=T, main = "Full Tree")
text(fit.wine,use.n = TRUE,cex = .5)
######train predictions
pred.train = predict(fit.wine,X.train )
unique(pred.train)# see where the training points fall --Number of unqie predictions correpsonds diretly to the numebr of temrinal nodes
#test predictions
pred.test = predict(fit.wine,X.test , type = "vector")
pred.test #nodes
unique(pred.test)
pred.test = predict(fit.wine,X.test , type = "class")
confusionMatrix(pred.test, factor(X.test$Wine)) # See how accurate  

pred.test <- predict(fit.wine, X.test, type = "matrix")
pred.test <- data.frame(pred.test)
uniquepred.test=unique(pred.test[,2:4])#the class counts at that node in the ???tted tree,
names(uniquepred.test) <- c("#ofclass1countsatnode", "#ofclass2countsatnode","#ofclass3countsatnode")
uniquepred.test
#####################################
# GROW a classifcation tree and prune it
x11() # look at the cv error look at the fourth column of cp table 
plot(fit.wine$cptable[,4], main = "Cp for model selection", ylab = "cv error") # 4 is the index 
min_cp = which.min(fit.wine$cptable[,4])
min_cp #index of model--complexity of the tree
#Doa prune fit based on the min_cp value 
pruned_fit_wine=prune(fit.wine, cp = fit.wine$cptable[min_cp,1]) #snips off based on cp
## plot the  the pruned tree
x11()
rpart.plot(pruned_fit_wine)
plot(pruned_fit_wine, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_wine, use.n = TRUE,cex = .5)

#train predictions
pred_train = predict(pruned_fit_wine,X.train , type = "vector")
unique(pred_train)
#test predictions
pred_test = predict(pruned_fit_wine,X.test , type = "vector")
unique(pred_test)
###########################################################################
#install.packages("tree")
library("tree")
library("caret")
X.train=data.frame(X.train)
fit=tree(factor(Wine)~., data = X.train, split = "gini") # use all the predictors---for classification ---more flexibility ---define cost complexity criteria/impurity finction
summary(fit)# for split use gini----------------misclassification is 0//1# gini index-measure be in the pure class/mimini the criterion and have the terminal regions be pure
X.test=data.frame(X.test) #error rates 
pred.test= predict(fit, newdata = X.test, type = "class") 
confusionMatrix(pred.test, factor(X.test$Wine))

cv.fit = cv.tree(fit, FUN = prune.misclass)
summary(cv.fit)
cv.fit
pruned_tree <- prune.misclass(fit, best = 6)
summary(pruned_tree)
x11()
plot(pruned_tree)
text(pruned_tree,  cex = .5)
x11()
plot(fit)
text(fit,  cex = .5)


#predcition on test set- to get test error 
#The number of unique prediction corresponds to the number of general nodes-there should be X terminal nodes--every preidction gets assigned one qunatitiative value but there are only X diffrernt uniques ones corrrepsonding to unique regions
# biggest indicator of the wine/other important predictors--value is detmeinred based on the average of the training data/more error in regression 

