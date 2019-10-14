setwd("D:/PGP BABI/ML Group Assignmentt")
getwd()

#Loading all necessary Libraries
library(caret)
library(mlr)
library(DMwR)
library(caTools)
library(ipred)
library(dplyr)
library(base)
library(mice)
library(gmodels)

#Read the input file & backup
cars=read.csv("Cars.csv",na.strings = "")
cars1=cars
attach(cars)

#Summary and strcuture of data set
summary(cars)
str(cars)

# Conver the categorical variables to factor data type
cars$Engineer<-as.factor(cars$Engineer)
cars$MBA<-as.factor(cars$MBA)
cars$license<-as.factor(cars$license)


# Bivariate EDA
ggplot(cars,aes(x=Transport,y=Age,col=Transport))+geom_point()
ggplot(cars,aes(x=Transport,y=..count..,fill=Gender))+geom_bar(position = "dodge")
ggplot(cars,aes(x=Transport,y=..count..,fill=Engineer))+geom_bar(position = "dodge")
ggplot(cars,aes(x=Transport,y=..count..,fill=MBA))+geom_bar(position = "dodge")
ggplot(cars,aes(x=MBA,y=..count..,fill=Transport))+geom_bar(position = "dodge")
ggplot(cars,aes(x=Age,y=Work.Exp,col=Transport))+geom_point()
ggplot(cars,aes(x=Age,y=Distance,col=Transport))+geom_point()
ggplot(cars,aes(x=Age,y=Salary,col=Transport))+geom_point()
ggplot(cars,aes(x=Engineer,y=..count..,fill=Transport))+geom_bar(position = "dodge")
ggplot(cars,aes(x=Transport,y=Age,col=MBA))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Work.Exp,col=MBA))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Age,col=Gender))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Age,col=Engineer))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Salary,col=Engineer))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Salary,col=MBA))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Distance,col=Engineer))+geom_jitter(position = position_jitter(width = 0.2),size=2)
ggplot(cars,aes(x=Transport,y=Distance,col=MBA))+geom_jitter(position = position_jitter(width = 0.2),size=2)

# Univariate EDA
densityplot(cars$Age)
boxplot(cars$Age)
densityplot(cars$Work.Exp)
boxplot(cars$Work.Exp)
densityplot(cars$Salary)
boxplot(cars$Salary)
densityplot(cars$Distance)
boxplot(cars$Distance)



#Missing Value imputation

mean(cars$Salary[which(cars$MBA==0)])
mean(cars$Salary[which(cars$MBA==1)])

mean(cars$Salary[which(cars$Engineer==0 & cars$MBA==0)])     
mean(cars$Salary[which(cars$Engineer==0 & cars$MBA==1)])     
mean(cars$Salary[which(cars$Engineer==1 & cars$MBA==0)])     
mean(cars$Salary[which(cars$Engineer==1 & cars$MBA==1)])     

cars[which(is.na(cars$MBA)),]
cars[which(is.na(cars$MBA)),][4]<-1

#Recheck if any missing value exists and run a cross table to under the effect of license vs Transport
summary(cars)
cross1<-CrossTable(cars$license, cars$Transport,prop.r = TRUE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)

#check of Multi-collinearity using Correlation plot
library(dplyr)
forCorr=select_if(cars, is.numeric)
cor=cor(forCorr)
library(corrplot)
corrplot(cor, type="lower",method = "number")

#Convert the dependent variable into "Cars" and "Other Modes of Transport"
cars$car_Transport=ifelse(cars$Transport=='Car',1,0)
table(cars$car_Transport)
cars$car_Transport=as.factor(cars$car_Transport)
summary(cars)

#Test and Train data set split
split = sample.split(cars$car_Transport, SplitRatio = 0.7)
carstrain = subset(cars, split == TRUE)
carstest = subset(cars, split == FALSE)

prop.table(table(carstrain$car_Transport))

carstrain<-carstrain[,c(1:8,10)]
carstest<-carstest[,c(1:8,10)]
str(carstrain)
attach(carstrain)

#Balance the dependent variable using SMOTE
carsSMOTE<-SMOTE(car_Transport~., carstrain, k=5, perc.over = 250,perc.under = 150)
prop.table(table(carsSMOTE$car_Transport))
prop.table(table(carstest$car_Transport))
str(carsSMOTE)

#Create Train and Test Task
traincarTask = makeClassifTask(data = carsSMOTE,target = "car_Transport", positive = "1")
testcarTask = makeClassifTask(data = carstest, target = "car_Transport", positive = "1")

#Preliminary logistic regression using MLR (Multicollinear data set)
logistic.learner1 = makeLearner("classif.logreg",predict.type = "response")
logmodel = train(logistic.learner1,traincarTask)
logpredict = predict(logmodel, testcarTask)
lr.cm=confusionMatrix(logpredict$data$truth,logpredict$data$response, positive = "1")
lr.cm


##Finding the significant coefficients using different logistic regression models
attach(carsSMOTE)

#Model 1 (Salary, Work Experience & Age considered)
carsLModel1=glm(car_Transport~., family="binomial", data = carsSMOTE)
summary(carsLModel1)

#Model 2 (Only Salary considered)
carsLModel2<-glm(car_Transport~Distance+Salary+Gender+Engineer+MBA+license,family = "binomial",data = carsSMOTE)
summary(carsLModel2)

#Model 3 (Only Age considered)
carsLModel3<-glm(car_Transport~Distance+Age+Gender+Engineer+MBA+license,family = "binomial",data = carsSMOTE)
summary(carsLModel3)

#Model 4 (Only Work Experience considered)
carsLModel4<-glm(car_Transport~Distance+Work.Exp+Gender+Engineer+MBA+license,family = "binomial",data = carsSMOTE)
summary(carsLModel4)

# Plot ROC & AUC
library(pROC)
par(pty="s")
roc(carsSMOTE$car_Transport,carsLModel1$fitted.values,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage",
    ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=TRUE,print.auc.x=45)
roc(carsSMOTE$car_Transport,carsLModel2$fitted.values,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage",
    ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=TRUE,print.auc.x=45)
roc(carsSMOTE$car_Transport,carsLModel3$fitted.values,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage",
    ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=TRUE,print.auc.x=45)
roc(carsSMOTE$car_Transport,carsLModel4$fitted.values,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False Positive Percentage",
    ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=TRUE,print.auc.x=45)

#New Train and Test task set (Solution for multicollinearity)
str(carsSMOTE)
attach(carstrain)
carsSMOTE_new<-carsSMOTE[,c(1:4,7:9)]
carstest_new<-carstest[,c(1:4,7:9)]
str(carsSMOTE_new)
str(carstest_new)
traincarTask2 = makeClassifTask(data = carsSMOTE_new,target = "car_Transport", positive = "1")
testcarTask2 = makeClassifTask(data = carstest_new, target = "car_Transport", positive = "1")


#Final logistic regression using MLR (Multicollinear data set)
logistic.learner2 = makeLearner("classif.logreg",predict.type = "response")
logmodel2 = train(logistic.learner2,traincarTask2)
logpredict2 = predict(logmodel2, testcarTask2)
lr.cm2=confusionMatrix(logpredict2$data$truth,logpredict2$data$response, positive = "1")
lr.cm2


#KNN
knn.learner=makeLearner("classif.knn",predict.type="response")
knn.model=train(knn.learner,traincarTask2)
knn.predict=predict(knn.model,testcarTask2)

knn.cm=confusionMatrix(knn.predict$data$truth,knn.predict$data$response, positive = "1")
knn.cm


#Naive Baeyes
nb.learner=makeLearner("classif.naiveBayes",predict.type="response")
nb.model=train(nb.learner,traincarTask2)
nb.predict=predict(nb.model,testcarTask2)

confusionMatrix(nb.predict$data$truth,nb.predict$data$response, positive = "1")


#Bagging
rf.learner=makeLearner("classif.randomForest", predict.type = "response")
rf.model=train(rf.learner, traincarTask2)
rf.predict= predict(rf.model, testcarTask2)
confusionMatrix(rf.predict$data$truth, rf.predict$data$response, positive = "1")

#Boosting
gbm.learner=makeLearner("classif.gbm", predict.type = "response")
gbm.model=train(gbm.learner, traincarTask2)
gbm.predict= predict(gbm.model, testcarTask2)
confusionMatrix(gbm.predict$data$truth, gbm.predict$data$response, positive = "1")

