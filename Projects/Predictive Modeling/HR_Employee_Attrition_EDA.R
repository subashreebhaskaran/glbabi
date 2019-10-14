getwd()
setwd("/Users/anand/Documents/BABI/DataMining/GroupAssignment_HRAttrition")

#Read Data & EDA
hr_data <- read.csv("HR_Employee_Attrition_Data.csv")
str(hr_data)
summary(hr_data)

#data type change:
hr_data <- subset(hr_data, select = -c(EmployeeCount, Over18, StandardHours))
hr_data$Education <- as.factor(hr_data$Education)
hr_data$EnvironmentSatisfaction <- as.factor(hr_data$EnvironmentSatisfaction)
hr_data$JobInvolvement <- as.factor(hr_data$JobInvolvement)
hr_data$JobSatisfaction <- as.factor(hr_data$JobSatisfaction)
hr_data$PerformanceRating <- as.factor(hr_data$PerformanceRating)
hr_data$RelationshipSatisfaction <- as.factor(hr_data$RelationshipSatisfaction)
hr_data$WorkLifeBalance <- as.factor(hr_data$WorkLifeBalance)
hr_data$Age <- as.factor(hr_data$Age)
hr_data$EmployeeNumber <- as.factor(hr_data$EmployeeNumber)


#scaling:
library(MASS)
ind <- sapply(hr_data, is.numeric)
hr_data[ind] <- lapply(hr_data[ind], scale)
View(hr_data)

write.csv(hr_data, "Scaled_HR_Data.csv")

#Spilting the data for training and Testing 
install.packages("caTools")
library(caTools)
#setting the seed value
set.seed(123)
#setting the splitratio
splitdata=sample.split(hr_data,SplitRatio = .70)
hr_trainingdata=subset(hr_data,splitdata ==TRUE)
hr_testingdata=subset(hr_data,splitdata ==FALSE)

write.csv(hr_trainingdata, "HR_trainingdata.csv")
write.csv(hr_testingdata, "HR_testingdata.csv")

