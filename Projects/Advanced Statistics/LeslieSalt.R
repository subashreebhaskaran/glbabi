leslie_data <- read.csv("Dataset_LeslieSalt.csv")

leslie_data$Date <- leslie_data$Date/12
summary(leslie_data)
str(leslie_data)
leslie_data$County <- as.factor(leslie_data$County)
leslie_data$Flood <- as.factor(leslie_data$Flood)


model1 <- lm(Price ~ Size+Elevation+Sewer+Date+Flood+Distance+County, data = leslie_data)
model1
#Output:
#Coefficients:
#  (Intercept)         Size    Elevation        Sewer         Date       Flood1     Distance      County1  
#2.364e+01   -6.043e-03    5.193e-01   -9.573e-04    1.021e+00   -1.202e+01    1.858e-01   -8.789e+00  
