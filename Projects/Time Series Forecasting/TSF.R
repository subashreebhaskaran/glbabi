setwd("D:/PGP BABI/TSF/Group Assignment")
getwd()
csvfile<-read.csv(file.choose())
str(csvfile)
summary(csvfile)

## Calling the needed Libraries
library(data.table)
library(ggplot2)
library(fpp2)
library(forecast)
library(stats)
library(tseries)

## Converting the file to Time series
tsfile<-ts(csvfile[,3],start=c(2000,1),frequency = 4)
View(tsfile)

## Plotting the Timeseries to check if missing values exist
plot(tsfile, xlab="Year", ylab="Beer Sales in Thousand Dollars", main="Beer Sales Plot")

##to check if seasonality exists
## Plot1 - Seasonality Yearwise
ggseasonplot(tsfile, year.labels = TRUE, year.labels.left = TRUE) + ylab("Sale Value") + ggtitle("Beer Sales")

## Plot2 - Seasonality Yearwise (Polartype)
ggseasonplot(tsfile, polar = TRUE) + ylab("Sale Value") + ggtitle("Beer Sales")

## Plot3 - Seasonality Quarterwise
monthplot(tsfile, xlab="Quarter", ylab = "Sale Value")



### A1) Decompose Method:
# Step 1: Decompose
TSDecompose<-decompose(tsfile, type = "a")
plot(TSDecompose)
TSDecompose
# Step 2: Identify Seasonal Indices
Ses_ind<-round(t(TSDecompose$figure),2)
Ses_ind_df<-as.data.frame(Ses_ind)

### A2) STL Method:
stlfile<-stl(tsfile,s.window = "p")
plot(stlfile)



### Splitting into Test and Train
TS_Train<-window(tsfile,start=c(2000,1),end=c(2015,4),frequency=4)
TS_Test<-window(tsfile,start=c(2016,1),end=c(2017,4),frequency=4)
View(TS_Train)

autoplot(TS_Test,series="Train")+autolayer(TS_Train,series = "Test")+ggtitle("Beer Sales")+xlab("Year")+ylab("Sales")+guides(colour=guide_legend(title="Forecast"))


### Forecasting using STL Method:
TS_Train_STL<-stl(TS_Train,s.window = "p")
TS_Train_Forecast<-forecast(TS_Train_STL,method="rwdrift",h=8)
plot(TS_Train_Forecast)

Vec2<-cbind(TS_Test,as.data.frame(TS_Train_Forecast<-forecast(TS_Train_STL,method="rwdrift",h=8))[,1])
ts.plot(Vec2,col=c("red","green"))


### Calculating RMSE and MAPE
RMSE<-sqrt((sum(Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))
round(RMSE,2)
MAPE<-mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
round(MAPE,2)

### Holt Winter Model
TS_Train_HW<-hw(TS_Train, seasonal="additive")
plot(TS_Train_HW)

TS_Train_HW$model
TS_Train_HW

TS_HW_Forecast<-forecast(hw(TS_Train,seasonal = "a",h=8))
TS_HW_Forecast
TS_Test
vec1<-cbind(TS_Test,as.data.frame(TS_HW_Forecast)[,1])
ts.plot(vec1,col=c("red","green"))

### Calculating RMSE and MAPE
RMSE<-sqrt((sum(vec1[,1]-vec1[,2])^2)/length(vec1[,1]))
round(RMSE,2)
MAPE<-mean(abs(vec1[,1]-vec1[,2])/vec1[,1])
round(MAPE,2)


### SARIMA Model
## Augmented Dickey Fuller Test - Test of Stationarity
adf.test(TS_Train,k=8)
adf.test(log(TS_Train),k=8)
adf.test(diff(TS_Train),k=8)
adf.test(diff(log(TS_Train)),k=8)

plot(log(TS_Train))
plot(diff(TS_Train))
plot(diff(log(TS_Train)))

par(mfrow=c(1,2))        
acf(ts(TS_Train),main="ACF")
pacf(ts(TS_Train),main="PACF")

TS_Arima<-auto.arima(TS_Train)
TS_Arima
TS_Arima$residuals
plot(TS_Arima$residuals)
acf(ts(TS_Arima$residuals),main=("ACF_211_011"))

TS_Arima_Test1<-arima(TS_Train,order = c(2,1,1),seasonal=c(1,1,1),method = "ML")
TS_Arima_Test1
acf(ts(TS_Arima_Test1$residuals), main=c("ACF_211_111"))

TS_Arima_Test2<-arima(TS_Train,order = c(2,1,0),seasonal=c(1,1,1),method = "ML")
TS_Arima_Test2
acf(ts(TS_Arima_Test2$residuals),main=("ACF_210_111"))

TS_Arima_Test3<-arima(TS_Train,order = c(2,1,2),seasonal=c(1,1,0),method = "ML")
TS_Arima_Test3
acf(ts(TS_Arima_Test3$residuals), main=("ACF_212_110"))

par(mfrow=c(2,2))
acf(ts(TS_Arima$residuals),main=("ACF_211_011"))
acf(ts(TS_Arima_Test1$residuals), main=c("ACF_211_111"))
acf(ts(TS_Arima_Test2$residuals),main=("ACF_210_111"))
acf(ts(TS_Arima_Test3$residuals), main=("ACF_212_110"))


TS_ARIMA_Forecast<-forecast(auto.arima(TS_Train),h=8)
TS_ARIMA_Forecast
vec3<-cbind(TS_Test,as.data.frame(TS_ARIMA_Forecast)[,1])
ts.plot(vec3, col=c("red","green"))


TS_Train%>%diff()%>%ggtsdisplay()

TS_Train %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

Box.test(TS_Arima$residuals,lag=6,type="Ljung-Box")
Box.test(TS_Arima$residuals,lag=4,type="Ljung-Box")
Box.test(TS_Arima$residuals,lag=4,type="Ljung-Box")
