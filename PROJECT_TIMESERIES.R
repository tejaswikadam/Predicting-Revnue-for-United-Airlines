# Installing and Loading packages 

install.packages("forecast")
install.packages("zoo")
install.packages("dplyr")
library(forecast)
library(zoo)
library(dplyr)

# Loading the dataset

united.data <- read.csv("United.csv")
typeof(united.data)
# Displaying the first and the last few rows of the dataset

head(united.data)
tail(united.data)

#-------------------------------------------------------------------------------------------
#PRE-PROCESSING DATA 

prepro <- as.data.frame(united.data)
prepro

#Checking for NA in the DataFrame(column wise).
sum(is.na(prepro$Year))
sum(is.na(prepro$Quarter))
sum(is.na(prepro$DOMESTIC))
sum(is.na(prepro$LATIN.AMERICA))
sum(is.na(prepro$ATLANTIC))
sum(is.na(prepro$PACIFIC))
sum(is.na(prepro$INTERNATIONAL))

#Droping international columns
#The total column contains the operational revenue across domestic, latin america
# atlantic and pacific regions. The international columsn contains all Na and 0 at certain 
# so not considering that column for this project.
prepro1 = select(prepro, -3:-7)
prepro1

#Droping the the rows that show totals across 4 quarters (yearly)
prepro1 <- prepro1[-c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,103),]
prepro1

#Reindexing the dataframe 
rownames(prepro1) <- 1:nrow(prepro1)
prepro1

#column Names
colnames(prepro1)

#Renaming the columns
prepro1 %>% 
  rename(
    Year= ?..Year
  )

#Saving the dataframe as a csv:
write.csv(prepro1,"F:/SEM3/Time_Series_analysis/Working/Project\\Unitedprepro.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------
#SOME PREPROCESSING IN EXCEL
#-------------------------------------------------------------------------------------------

# Setting the working directory 
setwd("C:/Users/STSC/Desktop/Time_Series_analysis/Working/Project")
#Importing the preprocessed dataset:
united1.data <- read.csv("Unitedprepro.csv")
head(united1.data)

#-------------------------------------------------------------------------------------------
#Create time series data set in R using the ts() function.

united1.ts <- ts(united1.data$TOTAL, 
               start = c(2000,1), end =c(2020,2), frequency = 4)
united1.ts

#Plotting the initial data
plot(united1.ts, 
     xlab = "Time", ylab = "Operational cost", ylim = c(1475400, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000,2021), main = "Quarterly operational Costs", lwd = 3, col="blue") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))

united.stl <- stl(united1.ts, s.window = "periodic")
autoplot(united.stl, main = "Quartely operational cost")

#Acf() function to identify possible time series components.
Acf(united1.ts, lag.max = 8, main = "Autocorrelation for quartely operational costs")

#-------------------------------------------------------------------------------------------
## CReATE DATA PARTITION.
nValid <- 16
nTrain <- length(united1.ts) - nValid
train.ts <- window(united1.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(united1.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))
nTrain
train.ts
valid.ts

#PLot for the training partiton 
plot(train.ts, 
     xlab = "Time", ylab = "Operational cost", ylim = c(1475400, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000,2021), main = "Quarterly operational Costs (TrainingData)", lwd = 3, col="blue") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(valid.ts, col = "black", lty = 1, lwd = 2)

#Fitting Linear and Quadratic trend 
#1) naive 
#2) seasonal naive 
#3) Centered MA 
#4) Trailing MASS
#5)Holts Winter model.extract


#-------------------------------------------------------------------------------------------
#Basic Forecasting
#-------------------------------------------------------------------------------------------
#Model1
#Fitting Linear and Quadratic trend
united.lin <- tslm(train.ts ~ trend)
united.quad <- tslm(train.ts ~ trend + I(trend^2))
# See summary of forecasting equation and associated parameters.
summary(united.lin)
summary(united.quad)
# Apply forecast() function to make predictions for ts data in
# training and validation sets.  
united.lin.pred <- forecast(united.lin, h = nValid, level = 0)
united.quad.pred <- forecast(united.quad, h = nValid, level = 0)

united.lin.pred
united.quad.pred

# Plot predictions for linear trend forecast.
plot(united.lin.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1475400, 11402000), bty = "l",
     xlim = c(2000,2021), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000,2021, 1)) )
lines(united.lin$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot predictions for quadratic trend forecast.
plot(united.quad.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1475400, 11402000), bty = "l",
     xlim = c(2000,2021), main = "Linear Trend Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000,2021, 1)) )
lines(united.quad$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

#Model2
#NAIVE AND SEASONAL NAIVE
united.naive.pred <- naive(train.ts, h = nValid)
united.snaive.pred <- snaive(train.ts, h = nValid)

# Plot predictions for linear trend forecast.
plot(united.naive.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1475400, 11402000), bty = "l",
     xlim = c(2000,2021), main = "Naive Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000,2021, 1)) )
lines(united.naive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

# Plot the predictions for seasonal naive forecast.
plot(united.snaive.pred$mean, 
     xlab = "Time", ylab = "Ridership (in 000s)", ylim = c(1475400, 11402000), bty = "l",
     xlim = c(2000,2021), main = "Seasonal naive forecast Forecast", 
     col = "blue", lwd =2) 
axis(1, at = seq(2000,2021, 1), labels = format(seq(2000,2021, 1)) )
lines(united.snaive.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)

#-------------------------------------------------------------------------------------------
#TWO LEVEL MODEL 
#-------------------------------------------------------------------------------------------
# Regression Model with linear Trend and Seasonality
united.trend.seas <- tslm(train.ts ~ trend + season)
summary(united.trend.seas)
# Creating regression forecast for validation period.
united.trend.seas.pred <- forecast(united.trend.seas, h = nValid, level = 0)
united.trend.seas.pred
# Identifying and displaying residuals for time series based on the regression
united.trend.seas.res <- united.trend.seas$residuals 
united.trend.seas.res

# Applying trailing MA 
united.trailing.res_4 <- rollmean(united.trend.seas.res, k = 4 , align = "right") 
united.trailing.res_4
# Creating forecast for validation period
united.trailing.res_4.pred <- forecast(united.trailing.res_4, h = nValid, level = 0) 
united.trailing.res_4.pred

# combining regression forecast and trailing MA forecast for residuals.
united.combined <- united.trend.seas.pred$mean + united.trailing.res_4.pred$mean 
united.combined

#creating a combined table 
united.comparison <- data.frame(united.trend.seas.pred$mean, united.trailing.res_4.pred$mean, 
                                united.combined)
names(united.comparison) <- c("United trend&seasonality","Residuals","Combined Df")
united.comparison

# Plot training data and regression model.
plot(train.ts, 
     xlab = "Time", ylab = "Operational cost", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000,2021), lwd =2,
     main = "Training series and Regression with Trend and Seasonality") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.trend.seas$fitted, col = "blue", lwd = 2)
lines(united.trend.seas.pred$mean, col = "red", lty =5, lwd = 2)
lines(valid.ts)
legend(2000,5500, legend =  col = c("black", "brown" , "brown"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

# Plot regression residuals data and trailing MA based on residuals.
plot(united.trend.seas.res, 
     xlab = "Time", ylab = "Operational cost", bty = "l",
     xaxt = "n", xlim = c(2000,2021),ylim=c(-2547921.18,2547921.18),lwd =2,
     main = "Regression Residuals and Trailing MA for Residuals, k =4") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.trailing.res_4, col = "blue", lwd = 2, lty = 1)
lines(united.trailing.res_4.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2000,700, legend =  col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

#--------------------------------------------------------------------------------------------
# Regression Model with quad Trend and Seasonality
united.quad.trend.seas <- tslm(train.ts ~ trend+ I(trend^2) + season)
summary(united.quad.trend.seas)
# Creating regression forecast for validation period.
united.quad.trend.seas.pred <- forecast(united.quad.trend.seas, h = nValid, level = 0)
united.quad.trend.seas.pred

# Identifying and displaying residuals for time series based on the regression
united.quad.trend.seas.res <- united.quad.trend.seas$residuals 
united.quad.trend.seas.res

# Applying trailing MA 
united.quad.trailing.res_4 <- rollmean(united.quad.trend.seas.res, k = 4 , align = "right") 
united.quad.trailing.res_4
# Creating forecast for validation period
united.quad.trailing.res_4.pred <- forecast(united.quad.trailing.res_4, h = nValid, level = 0) 
united.quad.trailing.res_4.pred

# combining regression forecast and trailing MA forecast for residuals.
united.quad.combined <- united.quad.trend.seas.pred$mean + united.quad.trailing.res_4.pred$mean 
united.quad.combined

#creating a combined table 
united.quad.comparison <- data.frame(united.trend.seas.pred$mean, united.trailing.res_4.pred$mean, 
                                united.combined)
names(united.quad.comparison) <- c("United quad trend&seasonality","Residuals","Combined Df")
united.quad.comparison

# Plot training data and regression model.
plot(train.ts, 
     xlab = "Time", ylab = "Operational cost", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000,2021), lwd =2,
     main = "Training series and Regression with quadratic Trend and Seasonality") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.quad.trend.seas$fitted, col = "blue", lwd = 2)
lines(united.quad.trend.seas.pred$mean, col = "red", lty =5, lwd = 2)
lines(valid.ts)
legend(2000,5500, legend =  col = c("black", "brown" , "brown"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

# Plot regression residuals data and trailing MA based on residuals.
plot(united.quad.trend.seas.res, 
     xlab = "Time", ylab = "Operational cost", bty = "l",
     xaxt = "n", xlim = c(2000,2021),ylim=c(-2547921.18,2547921.18),lwd =2,
     main = "Regression Residuals and Trailing MA for Residuals, k =4") 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.quad.trailing.res_4, col = "blue", lwd = 2, lty = 1)
lines(united.quad.trailing.res_4.pred$mean, col = "blue", lwd = 2, lty = 5)
legend(2000,700, legend =  col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

#Accuracy Measures
round(accuracy(united.trend.seas.pred, valid.ts), 3) 
round(accuracy(united.combined, valid.ts), 3)
round(accuracy(united.quad.trend.seas.pred, valid.ts), 3) 
round(accuracy(united.quad.combined, valid.ts), 3)

#-------------------------------------------------------------------------------------------
#Holt Winters model 
#-------------------------------------------------------------------------------------------
united.ZZZ.train <- ets(train.ts, model = "ZZZ")
united.ZZZ.train 

united.ZZZ.train.pred<- forecast(united.ZZZ.train , h = nValid, level = 0)
united.ZZZ.train.pred

plot(united.ZZZ.train.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.ZZZ.train.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

#Accuracy measure
round(accuracy(united.ZZZ.train.pred, valid.ts), 3)

#-------------------------------------------------------------------------------------------
#Regression models
#-------------------------------------------------------------------------------------------
#Model 1: Regression model with linear trend
united.lin.trend <- tslm(train.ts~ trend)
summary(united.lin.trend)

#Forecasting for Validation period:
united.lin.trend.pred <- forecast(united.lin.trend, h = nValid, level = 0)
united.lin.trend.pred

plot(united.lin.trend.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with linear trend", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.lin.trend.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

#---------------------------------------------------------------------------------------------------
#Model 2: Regression Model with Exponential Trend:
united.extrend <-tslm(train.ts ~ trend, lambda = 0)
summary(united.extrend)

#Forecasting for Validation period:
united.extrend.pred <- forecast(united.extrend, h = nValid, level = 0)
united.extrend.pred

plot(united.extrend.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with exponential trend", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.extrend.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(train.ts,col="black",lty=1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")
#---------------------------------------------------------------------------------------------------
#Model 3: Regression mode with quadratic trend
united.quadtrend <- tslm(train.ts~ trend + I(trend^2))
summary(united.quadtrend)

#Forecasting for Validation period:
united.quadtrend.pred <- forecast(united.quadtrend, h = nValid, level = 0)
united.quadtrend.pred

plot(united.quadtrend.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with quad trend", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.quadtrend.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

#---------------------------------------------------------------------------------------------------
#Model 4:Regression model with seasonality
united.sea <- tslm(train.ts ~ season) 
summary(united.sea)

#Forecasting for Validation period:
united.sea.pred <- forecast(united.sea, h = nValid, level = 0)
united.sea.pred

plot(united.sea.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with seasonality", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.sea.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

#---------------------------------------------------------------------------------------------------
#Model 5: Regression model with linear trend and seasonality.
united.lin.sea <- tslm(train.ts ~ trend + season)
summary(united.lin.sea)

united.lin.sea.pred <- forecast(united.lin.sea, h = nValid, level = 0)
united.lin.sea.pred

plot(united.lin.sea.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with linear trend and seasonality", flty = 2) 
axis(1, at = seq(2000, 2021, 1), labels = format(seq(2000, 2021, 1)))
lines(united.lin.sea.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021, 11508100, "Future")

#Model 5_1: Regression model with quadratic trend and seasonality.
united.qua.sea <- tslm(train.ts ~ trend +I(trend^2) + season)
summary(united.qua.sea)

united.qua.sea.pred <- forecast(united.qua.sea, h = nValid, level = 0)
united.qua.sea.pred

plot(united.qua.sea.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Regression model with quad trend and seasonality", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(united.qua.sea.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021.25, 11508100, "Future")

#Accuracy for all the developed models
#Comparing Accuracy for Above 5 MLR models For united operational revenue

#Accuracy for Model 1: Regression model with linear trend
round(accuracy(united.lin.trend.pred, valid.ts), 3)  #second best 

#Accuracy for Model2:  Regression Model with Exponential Trend:
round(accuracy(united.extrend.pred, valid.ts), 3) 

#Accuracy for Model 3:Regression mode with quadratic trend
round(accuracy(united.quadtrend.pred, valid.ts), 3)  

#Accuracy for Model 4:Regression model with seasonality
round(accuracy(united.sea.pred, valid.ts), 3)   

#Accuracy for Model 5: Regression model with linear trend and seasonality.  
round(accuracy(united.lin.sea.pred, valid.ts),3) #first best 

#Accuracy for Model 5: Regression model with quadratic trend and seasonality.  
round(accuracy(united.qua.sea.pred, valid.ts),3) 


#-------------------------------------------------------------------------------------------
#ARIMA MODEL 
#-------------------------------------------------------------------------------------------
united.auto.arima <- auto.arima(train.ts)
summary(united.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
united.auto.arima.pred <- forecast(united.auto.arima, h = nValid, level = 0)
united.auto.arima.pred

plot(united.auto.arima.pred, 
     xlab = "Time", ylab = "united operatonal costs", ylim = c(1475100, 11402000), bty = "l",
     xaxt = "n", xlim = c(2000, 2021), 
     main = "Auto ARIMA Model", flty = 2) 
axis(1, at = seq(2000, 2022, 1), labels = format(seq(2000, 2022, 1)))
lines(united.auto.arima.pred$fitted, col = "blue", lwd = 3)
lines(valid.ts, col = "black", lty = 1)
lines(c(2018 - 1.70, 2018 - 1.70), c(1475400, 11402000))
lines(c(2020.35, 2020.35), c(1475400, 11402000))
text(2008, 11508100, "Training")
text(2018, 11508100, "Validation")
text(2021.25, 11508100, "Future")

#Accuracy
round(accuracy(united.auto.arima.pred, valid.ts), 3) #best till now 


# ALL MODELS ACCURACY TEST
#---------------------------TWO level model with (trailing_ma)-------------------------------
round(accuracy(united.trend.seas.pred, valid.ts), 3) 
round(accuracy(united.combined, valid.ts), 3)
round(accuracy(united.quad.trend.seas.pred, valid.ts), 3) 
round(accuracy(united.quad.combined, valid.ts), 3)
#--------------------------------------------------------------------------------------------

#---------------------------Holts Winter model-----------------------------------------------
round(accuracy(united.ZZZ.train.pred, valid.ts), 3)
#--------------------------------------------------------------------------------------------

#---------------------------Regressions models accuracy--------------------------------------
#Accuracy for Model 1: Regression model with linear trend
round(accuracy(united.lin.trend.pred, valid.ts), 3)  #second best 

#Accuracy for Model2:  Regression Model with Exponential Trend:
round(accuracy(united.extrend.pred, valid.ts), 3) 

#Accuracy for Model 3:Regression mode with quadratic trend
round(accuracy(united.quadtrend.pred, valid.ts), 3)  

#Accuracy for Model 4:Regression model with seasonality
round(accuracy(united.sea.pred, valid.ts), 3)   

#Accuracy for Model 5: Regression model with linear trend and seasonality.  
round(accuracy(united.lin.sea.pred, valid.ts),3) #first best 

#Accuracy for Model 5: Regression model with quadratic trend and seasonality.  
round(accuracy(united.qua.sea.pred, valid.ts),3) 
#--------------------------------------------------------------------------------------------

#--------------------------------ARIMA MODEL-------------------------------------------------
round(accuracy(united.auto.arima.pred, valid.ts), 3) #best till now 
#--------------------------------------------------------------------------------------------
