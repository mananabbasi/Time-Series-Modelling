install.packages("TTR") 
install.packages("forecast") 
library(TTR) 
library(forecast) 

library(readxl)


Birth <- read_excel("Vital Statistics in the UK.xlsx", sheet = "Birth", skip = 5)
Birth


#imp
column_names <- colnames(Birth)   # Get column names
indices <- seq_along(column_names)                        # Get column indexes
column_info <- data.frame(Index = indices, Name = column_names)
print(column_info)

#Time Series
Birth_TS_1 <- ts(Birth$`Number of live births: United Kingdom`, frequency=1, start=c(1887,1), end=c(2021, 1))

# Plot the time series
plot.ts(Birth_TS_1, main="Birth Data in United Kingdom (1887-2021)", ylab="Number of Birth", xlab="Year")

sum(is.nan(Birth_TS_1))  # Returns the number of NaN values
sum(is.infinite(Birth_TS_1))  # Returns the number of Inf values
sum(is.na(Birth_TS_1))






#In this case, it appears that an additive model is not appropriate for describing this time series, since the size of the 
#random fluctuations seem to increase with the level of the time series. Thus, we may need to 
#transform the time series in order to get a transformed time series that can be described using an additive model.




Birth_TS_1 <- as.numeric(as.character(Birth_TS_1))
log_Birth_TS_1 <- log(Birth_TS_1)
plot.ts(log_Birth_TS_1) 
log_Birth_TS_1



#Decomposing the Birth Time Series


#Thus, we can try to estimate the trend component of this time series by smoothing using a simple moving average. To 
#smooth the time series using a simple moving average of order 15, and plot the smoothed time series data, we type: 

library("TTR") 

log_Birth_TS_1SMA3 <- SMA(log_Birth_TS_1,n=15) 
plot.ts(log_Birth_TS_1SMA3) 




#Forecasts using Smoothing 
#Holt-Winters Exponential Smoothing (additive model with increasing or decreasing trend and no seasonality)


log_Birth_TS_1_Numericforecasts_2 <- HoltWinters(log_Birth_TS_1, gamma=FALSE) 

log_Birth_TS_1_Numericforecasts_2



#REsults of forcast



plot(log_Birth_TS_1_Numericforecasts_2)



#Checking Sum   Of squre the lesser value means best model  0.33433

log_Birth_TS_1_Numericforecasts_2$SSE 



#As for simple exponential smoothing, we can make forecasts for future times not covered by the original time series by 

log_Birth_TS_1_Numericforecasts_2forecasts2 <- forecast(log_Birth_TS_1_Numericforecasts_2, h=19) 
plot(log_Birth_TS_1_Numericforecasts_2forecasts2) 





#whether the in-sample forecast errors show non-zero autocorrelations at lags 1-20. For example, for the skirt hem data, 
#we can make a correlogram, and carry out the Ljung-Box test, by typing (na.omit omits the null values):

acf(na.omit(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals), lag.max=20)
#Here the correlogram shows that the sample autocorrelation for the in-sample forecast errors at lag 18 exceeds 
#the significance bounds. However, we would expect one in 20 of the autocorrelations for the first twenty lags to 
#exceed the 95% significance bounds by chance alone.
Box.test(na.omit(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals), lag = 20, type = "Ljung-Box") 



#As for simple exponential smoothing, we should also check that the forecast errors have constant variance over time, 
#and are normally distributed with mean zero. We can do this by making a time plot of forecast errors, and a histogram 
#of the distribution of forecast errors with an overlaid normal curve: 
plot.ts(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals) 
log_Birth_TS_1_Numericforecasts_2forecasts2$residuals <- log_Birth_TS_1_Numericforecasts_2forecasts2$residuals[!is.na(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals)]




plotForecastErrors <- function(forecasterrors) {
  # Calculate the standard deviation
  mybinsize <- IQR(forecasterrors) / 4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd * 5
  mymax <- max(forecasterrors) + mysd * 5
  # Generate a sequence for histogram bins
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) mymin <- mymin2
  if (mymax2 > mymax) mymax <- mymax2
  mybins <- seq(mymin, mymax, mybinsize)
  # Plot the histogram
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, 
       main = "Histogram of Forecast Errors")
  # Overlay a normal distribution curve
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}


plotForecastErrors(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals) # make a histogram


mean(log_Birth_TS_1_Numericforecasts_2forecasts2$residuals) 






#Applying Arima to get a better model 





#Now Applying Arima Model in my UK-Birth Time series


plot.ts(Birth_TS_1)

#Now Applying the difference to get a stationary time series
#ARIMA models are defined for stationary time series. Therefore, if you start off with a non-stationary time 
#series, you will first need to ‘difference’ the time series until you obtain a stationary time series. 
class(Birth_TS_1)
str(Birth_TS_1)
Birth_TS_1 <- as.numeric(as.character(Birth_TS_1))

#Applying 1 Difference

Birth_TS_1diff1 <- diff(Birth_TS_1, differences=1) 
plot.ts(Birth_TS_1diff1) 

#Difference 2
Birth_TS_1diff3 <- diff(Birth_TS_1, differences=2) 
plot.ts(Birth_TS_1diff2) 


#The time series of second differences (above) does appear to be stationary in mean and variance, as the level of the 
#series stays roughly constant over time, and the variance of the series appears roughly constant over time. 

# plot a  correlogram 
acf(Birth_TS_1diff2, lag.max=20) 
acf(Birth_TS_1diff2, lag.max=20, plot=FALSE) # 


# plot a  Partial correlogram

pacf(Birth_TS_1diff2, lag.max=20) 
# plot a partial correlogram 
pacf(Birth_TS_1diff2, lag.max=20, plot=FALSE)

#Model will be 3,2,1) 


#(3,2,1)



#Model Selection Considerations:



Birth_TS_1arima_model1 <- arima(Birth_TS_1, order=c(3,2,1)) # fit an ARIMA(3,2,1) model 
Birth_TS_1arima_model1 


#now applying forcasting2.1.3 Forecasting Using an ARIMA Model(3,2,1)

Birth_TS_1arimaforecasts <- forecast(Birth_TS_1arima_model1, h=5, level=c(99.5)) 
Birth_TS_1arimaforecasts 




Birth_TS_1arimaforecasts <- forecast(Birth_TS_1arimaforecasts, h=5, level=c(99.5))
forecast_years <- seq(from = 2022, by = 1, length.out = 5)
custom_forecast <- ts(Birth_TS_1arimaforecasts$mean, start = 2022, frequency = 1)
forecast_df <- data.frame(
  Year = forecast_years,
  Forecast = as.vector(custom_forecast),
  Lower = as.vector(Birth_TS_1arimaforecasts$lower),
  Upper = as.vector(Birth_TS_1arimaforecasts$upper)
)

forecast_df




plot(Birth_TS_1arimaforecasts)



acf(Birth_TS_1arimaforecasts$residuals, lag.max=20) 
Box.test(Birth_TS_1arimaforecasts$residuals, lag=20, type="Ljung-Box")

#there is no autocorelation in the forcast error

plot.ts(Birth_TS_1arimaforecasts$residuals) 

plotForecastErrors(Birth_TS_1arimaforecasts$residuals)



mean(Birth_TS_1arimaforecasts$residuals) 
#The Forcaset errors are not normaly distributed so trying another model



#Model 3 using Auto Arima Funaction


auto.arima(Birth_TS_1)
auto.arima(Birth_TS_1,ic='bic')
#ARIMA(1,0,1) with non-zero mean 


# plot a  correlogram 
acf(Birth_TS_1diff1, lag.max=20) 
acf(Birth_TS_1diff1, lag.max=20, plot=FALSE) # 


# plot a  Partial correlogram

pacf(Birth_TS_1diff1, lag.max=20) 
# plot a partial correlogram 
pacf(Birth_TS_1diff1, lag.max=20, plot=FALSE)

Birth_TS_1arima_model2 <- arima(Birth_TS_1, order=c(0,1,0)) # fit an ARIMA(0,1,0) model 
Birth_TS_1arima_model2 


#Arime Model (0,1,0)
Birth_TS_1arimaforecasts2 <- forecast(Birth_TS_1arima_model2, h=5, level=c(99.5)) 
Birth_TS_1arimaforecasts2 



acf(Birth_TS_1arimaforecasts2$residuals, lag.max=20) 
Box.test(Birth_TS_1arimaforecasts2$residuals, lag=20, type="Ljung-Box")


plot.ts(Birth_TS_1arimaforecasts2$residuals) 

mean(Birth_TS_1arimaforecasts2$residuals) 

#This model might be better as lower Residue mean value and it is slighlty less biased as compare to previous model
