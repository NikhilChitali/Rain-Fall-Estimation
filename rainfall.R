#Introduction + Dataset +
rain_ts <- ts(rainfall_area_annual_wt_India_1901_2015$ANNUAL)

#Plot data
plot.ts(rain_ts)


#Transformation of timeseries in case of seasonal or random fluctuationsr
rain_ts_log <- log(rain_ts)
plot(rain_ts_log)





####Part 2 :
#Decomposing Time Series
install.packages("TTR")
install.packages("xts")
update.packages(ask=FALSE)
library(TTR)

#If Fluctuations are present, then we have to smoothen it
rainval_ts <- ts(rain_ts)
plot.ts(rainval_ts)

#Smoothen timeseries using simple moving average
#Smoothen over 3 time period
rainval_sma3 <- SMA(rainval_ts,3)
plot.ts(rainval_sma3)

#Smoothen over 5 time period
rainval_sma5 <- SMA(rainval_ts,5)
plot.ts(rainval_sma5)

#Smoothen over 7 time period
rainval_sma7 <- SMA(rainval_ts,7)
plot.ts(rainval_sma7)

#Smoothen timeseries using Exponential moving average
#Smoothen over ratio=0.25
rainval_ema3_25 <- EMA(rainval_ts,3,ratio=0.25)
plot.ts(rainval_ema3_25)

#Smoothen over ratio=0.50
rainval_ema3_50 <- EMA(rainval_ts,3,ratio=0.50)
plot.ts(rainval_ema3_50)

#Smoothen over ratio=0.75
rainval_ema3_75 <- EMA(rainval_ts,3,ratio=0.75)
plot.ts(rainval_ema3_75)

plot.ts(rain_ts)

rain_ts_decomp <- decompose(rain_ts)
#This will give error for no seasonal component
#This proves that our dataset has no seasonality and model that are 
#apt to seasonal data does not hold good for our dataset




####part 3 -
#Choose Dataset 
rain_india <- read.csv(file.choose())
rain_india_ts <- ts(rain_india$ANNUAL,start = 1901, end = 2013)
plot.ts(rain_india_ts)

#Forecasting using HoltWinters
rain_india_ts_hw <- HoltWinters(rain_india_ts,beta = F,gamma = F)
rain_india_ts_hw

rain_india_ts_hw$fitted
update.packages(ask=FALSE)
install.packages("munsell")
install.packages("pkgconfig")

library(forecast) 

rain_india_ts_hw_fcst <- forecast:::forecast.HoltWinters(rain_india_ts_hw,h=10)
forecast:::plot.forecast(rain_india_ts_hw_fcst)
rain_india_ts_hw_fcst
#This will give constant value due to constant variance 
#Thus we cannot use the "HoltWinters" model where there is no
#trend or seasonal component

#If only trend component is present then,
rain_annual_forecast <- HoltWinters(rain_ts, gamma = F)
rain_annual_forecast
plot(rain_annual_forecast)

rain_annual_forecast_future <- forecast:::forecast.HoltWinters(rain_annual_forecast,h=10)
forecast:::plot.forecast(rain_annual_forecast_future)
rain_annual_forecast_future


#If trend and seasonal component is present then we can use
#HoltWinters model




####Part 4 :

#Using ACF error checking
acf(na.exclude(rain_india_ts_hw_fcst$residuals), lax.max=10)
Box.test(rain_india_ts_hw_fcst$residuals, lag = 10, type = "Ljung-Box")
plot.ts(rain_india_ts_hw_fcst$residuals)
hist(rain_india_ts_hw_fcst$residuals, col = 'red')



###Part 5 :
#Using ARIMA for Forecasting
plot.ts(rain_ts)

#differncing with lag=1
rain_diff1 <- diff(rain_india_ts, differences = 1)
plot.ts(rain_diff1)

#differncing with lag=2
rain_diff2 <- diff(rain_india_ts, differences = 2)
plot.ts(rain_diff2)

#differncing with lag=3
rain_diff3 <- diff(rain_india_ts, differences = 3)
plot.ts(rain_diff3)

#differncing with lag=4
rain_diff4 <- diff(rain_india_ts, differences = 4)
plot.ts(rain_diff4)


#plot the grid


###Part 6 :
#Differencing using Auto Co-relation function
acf(rain_diff1, lag.max=20)    
acf(rain_diff2, lag.max=20) 
acf(rain_diff3, lag.max=20) 
acf(rain_diff4, lag.max=20) 
pacf(rain_diff1, lag.max=20)      
pacf(rain_diff2, lag.max=20)  
pacf(rain_diff3, lag.max=20)  
pacf(rain_diff4, lag.max=20)  

#c(2,1,1)





###Part 7:
#Choose Dataset 
#rain_india <- read.csv(file.choose())
rain_india_ts <- ts(rain_india$ANNUAL,start = 1901, end = 2013)
plot.ts(rain_india_ts)

rain_model <- arima(rain_india_ts, order = c(2,1,1))
rain_model
rain_arima <- forecast::auto.arima(rain_india_ts)
rain_arima
Box.test(rain_arima$residuals, lag=40, type="Ljung-Box")
#if here p>0.05 then we can say that there is acceptable correlation 
#and hence ARIMA model holds good for forecasting


#Forecasting for next 4 years
rain_model_forecast <- forecast::forecast(rain_arima,h=10)
rain_model_forecast
plot(rain_model_forecast)

#Errors in forecasting
forecast::accuracy(rain_arima)

