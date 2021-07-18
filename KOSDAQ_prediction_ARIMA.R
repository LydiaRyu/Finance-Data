
# Data Importing

library(readxl)

kd_sc<- read.csv("KOSDAQ Semiconductor Historical Data.csv")
kd_fbt<- read.csv("KOSDAQ Food Beverage & Tobacco Historical Data.csv")
kd_fin<- read.csv("KOSDAQ Financials Historical Data.csv")


kd_sc.ts <- ts(kd_sc$Price, start = c(2018, 6), freq = 12)
kd_fbt.ts <- ts(kd_fbt$Price, start = c(2018, 6), freq = 12)
kd_fin.ts <- ts(kd_fin$Price, start = c(2018, 6), freq = 12)


# Time Plot

library(ggplot2)
plot(kd_sc.ts, xlab = "Time Plot of Semiconductor Sector", ylab = "")
plot(kd_fbt.ts, xlab = "Time Plot of FB&T Sector", ylab = "")
plot(kd_fin.ts, xlab = "Time Plot of financials Sector", ylab = "")


# ACF & PACF

par(mfrow=c(1,2))
acf(kd_sc.ts, lag.max = 36)
pacf(kd_sc.ts, lag.max = 36)

par(mfrow=c(1,2))
acf(kd_fbt.ts, lag.max = 36)
pacf(kd_fbt.ts, lag.max = 36)

par(mfrow=c(1,2))
acf(kd_fin.ts, lag.max = 36)
pacf(kd_fin.ts, lag.max = 36)
```

# Difference & ACF

kd_sc.ts.diff <- diff(kd_sc.ts)

par(mfrow=c(1,2))
plot(kd_sc.ts.diff)
acf(kd_sc.ts.diff)

kd_fbt.ts.diff <- diff(kd_fbt.ts)

par(mfrow=c(1,2))
plot(kd_fbt.ts.diff)
acf(kd_fbt.ts.diff)

kd_fin.ts.diff <- diff(kd_fin.ts)

par(mfrow=c(1,2))
plot(kd_fin.ts.diff)
acf(kd_fin.ts.diff)


# auto.arima(Semiconductor) 
library(forecast)
auto.arima(kd_sc.ts)

kd_sc.arima <- arima(kd_sc.ts, order =c(0,1,0))
kd_sc.arima

# Box-Ljung Test(Semiconductor)
Box.test(kd_sc.arima$residuals, lag =1, type = "Ljung")

# Forecasting(Semiconductor)
kd_sc.forecast <- forecast(kd_sc.arima, h=7)
kd_sc.forecast

plot(kd_sc.forecast)

# auto.arima(Food Beverage & Tobacco) 
auto.arima(kd_fbt.ts)

kd_fbt.arima <- arima(kd_fbt.ts, order =c(1,0,0))
kd_fbt.arima

# Box-Ljung Test(Food Beverage & Tobacco)
Box.test(kd_fbt.arima$residuals, lag =1, type = "Ljung")

# Forecasting(Food Beverage & Tobacco)
kd_fbt.forecast <- forecast(kd_fbt.arima, h=7)
kd_fbt.forecast

plot(kd_fbt.forecast)

# auto.arima(Financial)
auto.arima(kd_fin.ts)

kd_fin.arima <- arima(kd_fin.ts, order =c(0,2,1))
kd_fin.arima

# Box-Ljung Test(Financial)
Box.test(kd_fin.arima$residuals, lag =1, type = "Ljung")

# Forecasting(Financial)
kd_fin.forecast <- forecast(kd_fin.arima, h=7)
kd_fin.forecast

plot(kd_fin.forecast)
