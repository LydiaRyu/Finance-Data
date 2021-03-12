
# Analyze Hyundai Mobis stock price 
  
#install.packages("tseries")
#install.packages("quantmod")
library(tseries)
library(quantmod)

mobis<-get.hist.quote("012330.ks", quote="AdjClose",quiet=TRUE, 
                      start=as.Date('2010-01-04'), end=as.Date('2019-12-27'))
mobis <- na.omit(mobis)

# Simple rate of return & Logarithmic rate of return
  
ret_simple<-diff(mobis)/lag(mobis, k=-1)*100
plot(ret_simple, main="Simple rate of return(Hyundai Mobis)")
summary(ret_simple)

ret_log<-diff(log(mobis))*100
plot(ret_log, main= "Logarithmic rate of return(Hyundai Mobis)")
summary(ret_log)

# Stock price chart

H_mobis<-getSymbols("012330.KS", from="2010-01-04", to="2019-12-27",auto.assign = F)
H_mobis <- na.omit(H_mobis)
colnames(H_mobis)<-c("op","hi","lo","close","vol","adj")
head(H_mobis)

chartSeries(H_mobis, theme="white", TA= NULL)
candleChart(H_mobis, theme="white")

# Bollinger Bands & Commodity Channel Index  
  
H_mobis=getSymbols("012330.KS",from="2010-01-04", to="2019-12-27", auto.assign = F)
H_mobis <- na.omit(H_mobis)
chartSeries(H_mobis,theme = "white")

addBBands()

addCCI()

# Identify ARIMA Model and forecast Hyundai Mobis stock price in 5 days
  
#install.packages("TTR")
#install.packages("forecast")
#install.packages("tseries")
library("TTR")
library("forecast")
library("tseries")


H_mobis=get.hist.quote("012330.KS", start="2010-01-04", end="2019-12-27", quote="Close")
H_mobis <- na.omit(H_mobis)
plot(H_mobis)


H_mobis_ts<-ts(H_mobis)
plot.ts(H_mobis_ts)

H_mobis_diff<- diff(H_mobis_ts, differences = 1)
plot.ts(H_mobis_diff)

acf(H_mobis_diff, lag.max = 20) 

pacf(H_mobis_diff, lag.max = 20)

H_mobis_arima<- arima(H_mobis_ts, order = c(1,1,2))
H_mobis_fcast<- forecast(H_mobis_arima, h=5)
plot(H_mobis_fcast)


# Compare with 'auto.arima'
  
auto.arima(H_mobis)
H_mobis_arima2<- arima(H_mobis_ts, order = c(2,1,1))
H_mobis_fcast2<- forecast(H_mobis_arima2, h=5)
plot(H_mobis_fcast2)


