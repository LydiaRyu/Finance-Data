
# Read dataset

library(readxl)
bok<- read.csv("BOK_macro.csv")
bok<-t(bok)
head(bok)

# Convert to time series data

bok.ts<-ts(bok, start = c(2011, 3), freq = 12)
head(bok.ts)

# Time plot

Employment.ts <- ts(bok[, 1], start = c(2011, 3), freq = 12)
Bond.ts <- ts(bok[, 2], start = c(2011, 3), freq = 12)

layout(1:2)
plot(Employment.ts, ylab = "Employment rate(%)")
plot(Bond.ts, ylab="Treasury bond yield(year%)")

# Autoplot

library(ggfortify)
library(forecast)


autoplot(Employment.ts) +
  ggtitle("Employment rate(10 years)") +
  xlab("Year") +
  ylab("Employment rate(%)")

autoplot(Bond.ts) +
  ggtitle("Treasury bond yield(10 years)") +
  xlab("Year") +
  ylab("Treasury bond yield(year%)")

# Calculate the annual mean using the aggregate function

Employment.annual<-aggregate(Employment.ts)/12
Bond.annual<-aggregate(Bond.ts)/12

layout(1:2)

plot(Employment.annual)
plot(Bond.annual)