# Read last 5 years(2016) of data from Yahoo Finance data
# Netflix shares

NFLX <- read.csv("NFLX.csv")

# Convert Netflix data to time series dataset

NFLX.ts<- ts(NFLX$Close, start = c(2016, 1), freq = 12)

# Decomposition of Netflix data

NFLX.decom.add <- decompose(NFLX.ts, type = "additive")
NFLX.decom.mult <- decompose(NFLX.ts, type = "mult")


plot(NFLX.decom.add)
plot(NFLX.decom.mult)


# Auto correlation about random factor

ggAcf(NFLX.decom.add$random)
ggAcf(NFLX.decom.mult$random)