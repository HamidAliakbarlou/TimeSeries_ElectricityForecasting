#### NAIVE METHOD ####

#Load Libraries
library(astsa)
library(forecast)
library(timeSeries)

#Load initial data
load("nyisodata.RData")

# subset data of nyisodata for zone Genese
nyisoGenese <- nyisodata[ c('TimeStamp', 'GENESE')]

# missing data on row 9431
missingData<- which(is.na(nyisoGenese$GENESE))

# fill in missing data with mean of previous and next value
nyisoGenese$GENESE[missingData] <- (nyisoGenese$GENESE[missingData-1]
                              + nyisoGenese$GENESE[missingData+1])/2

nyisoGenese$TimeStamp <- as.POSIXct(nyisoGenese$TimeStamp, 
                                    format="%m/%d/%Y %H:%M:%S", 
                                    tz="America/New_York")


# Specifying format for input character string
date_format <- "%Y-%m-%d"

# Convert to Date format
date_obj <- as.Date(substr(nyisoGenese$TimeStamp,1,10), 
                    format = date_format)

# Verify the format
date_obj_2 <- strftime(date_obj, format = "%Y-%m-%d")


day  <- date_obj_2
hour <- substr(nyisoGenese$TimeStamp,12,13)
date <- paste(day, hour, sep="-")

GENESE <- timeSeries(nyisoGenese$GENESE, date, format="%Y/%m/%d-%H")

df <- data.frame(day=as.Date(day,"%Y-%m-%d"), GENESE=GENESE)

GENESE_Daily <- aggregate(GENESE ~ day, df, 
                          function(x) c(dailydemand = sum(x)))

Yt <- timeSeries(GENESE_Daily[[2]], GENESE_Daily[[1]],
                 format="%Y-%m-%d",
                 zone = "NewYork",
                 FinCenter = "NewYork")

#####################################################################
sdate  <- c(2020,1)
edate  <- c(2022,1)

# naive forecast (random walk)
ts_naive <- ts(Yt$TS.1,    start = c(2015, 1), frequency = 365)

# Compute bias, pbias, and MAPE
naivefo <-naive(ts_naive,h=1)
forecast <- window(naivefo$fitted, start=sdate, end=edate)
observed <- window(naivefo$x, start=sdate, end=edate)
bias1  <- mean(forecast-observed)
pbias1 <- mean((forecast-observed)/observed)*100
mape1  <- mean(abs((forecast-observed)/observed)*100)

naiveS <- snaive(ts_naive, h=1)
forecastS <- window(naiveS$fitted, start=sdate, end=edate) 
observedS <- window(naiveS$x, start=sdate, end=edate) 
biasS  <- mean(forecastS-observedS) 
pbiasS <- mean((forecastS-observedS)/observedS)*100
mapeS  <- mean(abs((forecastS-observedS)/observedS)*100)


# 3 days Moving Average
naive3t <- zoo::rollmean(ts_naive, 3, align="right")
naive3 <- naive(naive3t, h=1)
forecast3 <- window(naive3$fitted, start=sdate, end=edate)
observed3 <- window(ts_naive, start=sdate, end=edate)
bias3 <- mean(forecast3-observed3)
pbias3 <- mean((forecast3-observed3)/observed3)*100
mape3 <- mean(abs((forecast3-observed3)/observed3)*100)


# 7 days Moving Average
naive7t <- zoo::rollmean(ts_naive, 7, align="right")
naive7 <- naive(naive7t, h=1)
forecast7 <- window(naive7$fitted, start=sdate, end=edate)
observed7 <- window(ts_naive, start=sdate, end=edate)
bias7 <- mean(forecast7-observed7)
pbias7 <- mean((forecast7-observed7)/observed7)*100
mape7 <- mean(abs((forecast7-observed7)/observed7)*100)


# Show observed and forecasts as of Jan 2016
plot(observed, ylab="Demand (MW)",xlab="Time", lwd= 2)
lines(window(forecast, start=sdate), col="blue")
lines(forecastS, col="green")
lines(window(forecast3, start=sdate), col="magenta")
lines(window(forecast7, start=sdate), col="darkgoldenrod1")
legend("topright", 
       legend=c("observed","naive","seasonal naive",
                "3 days moving average","7 days moving average"),
       col=c("black","blue","green",
             "magenta","darkgoldenrod1"), lty=1, cex=.7)

cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])
print(accuracy(forecastS, observedS)[,1:5])
print(accuracy(forecast3, observed3)[,1:5])
print(accuracy(forecast7, observed7)[,1:5])
