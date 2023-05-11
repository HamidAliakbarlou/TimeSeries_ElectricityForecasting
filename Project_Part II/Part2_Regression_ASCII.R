#################### Load libraries ####################
library(timeSeries)
library(forecast)
library(lmtest)

#################### Prepare NYISO data ####################

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

# max daily peak
df <- data.frame(day=as.Date(day,"%Y-%m-%d"), GENESE=GENESE)

# aggregate df
GENESE_Daily_max <- aggregate(GENESE ~ day, df, 
                              function(x) c(dailydemand = max(x)))

Yt_max <- timeSeries(GENESE_Daily_max[[2]], GENESE_Daily_max[[1]],
                     format="%Y-%m-%d",
                     zone = "NewYork",
                     FinCenter = "NewYork")

demand_max_daily <- timeSeries(GENESE_Daily_max[[2]], 
                               GENESE_Daily_max[[1]],
                               format="%Y-%m-%d",
                               zone = "NewYork",
                               FinCenter = "NewYork")

## To make consistent with plotting part of code
GENESE_Daily <- GENESE_Daily_max

#--------------------------------------------------------------------
# Imputation
# - need to make sure no conflict with other code
# - we impute from the peak of previous and next week
# - from August 03 to August 08 
# -- for 1 day before disaster, disaster and 3 days after disaster
# -- replacing 5 days only
#--------------------------------------------------------------------
find_row_indices <- function(df, dates) {
  row_indices <- sapply(dates,
                        function(date)
                          which(rownames(df) == as.Date(date)))
  return(row_indices)
}

demand_max_daily_impute <- demand_max_daily

imputedays.previous <- seq(from=as.POSIXct("2020-07-27 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                           to=as.POSIXct("2020-08-01 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                           by="day")

imputedays.current <- seq(from=as.POSIXct("2020-08-03 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                          to=as.POSIXct("2020-08-08 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                          by="day")

imputedays.after <- seq(from=as.POSIXct("2020-08-10 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                        to=as.POSIXct("2020-08-15 00:00:00", 
                    format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                        by="day")

# index 2035 2036 2037 2038 2039 2040
# values 1878.657 1702.618 1574.064 1603.221 1557.119 1563.440
imputedays.indexPrevious <- find_row_indices(demand_max_daily_impute,
                                             imputedays.previous)

# index 2049 2050 2051 2052 2053 2054
# values 1945.496 1844.569 1665.711 1713.288 1608.730 1531.894
imputedays.indexAfter <- find_row_indices(demand_max_daily_impute,
                                          imputedays.after)

# index 2042 2043 2044 2045 2046 2047
imputedays.indexCurrent <- find_row_indices(demand_max_daily_impute,
                                            imputedays.current)

# mean
imputedays.imputeValues <- mapply(function(x, y) { (x + y) / 2 },
                    demand_max_daily_impute[imputedays.indexPrevious], 
                    demand_max_daily_impute[imputedays.indexAfter])

# values 1611.134 1416.236 1362.905 1329.931 1384.207 1432.184
demand_max_daily_impute[imputedays.indexCurrent]

# Impute values
# - Replace the values in demand_max_daily_impute 
# with the values in imputedays.imputeValues
demand_max_daily_impute[imputedays.indexCurrent]<-
  imputedays.imputeValues

# values 1912.076 1773.594 1619.887 1658.255 1582.925 1547.667
demand_max_daily_impute[imputedays.indexCurrent]


# --- end change ---



Yt <- demand_max_daily_impute


#################### Explanatory Variables ####################

### Import Stations Data
Stations <- read.csv("3 Main Stations_updated.csv")

### Summaries
names(Stations)
# "STATION"     -- Station ID                  
# "NAME"        -- Station Location Name
# "LATITUDE"
# "LONGITUDE"
# "ELEVATION"
# "DATE"        -- Assuming in Eastern Standard Time
# "AWND"        -- Average Daily Wind Speed
# "PRCP"        -- Precipitation 
# "SNOW"        -- Snowfall
# "SNWD"        -- Snow Depth
# "TAVG"        -- Average Daily Temperature
# "TMAX"        -- Maximum Daily Temperature
# "TMIN"        -- Minimum Daily Temperature

range(Stations$DATE)
#"2015-01-01" "2022-12-31"

nrow(Stations)
# ("2015-01-01" , "2022-12-31" )-> 8 years
# 365 days * 8 years + 2 (leap years 2016 and 2020) = 2922 
# 8766 observations/ 3 stations = 2922

str(Stations)
# Variable types
# $ STATION  : chr  
# $ NAME     : chr  
# $ LATITUDE : num  
# $ LONGITUDE: num  
# $ ELEVATION: num  
# $ DATE     : chr  
# $ AWND     : num  
# $ PRCP     : num  
# $ SNOW     : num  
# $ SNWD     : num  
# $ TAVG     : int  
# $ TMAX     : int  
# $ TMIN     : int  

summary(Stations)
# Snowfall, Snowdepth and Average Daily Temperature are
# missing 33% of the time, we will decide what to do with them

Stations_Subset <- Stations[ c('STATION', 
                               'NAME',
                               'DATE',
                               'LATITUDE',
                               'LONGITUDE', 
                               'ELEVATION',
                               'AWND',
                               'PRCP', 
                               'SNOW',
                               'SNWD',
                               'TAVG', 
                               'TMAX', 
                               'TMIN')] 


### Impute the missing values 
Elmira <- Stations_Subset[Stations_Subset$NAME == 
                    "ELMIRA CORNING REGIONAL AIRPORT, NY US",]

Rochester <- Stations_Subset[Stations_Subset$NAME == 
                    "ROCHESTER GREATER INTERNATIONAL, NY US",]

Syracuse <- Stations_Subset[Stations_Subset$NAME == 
                    "SYRACUSE HANCOCK INTERNATIONAL AIRPORT, NY US",]

summary(Elmira) # majority of the NA from Elmira
summary(Rochester)
summary(Syracuse)

# Latitude, Longitude and Elevation same across cols
table(Elmira$LATITUDE)
table(Rochester$LATITUDE)
table(Syracuse$LATITUDE)

table(Elmira$LONGITUDE)
table(Rochester$LONGITUDE)
table(Syracuse$LONGITUDE)

table(Elmira$ELEVATION)
table(Rochester$ELEVATION)
table(Syracuse$ELEVATION)

# We will impute the missing data in Rochester and Syracuse
#by the median because they contain less than 2% of missing values

#Rochester imputation (1 NA in avg wind speed var)
Rochester$AWND[is.na(Rochester$SNOW)] <- 0.0000 #median
Rochester$AWND[is.na(Rochester$SNWD)] <- 0.0000 #median
Rochester$AWND[is.na(Rochester$TAVG)] <- 51.00 #median

#Syracuse imputation (1 NA in snow depth & Max Temperature Vars)
Syracuse$SNWD[is.na(Syracuse$SNWD)] <- 0.000 #median
Syracuse$TMAX[is.na(Syracuse$TMAX)] <- 61.00 #median


# As for Elmira, it only carries a weight of 5% when 
# proportionnaly weighing the stations to aggregate the datasets. 

# Thus we can set to 0 the variables that are all 
# NA so hey have no impact on the weather aggregation

# And we will impute with the median the variables
# that have less than 0.5% missing

Elmira$SNOW[is.na(Elmira$SNOW)] <- 0.000 
Elmira$SNWD[is.na(Elmira$SNWD)] <- 0.000
Elmira$TAVG[is.na(Elmira$TAVG)] <- 0.000

Elmira$AWND[is.na(Elmira$AWND)] <- 5.140 #median
Elmira$PRCP[is.na(Elmira$PRCP)] <- 0.000 #median
Elmira$TMAX[is.na(Elmira$TMAX)] <- 63.00 #median
Elmira$TMIN[is.na(Elmira$TMIN)] <- 37.00 #median

###Create Extra Variables
#HDD

Elmira$HDD40 <- sapply(Elmira$TMIN, function(x) max(0, 40 - x))
Elmira$HDD42 <- sapply(Elmira$TMIN, function(x) max(0, 42 - x))
Elmira$HDD45 <- sapply(Elmira$TMIN, function(x) max(0, 45 - x))
Elmira$HDD50 <- sapply(Elmira$TMIN, function(x) max(0, 50 - x))
Elmira$HDD62 <- sapply(Elmira$TMIN, function(x) max(0, 62 - x))
Elmira$HDD65 <- sapply(Elmira$TMIN, function(x) max(0, 65 - x))
Elmira$HDD68 <- sapply(Elmira$TMIN, function(x) max(0, 68 - x))

Rochester$HDD40 <- sapply(Rochester$TMIN, function(x) max(0, 40 - x))
Rochester$HDD42 <- sapply(Rochester$TMIN, function(x) max(0, 42 - x))
Rochester$HDD45 <- sapply(Rochester$TMIN, function(x) max(0, 45 - x))
Rochester$HDD50 <- sapply(Rochester$TMIN, function(x) max(0, 50 - x))
Rochester$HDD62 <- sapply(Rochester$TMIN, function(x) max(0, 62 - x))
Rochester$HDD65 <- sapply(Rochester$TMIN, function(x) max(0, 65 - x))
Rochester$HDD68 <- sapply(Rochester$TMIN, function(x) max(0, 68 - x))

Syracuse$HDD40 <- sapply(Syracuse$TMIN, function(x) max(0, 40 - x))
Syracuse$HDD42 <- sapply(Syracuse$TMIN, function(x) max(0, 42 - x))
Syracuse$HDD45 <- sapply(Syracuse$TMIN, function(x) max(0, 45 - x))
Syracuse$HDD50 <- sapply(Syracuse$TMIN, function(x) max(0, 50 - x))
Syracuse$HDD62 <- sapply(Syracuse$TMIN, function(x) max(0, 62 - x))
Syracuse$HDD65 <- sapply(Syracuse$TMIN, function(x) max(0, 65 - x))
Syracuse$HDD68 <- sapply(Syracuse$TMIN, function(x) max(0, 68 - x))

#CDD

Elmira$CDD55 <- sapply(Elmira$TMAX, function(x) max(0, x - 55))
Elmira$CDD60 <- sapply(Elmira$TMAX, function(x) max(0, x - 60))
Elmira$CDD65 <- sapply(Elmira$TMAX, function(x) max(0, x - 65))
Elmira$CDD68 <- sapply(Elmira$TMAX, function(x) max(0, x - 68))
Elmira$CDD70 <- sapply(Elmira$TMAX, function(x) max(0, x - 70))
Elmira$CDD72 <- sapply(Elmira$TMAX, function(x) max(0, x - 72))
Elmira$CDD75 <- sapply(Elmira$TMAX, function(x) max(0, x - 75))
Elmira$CDD78 <- sapply(Elmira$TMAX, function(x) max(0, x - 78))

Rochester$CDD55 <- sapply(Rochester$TMAX, function(x) max(0, x - 55))
Rochester$CDD60 <- sapply(Rochester$TMAX, function(x) max(0, x - 60))
Rochester$CDD65 <- sapply(Rochester$TMAX, function(x) max(0, x - 65))
Rochester$CDD68 <- sapply(Rochester$TMAX, function(x) max(0, x - 68))
Rochester$CDD70 <- sapply(Rochester$TMAX, function(x) max(0, x - 70))
Rochester$CDD72 <- sapply(Rochester$TMAX, function(x) max(0, x - 72))
Rochester$CDD75 <- sapply(Rochester$TMAX, function(x) max(0, x - 75))
Rochester$CDD78 <- sapply(Rochester$TMAX, function(x) max(0, x - 78))

Syracuse$CDD55 <- sapply(Syracuse$TMAX, function(x) max(0, x - 55))
Syracuse$CDD60 <- sapply(Syracuse$TMAX, function(x) max(0, x - 60))
Syracuse$CDD65 <- sapply(Syracuse$TMAX, function(x) max(0, x - 65))
Syracuse$CDD68 <- sapply(Syracuse$TMAX, function(x) max(0, x - 68))
Syracuse$CDD70 <- sapply(Syracuse$TMAX, function(x) max(0, x - 70))
Syracuse$CDD72 <- sapply(Syracuse$TMAX, function(x) max(0, x - 72))
Syracuse$CDD75 <- sapply(Syracuse$TMAX, function(x) max(0, x - 75))
Syracuse$CDD78 <- sapply(Syracuse$TMAX, function(x) max(0, x - 78))

#Average Wind Chill

Elmira$AvgWindChill <- 
  ifelse(((Elmira$TMIN + Elmira$TMAX)/2) < 50, 
         sqrt(Elmira$AWND)*(50-((Elmira$TMIN + Elmira$TMAX)/2)),
         0)

Rochester$AvgWindChill <- 
  ifelse(((Rochester$TMIN + Rochester$TMAX)/2) < 50, 
      sqrt(Rochester$AWND)*(50-((Rochester$TMIN + Rochester$TMAX)/2)),
         0)

Syracuse$AvgWindChill <- 
  ifelse(((Syracuse$TMIN + Syracuse$TMAX)/2) < 50,
         sqrt(Syracuse$AWND)*(50-((Syracuse$TMIN + Syracuse$TMAX)/2)),
         0)

#make sure the date columns have the same values
table(Elmira$DATE == Rochester$DATE) # all true
table(Elmira$DATE == Syracuse$DATE)  # all true
table(Rochester$DATE == Syracuse$DATE) # all true

# Weights according to NYISO :Elmira(5%),Rochester(85%),Syracuse(10%)
Elmira_Weights <- rep(0.05, 2922)
Rochester_Weights <- rep(0.85, 2922)
Syracuse_Weights <- rep(0.10, 2922)

# create proportionnaly weighted vars
DATE <- Rochester$DATE

AvgWind <- (Elmira$AWND * Elmira_Weights + 
              Rochester$AWND * Rochester_Weights + 
              Syracuse$AWND * Syracuse_Weights)

Precipitation <- (Elmira$PRCP * Elmira_Weights + 
                    Rochester$PRCP * Rochester_Weights + 
                    Syracuse$PRCP * Syracuse_Weights)

SnowDepth <- (Elmira$SNWD * Elmira_Weights + 
                Rochester$SNWD * Rochester_Weights + 
                Syracuse$SNWD * Syracuse_Weights)

SnowFall <- (Elmira$SNOW * Elmira_Weights + 
               Rochester$SNOW * Rochester_Weights + 
               Syracuse$SNOW * Syracuse_Weights)

Avg_Temp <- (Elmira$TAVG * Elmira_Weights + 
               Rochester$TAVG * Rochester_Weights + 
               Syracuse$TAVG * Syracuse_Weights)

Max_Temp <- (Elmira$TMAX * Elmira_Weights + 
               Rochester$TMAX * Rochester_Weights + 
               Syracuse$TMAX * Syracuse_Weights)

Min_Temp <- (Elmira$TMIN * Elmira_Weights + 
               Rochester$TMIN * Rochester_Weights + 
               Syracuse$TMIN * Syracuse_Weights)

HDD40 <- (Elmira$HDD40 * Elmira_Weights + 
            Rochester$HDD40 * Rochester_Weights + 
            Syracuse$HDD40 * Syracuse_Weights)

HDD42 <- (Elmira$HDD42 * Elmira_Weights + 
            Rochester$HDD42 * Rochester_Weights + 
            Syracuse$HDD42 * Syracuse_Weights)

HDD45 <- (Elmira$HDD45 * Elmira_Weights + 
            Rochester$HDD45 * Rochester_Weights + 
            Syracuse$HDD45 * Syracuse_Weights)

HDD50 <- (Elmira$HDD50 * Elmira_Weights + 
            Rochester$HDD50 * Rochester_Weights + 
            Syracuse$HDD50 * Syracuse_Weights)

HDD62 <- (Elmira$HDD62 * Elmira_Weights + 
            Rochester$HDD62 * Rochester_Weights + 
            Syracuse$HDD62 * Syracuse_Weights)

HDD65 <- (Elmira$HDD65 * Elmira_Weights + 
            Rochester$HDD65 * Rochester_Weights + 
            Syracuse$HDD65 * Syracuse_Weights)

HDD68 <- (Elmira$HDD68 * Elmira_Weights + 
            Rochester$HDD68 * Rochester_Weights + 
            Syracuse$HDD68 * Syracuse_Weights)

CDD55 <- (Elmira$CDD55 * Elmira_Weights + 
            Rochester$CDD55 * Rochester_Weights + 
            Syracuse$CDD55 * Syracuse_Weights)

CDD60 <- (Elmira$CDD60 * Elmira_Weights + 
            Rochester$CDD60 * Rochester_Weights + 
            Syracuse$CDD60 * Syracuse_Weights)

CDD65 <- (Elmira$CDD65 * Elmira_Weights + 
            Rochester$CDD65 * Rochester_Weights + 
            Syracuse$CDD65 * Syracuse_Weights)

CDD68 <- (Elmira$CDD68 * Elmira_Weights + 
            Rochester$CDD68 * Rochester_Weights + 
            Syracuse$CDD68 * Syracuse_Weights)

CDD70 <- (Elmira$CDD70 * Elmira_Weights + 
            Rochester$CDD70 * Rochester_Weights + 
            Syracuse$CDD70 * Syracuse_Weights)

CDD72 <- (Elmira$CDD72 * Elmira_Weights + 
            Rochester$CDD72 * Rochester_Weights + 
            Syracuse$CDD72 * Syracuse_Weights)

CDD75 <- (Elmira$CDD75 * Elmira_Weights + 
            Rochester$CDD75 * Rochester_Weights + 
            Syracuse$CDD75 * Syracuse_Weights)

CDD78 <- (Elmira$CDD78 * Elmira_Weights + 
            Rochester$CDD78 * Rochester_Weights + 
            Syracuse$CDD78 * Syracuse_Weights)

AvgWindChill <- (Elmira$AvgWindChill * Elmira_Weights + 
                   Rochester$AvgWindChill * Rochester_Weights + 
                   Syracuse$AvgWindChill * Syracuse_Weights)


############## plot explanatory varialbes vs demand ##############

# Create time series for mean temperature, heating degree days 
# and cooling degree days variables.
Tt   <- timeSeries(Avg_Temp, 
                   DATE, format="%Y-%m-%d", 
                   zone = "NewYork", FinCenter = "NewYork")

TMINt <- timeSeries(Min_Temp, 
                    DATE, format="%Y-%m-%d", 
                    zone = "NewYork", FinCenter = "NewYork")

TMAXt <- timeSeries(Max_Temp, 
                    DATE, format="%Y-%m-%d", 
                    zone = "NewYork", FinCenter = "NewYork")

HDD40t <- timeSeries(HDD40, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD42t <- timeSeries(HDD42, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD45t <- timeSeries(HDD45, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD50t <- timeSeries(HDD50, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD62t <- timeSeries(HDD62, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD65t <- timeSeries(HDD65, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

HDD68t <- timeSeries(HDD68, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD55t <- timeSeries(CDD55, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD60t <- timeSeries(CDD60, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD65t <- timeSeries(CDD65, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD68t <- timeSeries(CDD68, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD70t <- timeSeries(CDD70, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD72t <- timeSeries(CDD72, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD75t <- timeSeries(CDD75, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

CDD78t <- timeSeries(CDD78, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

WNt <- timeSeries(AvgWindChill, 
                  DATE, format="%Y-%m-%d", 
                  zone = "NewYork", FinCenter = "NewYork") 

SFt <- timeSeries(SnowFall, 
                  DATE, format="%Y-%m-%d", 
                  zone = "NewYork", FinCenter = "NewYork")

SDt <- timeSeries(SnowDepth, 
                  DATE, format="%Y-%m-%d", 
                  zone = "NewYork", FinCenter = "NewYork")

PRCPt <- timeSeries(Precipitation, 
                    DATE, format="%Y-%m-%d", 
                    zone = "NewYork", FinCenter = "NewYork")

### after plotting, click zoom to see better detail

#plot temperature vs daily demand
par(cex.axis = 1.6, cex.lab= 1.45)
plot(series(Tt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Average Daily Temperature (deg Farenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Average Daily Temperature", cex.lab= 2)
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot TMIN vs daily demand
plot(series(TMINt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Minimum Daily Temperature (deg Farenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Minimum Daily Temperature")
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot TMAX vs daily demand
plot(series(TMAXt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Maximum Daily Temperature (deg Farenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Maximum Daily Temperature")
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot CDD vs daily demand

par(mfrow=c(2,2))
plot(series(CDD55t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD55 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 55 degrees Fahrenheit ")

plot(series(CDD60t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD60 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 60 degrees Fahrenheit ")

plot(series(CDD70t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD70 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 70 degrees Fahrenheit ")

plot(series(CDD72t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD72 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 72 degrees Fahrenheit ")

#plot HDD vs daily demand

par(mfrow=c(2,2))
plot(series(HDD40t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD40 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 40 degrees Fahrenheit ")

plot(series(HDD42t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD42 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 42 degrees Fahrenheit ")

plot(series(HDD45t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD45 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 45 degrees Fahrenheit ")

plot(series(HDD50t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD50 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 50 degrees Fahrenheit ")

#plot lag-HDD cs daily demand
par(mfrow=c(1,2))
plot(lag(HDD45t,1), Yt, 
     xlab="lag-1 HDD45",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-1 HDD45 ")
plot(lag(HDD45t,2), Yt, 
     xlab="lag-2 HDD45",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-2 HDD45 ")

#plot lag-CDD cs daily demand
par(mfrow=c(1,2))
plot(lag(CDD72t,1), Yt, 
     xlab="lag-1 CDD72",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-1 CDD72 ")
plot(lag(CDD72t,2), Yt, 
     xlab="lag-2 CDD72",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-2 CDD72 ")


#plot windchill vs daily demand
par(mfrow=c(1,1))
plot(series(WNt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Windchill when Temperature Under 
     50 Degrees Fahrenheit (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Average Windchill")
legend("bottomright", cex = 0.5 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot windchill lag vs daily demand
par(mfrow=c(2,2))
plot(lag(WNt,1), Yt, 
     xlab="lag-1 Windchill when Temperature Under 
     50 Degrees Fahrenheit (deg Fahrenheit) ",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-1")

plot(lag(WNt,2), Yt, 
     xlab="lag-2 Windchill when Temperature Under 
     50 Degrees Fahrenheit (deg Fahrenheit)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-2")

plot(lag(WNt,3), Yt, 
     xlab="lag-3 Windchill when Temperature Under 
     50 Degrees Fahrenheit (deg Fahrenheit)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-3")

plot(lag(WNt,4), Yt, 
     xlab="lag-4 Windchill when Temperature Under 
     50 Degrees Fahrenheit (deg Fahrenheit)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-4")

#plot snowdepth  vs daily demand
par(mfrow=c(1,1))
plot(series(SDt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Snowdepth (inches)", pch=23)
title("Daily Electricity Demand in Genese vs Snowdepth")

#plot Snowdepth lag vs daily demand
par(mfrow=c(2,2))
plot(lag(SDt,1), Yt, 
     xlab="lag-1 Snowdepth (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowdepth with lag-1")

plot(lag(SDt,2), Yt, 
     xlab="lag-2 Snowdepth (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowdepth with lag-2")

plot(lag(SDt,3), Yt, 
     xlab="lag-3 Snowdepth (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowdepth with lag-3")

plot(lag(SDt,4), Yt, 
     xlab="lag-4 Snowdepth (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowdepth with lag-4")

#plot snowfall vs daily demand
par(mfrow=c(1,1))
plot(series(SFt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Snowfall (inches)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Snowfall (inches)")


#plot Snowfall lag vs daily demand
par(mfrow=c(2,2))
plot(lag(SFt,1), Yt, 
     xlab="lag-1 Snowfall",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowfall with lag-1 (inches)")

plot(lag(SFt,2), Yt, 
     xlab="lag-2 Snowfall",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowfall with lag-2 (inches)")

plot(lag(SFt,3), Yt, 
     xlab="lag-3 Snowfall",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowfall with lag-3 (inches)")

plot(lag(SFt,4), Yt, 
     xlab="lag-4 Snowfall",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Snowfall with lag-4 (inches)")

#plot precipitation vs daily demand
par(mfrow=c(1,1))
plot(series(PRCPt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Precipitation (inches)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Precipitation ")

#plot Precipitation lag vs daily demand
par(mfrow=c(2,2))
plot(lag(PRCPt,1), Yt, 
     xlab="lag-1 Precipitation (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Precipitation with lag-1")

plot(lag(PRCPt,2), Yt, 
     xlab="lag-2 Precipitation (inches)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Precipitation with lag-2")

plot(lag(PRCPt,3), Yt, 
     xlab="lag-3 Precipitation (inches) ",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Precipitation with lag-3")

plot(lag(PRCPt,4), Yt, 
     xlab="lag-4 Precipitation (inches) ",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Precipitation with lag-4 ")


# Day of the week effect
dayofweek <- substr(weekdays(strptime(DATE,"%Y-%m-%d")), 1, 3)
dow <- ifelse(dayofweek=="Sun", 1, 
       ifelse(dayofweek=="Mon", 2,
       ifelse(dayofweek=="Tue", 3, 
       ifelse(dayofweek=="Wed", 4,
       ifelse(dayofweek=="Thu", 5, 
       ifelse(dayofweek=="Fri", 6, 7))))))

par(mfrow=c(1,1))
boxplot(series(Yt) ~ dow,
        names=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        ylab="Daily Demand in GENESE (MW/h)")
title("Boxplot of Daily Electricity Demand 
      in Genese per Day of the Week")

############### plots to keep ############### 
pdf("part1_corrections.pdf")

#plot TAVG vs daily demand
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(Tt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Peak Hourly Load (MW/h)",
     xlab="Average Daily Temperature (deg Farenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) vs 
      Average Daily Temperature",)
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot TMIN vs daily demand
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(TMINt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Peak Hourly Load (MW/h)",
     xlab="Minimum Daily Temperature (deg Farenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) vs 
      Minimum Daily Temperature")
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
#plot TMAX vs daily demand
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(TMAXt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Peak Hourly Load (MW/h)",
     xlab="Maximum Daily Temperature (deg Farenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) vs 
      Maximum Daily Temperature")
legend("bottomleft", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
# plot CDD72
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(CDD72t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Peak Hourly Load (MW/h)",
     xlab="CDD72 (deg Fahrenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) 
      vs CDD72")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
# plot HDD45
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(HDD45t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Peak Hourly Load (MW/h)", 
     xlab="HDD45 (deg Fahrenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) 
      vs HDD45 ")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     
# plot HDD45 lag1 and lag2
par(mfrow=c(1,1))
plot(lag(HDD45t,1), Yt, 
     xlab="lag-1 HDD45",
     ylab="Daily Peak Hourly Load (MW/h)")
title("Peak Hourly Load for the Day 
      in GENESE (MW/h) 
      vs lag-1 HDD45 ")

par(mfrow=c(1,1))
plot(lag(HDD45t,2), Yt, 
     xlab="lag-2 HDD45",
     ylab="Daily Peak Hourly Load (MW/h)")
title("Peak Hourly Load for the Day 
      in GENESE (MW/h) 
      vs lag-2 HDD45 ")

#plot CDD72 lag-1 and lag2
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(lag(CDD72t,1), Yt, 
     xlab="lag-1 CDD72",
     ylab=" Daily Peak Hourly Load (MW/h)")
title("Peak Hourly Load for the Day in GENESE (MW/h) 
      vs lag-1 CDD72 ")

par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(lag(CDD72t,2), Yt, 
     xlab="lag-2 CDD72",
     ylab=" Daily Peak Hourly Load (MW/h)")
title("Peak Hourly Load for the Day in GENESE (MW/h) 
      vs lag-2 CDD72 ")

#plot windchill vs daily demand
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(WNt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Peak Hourly Load (MW/h)",
     xlab="Windchill When Under 50 F. (deg Fahrenheit)", pch=23)
title("Peak Hourly Load for the Day in GENESE (MW/h) 
      vs Average Windchill")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
                     "cyan", "orange", "purple", "black"), lty = 1)
                     

dev.off(dev.cur())

################## PART 2#######################

### Add dummy variables missing from part 1###

# days dummy variables
days <- dayofweek

# months dummy variables
monthofyear = as.Date(DATE,format = "%Y-%m-%d")
monthofyear = format(monthofyear,"%m")
months <- ifelse(monthofyear=="01", 1, 
          ifelse(monthofyear=="02", 2,
          ifelse(monthofyear=="03", 3, 
          ifelse(monthofyear=="04", 4,
          ifelse(monthofyear=="05", 5, 
          ifelse(monthofyear=="06", 6,
          ifelse(monthofyear=="07", 7,
          ifelse(monthofyear=="08", 8, 
          ifelse(monthofyear=="09", 9,
          ifelse(monthofyear=="10", 10, 
          ifelse(monthofyear=="11", 11, 12)))))))))))

# holiday dummy variables
holidays <- c("2015-01-01", "2015-05-25", "2015-07-03", 
              "2015-09-07", "2015-11-26", "2015-12-25",
              "2016-01-01", "2016-05-30", "2016-07-04", 
              "2016-09-05", "2016-11-24", "2016-12-26",
              "2017-01-02", "2017-05-29", "2017-07-04", 
              "2017-09-04", "2017-11-23", "2017-12-25",
              "2018-01-01", "2018-05-28", "2018-07-04", 
              "2018-09-03", "2018-11-22", "2018-12-25",
              "2019-01-01", "2019-05-27", "2019-07-04", 
              "2019-09-02", "2019-11-28", "2019-12-25",
              "2020-01-01", "2020-05-25", "2020-07-03", 
              "2020-09-07", "2020-11-26", "2020-12-25",
              "2021-01-01", "2021-05-31", "2021-07-05", 
              "2021-09-06", "2021-11-25", "2021-12-24",
              "2022-01-02", "2022-05-30", "2022-07-04", 
              "2022-09-05", "2022-11-24", "2022-12-26")

holidays <- as.Date(holidays)
prior_days <- holidays - 1
next_days <- holidays + 1

holidays_df = data.frame(DATE)
holidays_df$holidays = ifelse(DATE %in% as.character(holidays), 1, 0)
holidays_df$prior = ifelse(DATE %in% as.character(prior_days), 1, 0)
holidays_df$Next = ifelse(DATE %in% as.character(next_days), 1, 0)

Holidayt = timeSeries(holidays_df$holidays ,DATE)
priort = timeSeries(holidays_df$prior ,DATE)
Nextt = timeSeries(holidays_df$Next ,DATE)


###################Diagnostics for Lm ######################

pdf("diagnostics.pdf")

# linear model with temp only
mreg0 <- lm(Yt ~ Tt, x=T, y=T)
plot(mreg0$x[,2],mreg0$y,
     ylab="peak demand in Genese (MW/h)",xlab="Temperature (deg F.)",
     main="2015-2022: All data")
points(mreg0$x[,2],mreg0$fitted.values,col="red")
legend("top",legend=c("observed","fitted"),col=c("black","red"),
       pch=1)
print(summary(mreg0))

#linear model with days of week added
mreg1 <- lm(Yt ~ Tt + factor(dayofweek),
            x=T, y=T)
plot(mreg1$x[,2],mreg1$y,
     ylab="peak demand in Genese (MW/h)",xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)")
points(mreg1$x[,2],mreg1$fitted.values,
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomright",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg1))

#linear model with CDD + lag1
mreg2 <- lm(Yt ~ Tt +  factor(dayofweek) +
              CDD72t + lag(CDD72t,1), + lag(CDD72t,2),
            x=T, y=T)
plot(mreg2$x[,2],mreg2$y,
     ylab="peak demand in Genese (MW/h)",
     xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)
          + CDD65t + lag1(CDD65t)") 
points(mreg2$x[,2],mreg2$fitted.values, 
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomleft",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg2))

# linear model with HDD + lag1HDD + lag2HDD
mreg3 <- lm(Yt ~ Tt +  factor(dayofweek) +
              CDD72t + lag(CDD72t,1) +lag(CDD72t,2) + HDD45t +
              lag(HDD45t,1) + lag(HDD45t,2),
            x=T, y=T)
plot(mreg3$x[,2],mreg3$y,
     ylab="peak demand in Genese (MW/h)",
     xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)
          + CDD65t + lag1(CDD65t) + HDD45t + lag1(HDD45t)
          + lag2(HDD45t") 
points(mreg3$x[,2],mreg3$fitted.values, 
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomleft",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg3))

# linear model with holidays
mreg4 <- lm(Yt ~ Tt +  factor(dayofweek) +
              CDD72t + lag(CDD72t,1) +lag(CDD72t,2) + HDD45t +
              lag(HDD45t,1) + lag(HDD45t,2) + Holidayt$TS.1
            + priort$TS.1 + Nextt$TS.1 ,
            x=T, y=T)
plot(mreg4$x[,2],mreg4$y,
     ylab="peak demand in Genese (MW/h)",
     xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)
          + CDD65t + lag1(CDD65t) + HDD45t + lag1(HDD45t)
          + lag2(HDD45t + holidays + prior_days + Next_days") 
points(mreg4$x[,2],mreg4$fitted.values, 
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomleft",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg4))

# linear model with months
mreg5 <- lm(Yt ~ Tt +  factor(dayofweek) +
              CDD72t + lag(CDD72t,1) +lag(CDD72t,2) + HDD45t +
              lag(HDD45t,1) + lag(HDD45t,2) + Holidayt$TS.1
            + priort$TS.1 + Nextt$TS.1 + factor(months),
            x=T, y=T)
plot(mreg5$x[,2],mreg5$y,
     ylab="peak demand in Genese (MW/h)",
     xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)
  + CDD65t + lag1(CDD65t) + HDD45t + lag1(HDD45t)
  + lag2(HDD45t + holidays + prior_days + Next_days + factor(months") 
points(mreg5$x[,2],mreg5$fitted.values, 
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomleft",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg5))

# linear model with windchill
mreg6 <- lm(Yt ~ Tt +  factor(dayofweek) +
              CDD72t + lag(CDD72t,1) +lag(CDD72t,2) + HDD45t +
              lag(HDD45t,1) + lag(HDD45t,2) + Holidayt$TS.1
            + priort$TS.1 + Nextt$TS.1 + factor(months) + WNt,
            x=T, y=T)
plot(mreg6$x[,2],mreg6$y,
     ylab="peak demand in Genese (MW/h)",
     xlab="Temperature (deg F.)",
     main="2015-2022: All data adding + factor(dayofweek)
          + CDD65t + lag1(CDD65t) + HDD45t + lag1(HDD45t)
          + lag2(HDD45t + holidays + prior_days + Next_days 
          + factor(months) + windchill") 
points(mreg6$x[,2],mreg6$fitted.values, 
       col=as.numeric(factor(dayofweek)),pch=15)
legend("bottomleft",legend=levels(factor(dayofweek)),
      col=as.numeric(factor(levels(factor(dayofweek)))),pch=rep(15,6),
       title="Fitted")
print(summary(mreg6))

# plot against HDD 
mreg7 <- lm(Yt ~ factor(dayofweek) +
              CDD72t + lag(CDD72t,1) +lag(CDD72t,2) + HDD45t +
              lag(HDD45t,1) + lag(HDD45t,2) + Holidayt$TS.1
            + priort$TS.1 + Nextt$TS.1 + factor(months) + WNt,
            x=T, y=T)


plot(mreg7$x[,10],mreg7$y,
     ylab="peak demand in Genese (MW/h)",xlab="HDDt",
     main=paste("2015-2022: All data
                ",
                as.character(mreg7$call)[2]),cex.main=0.75)
points(mreg7$x[,10],mreg7$fitted.values,col="red")
legend("bottomright",legend=c("observed","fitted"),
       col=c("black","red"),
       pch=1)

# plot against CDD
plot(mreg7$x[,8],mreg7$y,
     ylab="peak demand in Genese (MW/h)",xlab="CDDt",
     main=paste("2015-2022: All data
                ",
                as.character(mreg7$call)[2]),cex.main=0.75)
points(mreg7$x[,8],mreg7$fitted.values,col="red")
legend("bottomright",legend=c("observed","fitted"),
       col=c("black","red"),
       pch=1)

# Check out diagnostic plots...
pdf("ACF_QQPLOT_LM.pdf") 

par(mfrow=c(1,1))
plot(mreg6)


par(mfrow=c(1,1))
acf(residuals(mreg6)[-(1:2)],
    main="ACF of the residuals of the regression model
    using iid Normal error structure")

# this one we might not need
par(mfrow=c(1,1))
pacf(residuals(mreg6)[-(1:2)],
     main="PACF of the residuals of the regression model")

f <- function(i) paste("Diagnostic Plot",i)
boxplot(residuals(mreg6)~factor(dayofweek[-(1:2)]),main=f(1))
boxplot(residuals(mreg6)~factor(months[-(1:2)]),main=f(2))
plot(mreg6$x[,9],residuals(mreg6),xlab="CDDt",main=f(3))
plot(mreg6$x[,10],residuals(mreg6),xlab="lag(CDDt,1)",main=f(4))
plot(mreg6$x[,11],residuals(mreg6),xlab="HDDt",main=f(5))
plot(mreg6$x[,12],residuals(mreg6),xlab="lag(HDDt,1)",main=f(6))
plot(mreg6$x[,13],residuals(mreg6),xlab="lag(HDDt,2)",main=f(7))
plot(mreg6$x[,14],residuals(mreg6),xlab="Holidays",main=f(8))
abline(h = 0)
plot(mreg6$x[,15],residuals(mreg6),xlab="Prior",main=f(9))
abline(h = 0)
plot(mreg6$x[,16],residuals(mreg6),xlab="next",main=f(10))
abline(h = 0)
plot(mreg6$x[,28],residuals(mreg6),xlab="Windchill",main=f(10))
plot(mreg6$fitted.values,residuals(mreg6),xlab="fitted",main=f(11))

dev.off(dev.cur())

dw_result <- dwtest(mreg6) 

############ ARMA ######################


# dummy variables for ARMA linear model
dayofweek <- substr(weekdays(strptime(DATE,"%Y-%m-%d")), 1, 3)

DMon <- ifelse(factor(dayofweek)=="Mon", 1, 0)
DTue <- ifelse(factor(dayofweek)=="Tue", 1, 0)
DWed <- ifelse(factor(dayofweek)=="Wed", 1, 0)
DThu <- ifelse(factor(dayofweek)=="Thu", 1, 0) 
DSat <- ifelse(factor(dayofweek)=="Sat", 1, 0)
DSun <- ifelse(factor(dayofweek)=="Sun", 1, 0)


DMont <- timeSeries(DMon ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
DTuet <- timeSeries(DTue ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
DWedt <- timeSeries(DWed ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
DThut <- timeSeries(DThu ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
DSatt <- timeSeries(DSat ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
DSunt <- timeSeries(DSun ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")

monthofyear = as.Date(DATE,format = "%Y-%m-%d")
monthofyear = format(monthofyear,"%m")

monthofyear = as.Date(DATE,format = "%Y-%m-%d")
monthofyear = format(monthofyear,"%m")

MFeb <- ifelse(factor(monthofyear)=="02",1,0)
MMar <- ifelse(factor(monthofyear)=="03",1,0)
MApr <- ifelse(factor(monthofyear)=="04",1,0)
MMay <- ifelse(factor(monthofyear)=="05",1,0)
MJun <- ifelse(factor(monthofyear)=="06",1,0)
MJul <- ifelse(factor(monthofyear)=="07",1,0)
MAug <- ifelse(factor(monthofyear)=="08",1,0)
MSep <- ifelse(factor(monthofyear)=="09",1,0)
MOct <- ifelse(factor(monthofyear)=="10",1,0)
MNov <- ifelse(factor(monthofyear)=="11",1,0)
MDec <- ifelse(factor(monthofyear)=="12",1,0)

MFebt <- timeSeries(MFeb ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MMart <- timeSeries(MMar ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MAprt <- timeSeries(MApr ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MMayt <- timeSeries(MMay ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MJunt <- timeSeries(MJun ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MJult <- timeSeries(MJul ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MAugt <- timeSeries(MAug ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MSept <- timeSeries(MSep ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MOctt <- timeSeries(MOct ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MNovt <- timeSeries(MNov ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")
MDect <- timeSeries(MDec ,DATE,
                    format="%Y-%m-%d",
                    zone = "NewYork",
                    FinCenter = "NewYork")

# Holiday dummy variable as TS 

holidays <- c("2015-01-01", "2015-05-25", "2015-07-03", 
              "2015-09-07", "2015-11-26", "2015-12-25",
              "2016-01-01", "2016-05-30", "2016-07-04", 
              "2016-09-05", "2016-11-24", "2016-12-26",
              "2017-01-02", "2017-05-29", "2017-07-04", 
              "2017-09-04", "2017-11-23", "2017-12-25",
              "2018-01-01", "2018-05-28", "2018-07-04", 
              "2018-09-03", "2018-11-22", "2018-12-25",
              "2019-01-01", "2019-05-27", "2019-07-04", 
              "2019-09-02", "2019-11-28", "2019-12-25",
              "2020-01-01", "2020-05-25", "2020-07-03", 
              "2020-09-07", "2020-11-26", "2020-12-25",
              "2021-01-01", "2021-05-31", "2021-07-05", 
              "2021-09-06", "2021-11-25", "2021-12-24",
              "2022-01-02", "2022-05-30", "2022-07-04", 
              "2022-09-05", "2022-11-24", "2022-12-26")

holidays <- as.Date(holidays)
prior_days <- holidays - 1
next_days <- holidays + 1

holidays_df = data.frame(DATE)
holidays_df$holidays = ifelse(DATE %in% as.character(holidays), 1, 0)
holidays_df$prior = ifelse(DATE %in% as.character(prior_days), 1, 0)
holidays_df$Next = ifelse(DATE %in% as.character(next_days), 1, 0)

Holidayt = timeSeries(holidays_df$holidays ,DATE)
priort = timeSeries(holidays_df$prior ,DATE)
Nextt = timeSeries(holidays_df$Next ,DATE)

#### split into train valid test###

# function for factors
create_binary_splits <- function(dayofweek, monthofyear) {
  # split indices
  train_idx <- 1:1826
  valid_idx <- 1827:2557
  test_idx <- 2558:2922
  
  # Create data frames for dayofweek and monthofyear variables
  dow_df <- data.frame(DMon=ifelse(factor(dayofweek)=="Mon", 1, 0), 
                       DTue=ifelse(factor(dayofweek)=="Tue", 1, 0), 
                       DWed=ifelse(factor(dayofweek)=="Wed", 1, 0), 
                       DThu=ifelse(factor(dayofweek)=="Thu", 1, 0), 
                       DSat=ifelse(factor(dayofweek)=="Sat", 1, 0),
                       DSun=ifelse(factor(dayofweek)=="Sun", 1, 0))
  
  moy_df <- data.frame(Mfeb=ifelse(factor(monthofyear)=="02",1,0), 
                       Mmar=ifelse(factor(monthofyear)=="03",1,0),
                       Mapr=ifelse(factor(monthofyear)=="04",1,0),
                       Mmay=ifelse(factor(monthofyear)=="05",1,0), 
                       Mjun=ifelse(factor(monthofyear)=="06",1,0),
                       Mjul=ifelse(factor(monthofyear)=="07",1,0),
                       Maug=ifelse(factor(monthofyear)=="08",1,0),
                       Msep=ifelse(factor(monthofyear)=="09",1,0),
                       Moct=ifelse(factor(monthofyear)=="10",1,0),
                       Mnov=ifelse(factor(monthofyear)=="11",1,0),
                       Mdec=ifelse(factor(monthofyear)=="12",1,0))
  
  # Split dayofweek and monthofyear to train, valid, and test subsets
  dow_train <- dow_df[train_idx,]
  dow_valid <- dow_df[valid_idx,]
  dow_test <- dow_df[test_idx,]
  moy_train <- moy_df[train_idx,]
  moy_valid <- moy_df[valid_idx,]
  moy_test <- moy_df[test_idx,]
  
  # Return a list of binary variable data frames
  return(list(dow_train=dow_train, dow_valid=dow_valid, 
              dow_test=dow_test,
              moy_train=moy_train, moy_valid=moy_valid, 
              moy_test=moy_test))
}

# function for TS
create_time_series_splits <- function(ts_data) {
  # split indices
  train_idx <- 1:1826
  valid_idx <- 1827:2557
  test_idx <- 2558:2922
  
  # create time series objects
  ts_train <- timeSeries(ts_data[train_idx], time(ts_data)[train_idx],
                         format = "%Y-%m-%d", zone = "NewYork",
                         FinCenter = "NewYork")
  ts_valid <- timeSeries(ts_data[valid_idx], time(ts_data)[valid_idx],
                         format = "%Y-%m-%d", zone = "NewYork",
                         FinCenter = "NewYork")
  ts_test <- timeSeries(ts_data[test_idx], time(ts_data)[test_idx],
                        format = "%Y-%m-%d", zone = "NewYork",
                        FinCenter = "NewYork")
  
  # assign new variable names
  names <- c("train", "valid", "test")
  new_names <- paste0(names, "_ts")
  names(ts_train) <- paste0(new_names[1], collapse = "")
  names(ts_valid) <- paste0(new_names[2], collapse = "")
  names(ts_test) <- paste0(new_names[3], collapse = "")
  
  # return a list of time series objects
  return(list(ts_train, ts_valid, ts_test))
}

# do data split

binary_splits <- create_binary_splits(dayofweek, monthofyear)

dow_train <- binary_splits$dow_train
dow_valid <- binary_splits$dow_valid
dow_test <- binary_splits$dow_test
moy_train <- binary_splits$moy_train
moy_valid <- binary_splits$moy_valid
moy_test <- binary_splits$moy_test

HDD45t_time_series_splits <- create_time_series_splits(HDD45t)
HDD45t_train <- HDD45t_time_series_splits[[1]]
HDD45t_valid <- HDD45t_time_series_splits[[2]]
HDD45t_test <- HDD45t_time_series_splits[[3]]

CDD72t_time_series_splits <- create_time_series_splits(CDD72t)
CDD72t_train <- CDD72t_time_series_splits[[1]]
CDD72t_valid <- CDD72t_time_series_splits[[2]]
CDD72t_test <- CDD72t_time_series_splits[[3]]

WNt_time_series_splits <- create_time_series_splits(WNt)
WNt_train <- WNt_time_series_splits[[1]]
WNt_valid <- WNt_time_series_splits[[2]]
WNt_test <- WNt_time_series_splits[[3]]

Holidayt_time_series_splits <- create_time_series_splits(Holidayt)
Holidayt_train <- Holidayt_time_series_splits[[1]]
Holidayt_valid <- Holidayt_time_series_splits[[2]]
Holidayt_test <- Holidayt_time_series_splits[[3]]

priort_time_series_splits <- create_time_series_splits(priort)
priort_train <- priort_time_series_splits[[1]]
priort_valid <- priort_time_series_splits[[2]]
priort_test <- priort_time_series_splits[[3]]

Nextt_time_series_splits <- create_time_series_splits(Nextt)
Nextt_train <- Nextt_time_series_splits[[1]]
Nextt_valid <- Nextt_time_series_splits[[2]]
Nextt_test <- Nextt_time_series_splits[[3]]

Yt_train <- timeSeries(Yt[1:1826],
                       time(Yt)[1:1826],
                       format="%Y-%m-%d",
                       zone = "NewYork",
                       FinCenter = "NewYork")


### first fit auto Arima model + pull diagnostics

fit <- arima(Yt_train, xreg=cbind(
  HDD45t_train, CDD72t_train,
  lag(HDD45t_train, 1), lag(HDD45t_train, 2),
  lag(CDD72t_train, 1), lag(CDD72t_train, 2),
  WNt_train, Holidayt_train, priort_train,
  Nextt_train,
  dow_train$DMon, dow_train$DTue, dow_train$DWed, 
  dow_train$DThu, dow_train$DSat, dow_train$DSun,
  moy_train$Mfeb,moy_train$Mmar,moy_train$Mapr,
  moy_train$Mmay,moy_train$Mjun,moy_train$Mjul,
  moy_train$Maug,moy_train$Msep,moy_train$Moct,
  moy_train$Mnov,moy_train$Mdec),
  order=c(5,0,0))

print(fit)

par(mfrow=c(1,1))
acf(residuals(fit)[-(1:2)])
title(main="With proper error structure (using auto.arima)")

### Create plots###
pdf("diagnostics_ARMA.pdf")

par(mfrow=c(1,1))
acf(residuals(fit)[-(1:2)])
title(main="With proper error structure (using auto.arima)")

f <- function(i) paste("Diagnostic Plot",i)
boxplot(residuals(fit)~factor(dayofweek[1:1826]),main=f(1))
boxplot(residuals(fit)~factor(months[1:1826]),main=f(2))

plot(CDD72t_train,residuals(fit),
     xlab="CDDt",
     ylab="Residuals",
     main=f(3))
plot(lag(CDD72t_train,1),residuals(fit),
     xlab="lag(CDDt,1)",
     ylab = "Residuals",
     main=f(4))
plot(lag(CDD72t_train,2),residuals(fit),
     xlab="lag(CDDt,2)",
     ylab = "Residuals",
     main=f(5))

plot(HDD45t_train,residuals(fit),
     xlab="HDDt",
     ylab = "Residuals",
     main=f(6))

plot(lag(HDD45t_train,1),residuals(fit),
     xlab="lag(HDDt,1)",
     ylab = "Residuals",
     main=f(7))

plot(lag(HDD45t_train,2),residuals(fit),
     xlab="lag(HDDt,2)",
     ylab = "Residuals",
     main=f(8))

plot(WNt_train,residuals(fit),
     xlab="Windchill",
     ylab = "Residuals",
     main=f(9))

boxplot(residuals(fit)~factor(Holidayt_train),main=f(10))
boxplot(residuals(fit)~factor(priort_train),main=f(11))
boxplot(residuals(fit)~factor(Nextt_train),main=f(12))

dev.off(dev.cur())

### expanding window for "forecasts"###

# train from 2015-2019
windows.train <- window(Yt, start=time(Yt)[1]
                        , end=time(Yt)[1826])

# validation from 2020-2021
windows.validation <- window(Yt, start=time(Yt)[1827]
                             , end=time(Yt)[2557])


train_length <- length(windows.train)-2
valid_length <- length(windows.validation)


# Create an empty vector to store the one-step-ahead forecasts
forecasts <- NULL
low_80 <- NULL
upp_80 <- NULL
low_95 <- NULL
upp_95 <- NULL

# Record the start time
start_time <- Sys.time()

# Loop through the validation set
for (i in 1:valid_length) {
  # Combine training set with current obs from the validation set
  current_data <- c(windows.train, windows.validation[1:i-1])
  
  # Combine the explanatory variables as well
  current_HDD45t <- rbind(window(HDD45t, 
                            start = time(HDD45t)[1], 
                            end = time(HDD45t)[train_length + i + 2]))
  current_HDD45t_lag1 <- rbind(window(lag(HDD45t,1), 
                            start = time(HDD45t)[1], 
                            end = time(HDD45t)[train_length + i + 2]))
  current_HDD45t_lag2 <- rbind(window(lag(HDD45t,2), 
                            start = time(HDD45t)[1], 
                            end = time(HDD45t)[train_length + i + 2]))
  current_CDD72t <- rbind(window(CDD72t, 
                            start = time(CDD72t)[1], 
                            end = time(CDD72t)[train_length + i+ 2]))
  current_CDD72t_lag1 <- rbind(window(lag(CDD72t,1), 
                            start = time(CDD72t)[1], 
                            end = time(CDD72t)[train_length + i+ 2]))
  current_CDD72t_lag2 <- rbind(window(lag(CDD72t,2), 
                            start = time(CDD72t)[1], 
                            end = time(CDD72t)[train_length + i+ 2]))
  current_WNt <- rbind(window(WNt, 
                            start = time(WNt)[1], 
                            end = time(WNt)[train_length + i+ 2]))
  current_Holidayt <- rbind(window(Holidayt, 
                            start = time(Holidayt)[1], 
                          end = time(Holidayt)[train_length + i+ 2]))
  current_Priort <- rbind(window(priort, 
                            start = time(priort)[1], 
                            end = time(priort)[train_length + i+ 2]))
  current_Nextt <- rbind(window(Nextt, 
                            start = time(Nextt)[1], 
                            end = time(Nextt)[train_length + i+ 2]))
  current_DMon <- rbind(window(lag(DMont,2), 
                            start = time(DMont)[1], 
                            end = time(DMont)[train_length + i+ 2]))
  current_DTue <- rbind(window(lag(DTuet,2), 
                            start = time(DTuet)[1], 
                            end = time(DTuet)[train_length + i+ 2]))
  current_DWed <- rbind(window(lag(DWedt,2), 
                            start = time(DWedt)[1], 
                            end = time(DWedt)[train_length + i+ 2]))
  current_DThu <- rbind(window(lag(DThut,2), 
                            start = time(DThut)[1], 
                            end = time(DThut)[train_length + i+ 2]))
  current_DSat <- rbind(window(lag(DSatt,2), 
                            start = time(DSatt)[1], 
                            end = time(DSatt)[train_length + i+ 2]))
  current_DSun <- rbind(window(lag(DSunt,2), 
                            start = time(DSunt)[1], 
                            end = time(DSunt)[train_length + i+ 2]))
  current_MFeb <- rbind(window(lag(MFebt,2), 
                            start = time(MFebt)[1], 
                            end = time(MFebt)[train_length + i+ 2]))
  current_MMar <- rbind(window(lag(MMart,2), 
                            start = time(MMart)[1], 
                            end = time(MMart)[train_length + i+ 2]))
  current_MApr <- rbind(window(lag(MAprt,2), 
                            start = time(MAprt)[1], 
                            end = time(MAprt)[train_length + i+ 2]))
  current_MMay <- rbind(window(lag(MMayt,2), 
                            start = time(MMayt)[1], 
                            end = time(MMayt)[train_length + i+ 2]))
  current_MJun <- rbind(window(lag(MJunt,2), 
                            start = time(MJunt)[1], 
                            end = time(MJunt)[train_length + i+ 2]))
  current_MJul <- rbind(window(lag(MJult,2), 
                            start = time(MJult)[1], 
                            end = time(MJult)[train_length + i+ 2]))
  current_MAug <- rbind(window(lag(MAugt,2), 
                            start = time(MAugt)[1], 
                            end = time(MAugt)[train_length + i+ 2]))
  current_MSep <- rbind(window(lag(MSept,2),
                            start = time(MSept)[1], 
                            end = time(MSept)[train_length + i+ 2]))
  current_MOct <- rbind(window(lag(MOctt,2), 
                            start = time(MOctt)[1], 
                            end = time(MOctt)[train_length + i+ 2]))
  current_MNov <- rbind(window(lag(MNovt,2), 
                            start = time(MNovt)[1], 
                            end = time(MNovt)[train_length + i+ 2]))
  current_MDec <- rbind(window(lag(MDect,2), 
                            start = time(MDect)[1], 
                            end = time(MDect)[train_length + i+ 2]))
  
  
  # temp[i] <- current_HDD45t_lag1
  
  # Create a matrix of the explanatory variables
  xreg_matrix <- cbind(
    current_HDD45t@.Data[-length(current_HDD45t@.Data)],
    current_HDD45t_lag1@.Data[-length(current_HDD45t_lag1@.Data)],
    current_HDD45t_lag2@.Data[-length(current_HDD45t_lag2@.Data)],
    current_CDD72t@.Data[-length(current_CDD72t@.Data)],
    current_CDD72t_lag1@.Data[-length(current_CDD72t_lag1@.Data)],
    current_CDD72t_lag2@.Data[-length(current_CDD72t_lag2@.Data)],
    current_WNt@.Data[-length(current_WNt)],
    current_Holidayt@.Data[-length(current_Holidayt)],
    current_Priort@.Data[-length(current_Priort)],
    current_Nextt@.Data[-length(current_Nextt)],
    current_DMon@.Data[-length(current_DMon@.Data)],
    current_DTue@.Data[-length(current_DTue@.Data)],
    current_DWed@.Data[-length(current_DWed@.Data)],
    current_DThu@.Data[-length(current_DThu@.Data)],
    current_DSat@.Data[-length(current_DSat@.Data)],
    current_DSun@.Data[-length(current_DSun@.Data)],
    current_MFeb@.Data[-length(current_MFeb@.Data)],
    current_MMar@.Data[-length(current_MMar@.Data)],
    current_MApr@.Data[-length(current_MApr@.Data)],
    current_MMay@.Data[-length(current_MMay@.Data)],
    current_MJun@.Data[-length(current_MJun@.Data)],
    current_MJul@.Data[-length(current_MJul@.Data)],
    current_MAug@.Data[-length(current_MAug@.Data)],
    current_MSep@.Data[-length(current_MSep@.Data)],
    current_MOct@.Data[-length(current_MOct@.Data)],
    current_MNov@.Data[-length(current_MNov@.Data)],
    current_MDec@.Data[-length(current_MDec@.Data)])
  
  xreg_matrix_2 <- cbind(current_HDD45t@.Data,
                         current_HDD45t_lag1@.Data,
                         current_HDD45t_lag2@.Data,
                         current_CDD72t@.Data,
                         current_CDD72t_lag1@.Data,
                         current_CDD72t_lag2@.Data,
                         current_WNt@.Data,
                         current_Holidayt@.Data,
                         current_Priort@.Data,
                         current_Nextt@.Data,
                         current_DMon@.Data,
                         current_DTue@.Data,
                         current_DWed@.Data,
                         current_DThu@.Data,
                         current_DSat@.Data,
                         current_DSun@.Data,
                         current_MFeb@.Data,
                         current_MMar@.Data,
                         current_MApr@.Data,
                         current_MMay@.Data,
                         current_MJun@.Data,
                         current_MJul@.Data,
                         current_MAug@.Data,
                         current_MSep@.Data,
                         current_MOct@.Data,
                         current_MNov@.Data,
                         current_MDec@.Data)                       
  
  
  
  
  # Fit the linear model with ARMA errors using auto.arima
  model <- arima(current_data, xreg = xreg_matrix, order=c(5,0,0))
  
  # Make a one-step-ahead forecast
  new_xreg <- matrix(c(current_HDD45t@.Data[train_length + i + 2],
                       current_HDD45t@.Data[train_length + i + 1],
                       current_HDD45t@.Data[train_length + i],
                       current_CDD72t@.Data[train_length + i + 2],
                       current_CDD72t@.Data[train_length + i + 1],
                       current_CDD72t@.Data[train_length + i],
                       current_WNt@.Data[train_length + i + 2],
                       current_Holidayt@.Data[train_length + i + 2],
                       current_Priort@.Data[train_length + i + 2],
                       current_Nextt@.Data[train_length + i + 2],
                       current_DMon@.Data[train_length + i + 2],
                       current_DTue@.Data[train_length + i + 2],
                       current_DWed@.Data[train_length + i + 2],
                       current_DThu@.Data[train_length + i + 2],
                       current_DSat@.Data[train_length + i + 2],
                       current_DSun@.Data[train_length + i + 2],
                       current_MFeb@.Data[train_length + i + 2],
                       current_MMar@.Data[train_length + i + 2],
                       current_MApr@.Data[train_length + i + 2],
                       current_MMay@.Data[train_length + i + 2],
                       current_MJun@.Data[train_length + i + 2],
                       current_MJul@.Data[train_length + i + 2],
                       current_MAug@.Data[train_length + i + 2],
                       current_MSep@.Data[train_length + i + 2],
                       current_MOct@.Data[train_length + i + 2],
                       current_MNov@.Data[train_length + i + 2],
                       current_MDec@.Data[train_length + i + 2]),
                     nrow = 1, ncol = 27)
  
  
  
  forecast_result <- forecast(model, h = 1, xreg =new_xreg)
  
  # Store the forecast in the forecasts vector
  forecasts[i] <- forecast_result$mean[1]
  low_80[i] <- forecast_result$lower[1]
  upp_80[i] <- forecast_result$upper[1]
  low_95[i] <- forecast_result$lower[2]
  upp_95[i] <- forecast_result$upper[2]
  
}
# Now the 'forecasts' vector contains the 
# one-step-ahead forecasts for the validation set

# Record the end time 
end_time <- Sys.time()

###Calulate Errors###

# Calculate the errors between the forecasts and the actual values
errors <- windows.validation - forecasts

# Calculate MAPE
MAPE <- mean(abs(errors / windows.validation)) * 100

# Calculate RMSE
RMSE <- sqrt(mean(errors^2))

# Calculate MAE
MAE <- mean(abs(errors))

# Calculate MPE
MPE <- mean(errors / windows.validation) * 100

# Print the error metrics
cat("MAPE:", MAPE, "\n")
cat("RMSE:", RMSE, "\n")
cat("MAE:", MAE, "\n")
cat("MPE:", MPE, "\n")

###plot forecasts###

par(mfrow=c(2,1))

x <- 1:valid_length
plot(x, forecasts, type = "n",
     xlab = "Daily", 
     ylab = "Forecasts (Mw)", 
     main = "Forecasts for Validation set in 95% PI",
     ylim = c(min(low_80,upp_80,low_95,upp_95),
              max(low_80,upp_80,low_95,upp_95)))

# Create shaded 95% prediction interval
x_range <- c(x, rev(x))
y_range <- c(low_95, rev(upp_95))
polygon(x_range, y_range, col = "red", border = NA)  

# Draw the black line on top of the shaded area
lines(x, forecasts, type = "l", col = "black", lwd = 2)

x <- 1:valid_length
plot(x, forecasts, type = "n", 
     xlab = "Daily", 
     ylab = "Forecasts (Mw)", 
     main = "Forecasts for Validation set in 80% PI",
     ylim = c(min(low_80,upp_80,low_95,upp_95),
              max(low_80,upp_80,low_95,upp_95)))

# Create shaded 95% prediction interval
x_range <- c(x, rev(x))
y_range <- c(low_80, rev(upp_80))
polygon(x_range, y_range, col = "red", border = NA)  

# Draw the black line on top of the shaded area
lines(x, forecasts, type = "l", col = "black", lwd = 2)



