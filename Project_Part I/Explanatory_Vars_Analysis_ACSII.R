#################### Load libraries ####################
library(timeSeries)

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

df <- data.frame(day=as.Date(day,"%Y-%m-%d"), GENESE=GENESE)

GENESE_Daily <- aggregate(GENESE ~ day, df, 
                          function(x) c(dailydemand = sum(x)))

Yt <- timeSeries(GENESE_Daily[[2]], GENESE_Daily[[1]],
                 format="%Y-%m-%d",
                 zone = "NewYork",
                 FinCenter = "NewYork")

#################### Explanatory Variables ####################

### Import Stations Data
Stations <- read.csv("3 Main Stations.csv")

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
# "WT01"        -- Weather Type
# "WT02"        -- ""
# "WT03"        -- ""
# "WT04"        -- ""
# "WT05"        -- ""
# "WT06"        -- ""
# "WT08"        -- ""
# "WT09"        -- ""

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
# $ WT01     : int  
# $ WT02     : int  
# $ WT03     : int  
# $ WT04     : int  
# $ WT05     : int  
# $ WT06     : int  
# $ WT08     : int  
# $ WT09     : int  

summary(Stations)
# More than 90% of the data from the weather type variables
# are missing, thus we will omit

# Snowfall, Snowdepth and Average Daily Temperature are also 
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
             "ROCHESTER INTERNATIONAL AIRPORT, MN US",]

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
#by the median because they contain less than 0.5% of missing values

#Rochester imputation (1 NA in avg wind speed var)
Rochester$AWND[is.na(Rochester$AWND)] <- 10.74 #median

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

Elmira$HDD45 <- sapply(Elmira$TMIN, function(x) max(0, 45 - x))
Elmira$HDD50 <- sapply(Elmira$TMIN, function(x) max(0, 50 - x))
Elmira$HDD62 <- sapply(Elmira$TMIN, function(x) max(0, 62 - x))
Elmira$HDD65 <- sapply(Elmira$TMIN, function(x) max(0, 65 - x))
Elmira$HDD68 <- sapply(Elmira$TMIN, function(x) max(0, 68 - x))

Rochester$HDD45 <- sapply(Rochester$TMIN, function(x) max(0, 45 - x))
Rochester$HDD50 <- sapply(Rochester$TMIN, function(x) max(0, 50 - x))
Rochester$HDD62 <- sapply(Rochester$TMIN, function(x) max(0, 62 - x))
Rochester$HDD65 <- sapply(Rochester$TMIN, function(x) max(0, 65 - x))
Rochester$HDD68 <- sapply(Rochester$TMIN, function(x) max(0, 68 - x))

Syracuse$HDD45 <- sapply(Syracuse$TMIN, function(x) max(0, 45 - x))
Syracuse$HDD50 <- sapply(Syracuse$TMIN, function(x) max(0, 50 - x))
Syracuse$HDD62 <- sapply(Syracuse$TMIN, function(x) max(0, 62 - x))
Syracuse$HDD65 <- sapply(Syracuse$TMIN, function(x) max(0, 65 - x))
Syracuse$HDD68 <- sapply(Syracuse$TMIN, function(x) max(0, 68 - x))

#CDD

Elmira$CDD55 <- sapply(Elmira$TMAX, function(x) max(0, x - 55))
Elmira$CDD60 <- sapply(Elmira$TMAX, function(x) max(0, x - 60))
Elmira$CDD65 <- sapply(Elmira$TMAX, function(x) max(0, x - 65))
Elmira$CDD72 <- sapply(Elmira$TMAX, function(x) max(0, x - 72))
Elmira$CDD75 <- sapply(Elmira$TMAX, function(x) max(0, x - 75))
Elmira$CDD78 <- sapply(Elmira$TMAX, function(x) max(0, x - 78))

Rochester$CDD55 <- sapply(Rochester$TMAX, function(x) max(0, x - 55))
Rochester$CDD60 <- sapply(Rochester$TMAX, function(x) max(0, x - 60))
Rochester$CDD65 <- sapply(Rochester$TMAX, function(x) max(0, x - 65))
Rochester$CDD72 <- sapply(Rochester$TMAX, function(x) max(0, x - 72))
Rochester$CDD75 <- sapply(Rochester$TMAX, function(x) max(0, x - 75))
Rochester$CDD78 <- sapply(Rochester$TMAX, function(x) max(0, x - 78))

Syracuse$CDD55 <- sapply(Syracuse$TMAX, function(x) max(0, x - 55))
Syracuse$CDD60 <- sapply(Syracuse$TMAX, function(x) max(0, x - 60))
Syracuse$CDD65 <- sapply(Syracuse$TMAX, function(x) max(0, x - 65))
Syracuse$CDD72 <- sapply(Syracuse$TMAX, function(x) max(0, x - 72))
Syracuse$CDD75 <- sapply(Syracuse$TMAX, function(x) max(0, x - 75))
Syracuse$CDD78 <- sapply(Syracuse$TMAX, function(x) max(0, x - 78))

#Average Wind Chill

Elmira$AvgWindChill <- 
  ifelse(((Elmira$TMIN + Elmira$TMAX)/2) < 64.9, 
  sqrt(Elmira$AWND)*(64.9-((Elmira$TMIN + Elmira$TMAX)/2)),
  0)

Rochester$AvgWindChill <- 
  ifelse(((Rochester$TMIN + Rochester$TMAX)/2) < 64.9, 
  sqrt(Rochester$AWND)*(64.9-((Rochester$TMIN + Rochester$TMAX)/2)),
  0)

Syracuse$AvgWindChill <- 
  ifelse(((Syracuse$TMIN + Syracuse$TMAX)/2) < 64.9,
  sqrt(Syracuse$AWND)*(64.9-((Syracuse$TMIN + Syracuse$TMAX)/2)),
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

plot(series(CDD65t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD65 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 65 degrees Fahrenheit ")

plot(series(CDD75t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD75 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      CDD of Reference of 75 degrees Fahrenheit ")

#plot HDD vs daily demand

par(mfrow=c(2,2))
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

plot(series(HDD62t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD62 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 62 degrees Fahrenheit ")

plot(series(HDD65t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD65 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      HDD of Reference of 65 degrees Fahrenheit ")


#plot lag-CDD cs daily demand
par(mfrow=c(2,2))
plot(lag(CDD65t,1), Yt, 
     xlab="lag-1 CDD65",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-1 CDD65 ")
plot(lag(CDD65t,2), Yt, 
     xlab="lag-2 CDD65",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-2 CDD65 ")
plot(lag(CDD60t,1), Yt, 
     xlab="lag-1 CDD60",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-1 CDD60 ")
plot(lag(CDD60t,2), Yt, 
     xlab="lag-2 CDD60",ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-2 CDD60 ")



#plot windchill vs daily demand
par(mfrow=c(1,1))
plot(series(WNt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Windchill when Temperature Under 
     64.9 Degrees Fahrenheit (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Average Windchill")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
               "cyan", "orange", "purple", "black"), lty = 1)

#plot windchill lag vs daily demand
par(mfrow=c(2,2))
plot(lag(WNt,1), Yt, 
     xlab="lag-1 Windchill when Temperature Under 
     64.9 Degrees Fahrenheit (deg Fahrenheit) ",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-1")

plot(lag(WNt,2), Yt, 
     xlab="lag-2 Windchill when Temperature Under 
     64.9 Degrees Fahrenheit (deg Fahrenheit)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-2")

plot(lag(WNt,3), Yt, 
     xlab="lag-3 Windchill when Temperature Under 
     64.9 Degrees Fahrenheit (deg Fahrenheit)",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs 
      Windchill with lag-3")

plot(lag(WNt,4), Yt, 
     xlab="lag-4 Windchill when Temperature Under 
     64.9 Degrees Fahrenheit (deg Fahrenheit)",
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

#plot TAVG vs daily demand
# par(mfrow=c(1,1))
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(Tt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Average Daily Temperature (deg Farenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
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
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
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

# plot CDD65
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(CDD65t), series(Yt), 
     col=substr(GENESE_Daily[[1]], 1, 4),
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="CDD65 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs CDD65")
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
     ylab="Daily Demand in GENESE (MW/h)", 
     xlab="HDD45 (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs HDD45 ")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
               "cyan", "orange", "purple", "black"), lty = 1)

#plot CDD65 lag-1
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(lag(CDD65t,2), Yt, 
     xlab="lag-2 CDD65",
     ylab="Daily Demand in GENESE (MW/h)")
title("Daily Electricity Demand in Genese vs lag-2 CDD65 ")

#plot windchill vs daily demand
par(mfrow=c(1,1), 
    cex.axis = 1.7, 
    cex.lab= 1.7, cex.main = 1.4, 
    mgp = c(2.5, 1, 0))
plot(series(WNt), series(Yt), 
     col=substr(GENESE_Daily[[1]],1,4),
     ylab="Daily Demand in GENESE (MW/h)",
     xlab="Windchill When Under 64.9 F. (deg Fahrenheit)", pch=23)
title("Daily Electricity Demand in Genese vs 
      Average Windchill")
legend("bottomright", cex = 0.8 ,
       c("2015", "2016", "2017", "2018", 
         "2019", "2020", "2021", "2022"), 
       col = c("blue", "red", "green", "yellow", 
               "cyan", "orange", "purple", "black"), lty = 1)













