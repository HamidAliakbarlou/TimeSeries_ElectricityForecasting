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
                                           format="%Y-%m-%d %H:%M:%S",
                                           tz="America/New_York"), 
                           to=as.POSIXct("2020-08-01 00:00:00", 
                                         format="%Y-%m-%d %H:%M:%S",
                                         tz="America/New_York"), 
                           by="day")

imputedays.current <- seq(from=as.POSIXct("2020-08-03 00:00:00", 
                                          format="%Y-%m-%d %H:%M:%S",
                                          tz="America/New_York"), 
                          to=as.POSIXct("2020-08-08 00:00:00", 
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz="America/New_York"), 
                          by="day")

imputedays.after <- seq(from=as.POSIXct("2020-08-10 00:00:00", 
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz="America/New_York"), 
                        to=as.POSIXct("2020-08-15 00:00:00", 
                                      format="%Y-%m-%d %H:%M:%S",
                                      tz="America/New_York"), 
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
imputedays.imputeValues <- 
  mapply(function(x, y) { (x + y) / 2 },
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
Elmira <- 
  Stations_Subset[Stations_Subset$NAME == 
                    "ELMIRA CORNING REGIONAL AIRPORT, NY US",]

Rochester <- 
  Stations_Subset[Stations_Subset$NAME == 
                    "ROCHESTER GREATER INTERNATIONAL, NY US",]

Syracuse <- 
  Stations_Subset[Stations_Subset$NAME == 
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

Elmira$HDD45 <- sapply(Elmira$TMIN, function(x) max(0, 45 - x))

Rochester$HDD45 <- sapply(Rochester$TMIN, function(x) max(0, 45 - x))

Syracuse$HDD45 <- sapply(Syracuse$TMIN, function(x) max(0, 45 - x))

#CDD

Elmira$CDD72 <- sapply(Elmira$TMAX, function(x) max(0, x - 72))

Rochester$CDD72 <- sapply(Rochester$TMAX, function(x) max(0, x - 72))

Syracuse$CDD72 <- sapply(Syracuse$TMAX, function(x) max(0, x - 72))

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

HDD45 <- (Elmira$HDD45 * Elmira_Weights + 
            Rochester$HDD45 * Rochester_Weights + 
            Syracuse$HDD45 * Syracuse_Weights)

CDD72 <- (Elmira$CDD72 * Elmira_Weights + 
            Rochester$CDD72 * Rochester_Weights + 
            Syracuse$CDD72 * Syracuse_Weights)

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

CDD72t <- timeSeries(CDD72, 
                     DATE, format="%Y-%m-%d", 
                     zone = "NewYork", FinCenter = "NewYork")

WNt <- timeSeries(AvgWindChill, 
                  DATE, format="%Y-%m-%d", 
                  zone = "NewYork", FinCenter = "NewYork") 

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

############## Setup train, valid, test#####################
# train from 2015-2019
windows.train <- window(Yt, start=time(Yt)[1]
                        , end=time(Yt)[1826])

# validation from 2020-2021
windows.validation <- window(Yt, start=time(Yt)[1827]
                             , end=time(Yt)[2557])

# validation from 2020-2021
windows.test <- window(Yt, start=time(Yt)[2558]
                       , end=time(Yt)[2922])
