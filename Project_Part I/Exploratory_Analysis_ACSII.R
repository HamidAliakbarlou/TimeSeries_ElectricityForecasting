#=================================================================
# Exploratory
#=================================================================
# load libraries
library(astsa)
library(forecast)
library(timeSeries)

# load nyisodata.RData
load("nyisodata.RData")

# subset data of nyisodata for zone Genese
nyisoGenese <- nyisodata[ c('TimeStamp', 'GENESE')]

# missing data on row 9431
missingData<- which(is.na(nyisoGenese$GENESE))

# fill in missing data with mean of previous and next value
nyisoGenese$GENESE[missingData] <- (nyisoGenese$GENESE[missingData-1] 
                              + nyisoGenese$GENESE[missingData+1])/2

# convert date to timeseries format with hourly data
nyisoGenese$TimeStamp <- as.POSIXct(nyisoGenese$TimeStamp, 
                                    format="%m/%d/%Y %H:%M:%S", 
                                    tz="America/New_York")

# convert to time series
zoneB <- timeSeries(nyisoGenese$GENESE,
                    nyisoGenese$TimeStamp,
                    format="%m/%d/%Y %H:%M:%S",
                    zone="NewYork", FinCenter="NewYork")

#--------------------------------------------------------------------
# Convert data to daily, do not use aggregate() for hourly data
# - it ends at 00:00:00, instead of starting at 00:00:00
# - days of the week plot will not be representative
# - use dataframe
#--------------------------------------------------------------------
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
                     format="%Y-%m-%d")

# monthly average of daily peak
df_daily_peak <- as.data.frame(GENESE_Daily_max)
df_daily_peak$month <- substr(GENESE_Daily_max$day,1,7)
GENESE_Monthly_avg <- aggregate(SS.1 ~ month, df_daily_peak, 
                                function(x) c(dailydemand = mean(x)))

#--------------------------------------------------------------------
# For plotting
#--------------------------------------------------------------------
# color theme
plt_color <- rgb(50/255, 110/255, 70/255, alpha = 0.9)
# - blue
plt_color_q1 <- rgb(21/255, 52/255, 118/255, alpha = 0.3)
# - green
plt_color_q2 <- rgb(55/255, 103/255, 89/255, alpha = 0.3)
# - yellow
plt_color_q3 <- rgb(224/255, 170/255, 77/255, alpha = 0.3)
# - white
plt_color_q4 <- rgb(218/255, 216/255, 194/255, alpha = 0.2)
# - gray grid lines
plt_color_grid <- rgb(193/255, 193/255, 193/255, alpha = 0.5)

# plot quarterly background function
plt_quarterly <- function(row_indices, demand){
  for (i in 1:(length(row_indices) - 1)) {
    start_index <- row_indices[i]
    end_index <- row_indices[i + 1]
    if (i %% 4 == 1) {
      rect(start_index, 0, end_index, max(demand), 
           col = plt_color_q1, border = NA)
    } else if (i %% 4 == 2) {
      rect(start_index, 0, end_index, max(demand), 
           col = plt_color_q2, border = NA)
    } else if (i %% 4 == 3) {
      rect(start_index, 0, end_index, max(demand), 
           col = plt_color_q3, border = NA)
    } else {
      rect(start_index, 0, end_index, max(demand), 
           col = plt_color_q4, border = NA)
    }
  }
}

# plot - find row indices of dates
find_row_indices <- function(df, dates) {
  row_indices <- sapply(dates, 
                        function(date) 
                          which(rownames(df) == as.Date(date)))
  return(row_indices)
}

# plot - vertical lines function
fct_plt_lines <- function(plt_lines_seq) { 
  for (i in plt_lines_seq){
    abline(v = i, col = plt_color, lty = "dashed")  
  }
}

# plot - compute mean quarterly
fct_mean_quarterly <- function(row_indices, demand){
  mean_values <- c()
  for (i in 1:(length(row_indices)-1)) {
    start_index <- row_indices[i]+1
    end_index <- row_indices[i + 1]
    mean_values[i] <- mean(demand[start_index:end_index])
  }
  return(mean_values)
}




#--------------------------------------------------------------------
# Fig 2.2 : Zone B daily peak demand in MW (2015-2022)
#--------------------------------------------------------------------
# aggregate max daily load, start of day is 00:00:00
# aggregate() of ts doesn't work with hours
# test for demand_max_daily:
# - 2015-01-01 1291.7
# - 2015-01-02 1343.1
# - 2015-01-03 1309.8
# - 2015-01-04 1269.3
# - 2015-01-05 1535.9
# - 2015-01-06 1545.5
demand_max_daily <- timeSeries(GENESE_Daily_max[[2]], 
                               GENESE_Daily_max[[1]],
                               format="%Y-%m-%d")

# plot max daily load from 2015 to 2022
plot(series(window(demand_max_daily, 
                   start=timeDate("2015-01-01", 
                                  format="%Y-%m-%d",
                                  zone="NewYork", 
                                  FinCenter="NewYork"), 
                   end=timeDate("2022-12-31", 
                                format="%Y-%m-%d",
                                zone="NewYork", 
                                FinCenter="NewYork"))
),
ylim=c(min(demand_max_daily),max(demand_max_daily)),
axes=F,
type="l",
ylab="Demand (in MW)",
xlab="Daily"
)
axis(2, at=seq(0,max(demand_max_daily),
               by=100), labels=T, cex.axis=0.8)
axis(1, at=c(0,366,732,1097,1462,1827,2193,2558,2922),
     labels=c("2015","2016","2017","2018","2019",
              "2020","2021","2022","2023"), 
     col.axis = plt_color, 
     padj=1)



# plot vertical lines
# - which(rownames(demand_max_daily)
#==as.Date("2016-01-01 00:00:00")) 366

plt_lines_seq <- c(0,366,732,1097,1462,1827,2193,2558,2922)
fct_plt_lines(plt_lines_seq)

# background quarterly color
quarterly_seq.daily <- seq(from=as.POSIXct("2015-01-01 00:00:00", 
                  format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
                           to=as.POSIXct("2022-12-31 00:00:00", 
            format="%Y-%m-%d %H:%M:%S",tz="America/New_York"), 
            by="month")
quarterly_dates.daily <- quarterly_seq.daily[seq(1, 
               length(quarterly_seq.daily), by=3)]

row_indices <- find_row_indices(demand_max_daily, 
                                quarterly_dates.daily)
plt_quarterly(row_indices, demand_max_daily)

# put grid lines 
grid(nx = NA,
     ny = NULL,
     lty = 1, col = plt_color_grid, lwd = 2)

# plot mean quarterly
x <- row_indices
x <- c(row_indices, length(demand_max_daily))
y <- c(demand_max_daily[1],
       fct_mean_quarterly(row_indices, demand_max_daily))
y <- c(y, mean(demand_max_daily[2832:length(demand_max_daily)]))
lines(x,y, col="red")
points(x,y, col="red")


#--------------------------------------------------------------------
# Boxplot 2.1: Days of the week, daily peak demands in MW (2015-2022)
#--------------------------------------------------------------------
# boxplot by days of the week 
# - reusing demand_max_daily

df_demand_day_of_week <- as.data.frame(as.ts(demand_max_daily))
df_demand_day_of_week$ts <- time(demand_max_daily)
df_demand_day_of_week$day <- as.factor(weekdays(
  as.Date(time(demand_max_daily))))
df_demand_day_of_week$day <- factor(df_demand_day_of_week$day, 
                                    levels = c("Sunday", "Monday", 
                                               "Tuesday", 
                                               "Wednesday", 
                                               "Thursday", "Friday", 
                                               "Saturday"))
# - test df_demand_day_of_week:  Thurs, Fri, Sat, Sun
df_demand_day_of_week$day_shorthand <- df_demand_day_of_week$day
levels(df_demand_day_of_week$day_shorthand) <- c("Sun", "Mon", 
                                                 "Tues", "Wed", 
                                                 "Thurs", "Fri", 
                                                 "Sat")
boxplot(x ~ day_shorthand, data = df_demand_day_of_week, 
        outline = FALSE, 
        xlab="Days of the week", 
        ylab="Demand (MW)"
)



#--------------------------------------------------------------------
# Fig weekly : weekly peak demands in MW (2015-2022)
#--------------------------------------------------------------------
# aggregate max weekly load
# - start on sunday, end on saturday
# - ex. (2010-01-03-00, 2010-01-09-23)
# - value taken from saturday
# - 2015-1-03 is a saturday
# - aggregate() of ts working properly
by <- timeSequence(from=start(timeDate("2015-01-03", 
                                       format="%Y-%m-%d", 
                                       zone="NewYork", 
                                       FinCenter="NewYork")), 
                   to=end(zoneB), 
                   by="week", 
                   zone="NewYork", 
                   FinCenter="NewYork")

demand_max_weekly <- aggregate(demand_max_daily, by, max, 
                               zone="NewYork", 
                               FinCenter="NewYork")

# plot max daily load from 2015 to 2022
# - 2022-12-25 is a sunday
plot(series(window(demand_max_weekly, 
                   start=timeDate("2015-01-05", 
                                  format="%Y-%m-%d", 
                                  zone="NewYork", 
                                  FinCenter="NewYork"), 
                   end=timeDate("2022-12-26", 
                                format="%Y-%m-%d", 
                                zone="NewYork", 
                                FinCenter="NewYork"))
),
ylim=c(min(demand_max_weekly),max(demand_max_weekly)),
axes=F, type="l",
ylab="Demand (in MW)",
xlab="Weekly")
axis(2, at=seq(0,max(demand_max_weekly),by=100), 
     labels=T, cex.axis=0.8)
axis(1, at=c(1, 53, 106, 158, 210, 262, 314, 366, 418),
     labels=c("2015","2016","2017","2018","2019",
              "2020","2021","2022","2023"), 
     col.axis = plt_color, 
     padj=1)

title(main="Fig 3: Weekly peak demands in MW (2015-2022)")


# plot vertical lines
# - which(rownames(demand_max_weekly)
#==as.Date("2015-01-03 00:00:00")) 1
plt_lines_seq <- c(1, 53, 106, 158, 210, 262, 314, 366, 418)
fct_plt_lines(plt_lines_seq)


# background quarterly color
quarterly_seq.weekly <- seq(from=as.POSIXct("2015-01-03 00:00:00", 
                                          format="%Y-%m-%d %H:%M:%S", 
                                            tz="America/New_York"
), 
to=as.POSIXct("2022-12-31 00:00:00", 
              format="%Y-%m-%d %H:%M:%S", 
              tz="America/New_York"), 
by="week")
quarterly_dates.weekly <- quarterly_seq.weekly[
  seq(1, length(quarterly_seq.weekly), by=13)]

row_indices <- find_row_indices(demand_max_weekly, 
                                quarterly_dates.weekly)
plt_quarterly(row_indices, demand_max_weekly)

# put grid lines 
grid(nx = NA,
     ny = NULL,
     lty = 1, col = plt_color_grid, lwd = 2)

# plot mean quarterly
x <- row_indices
y <- c(demand_max_weekly[1],
       fct_mean_quarterly(row_indices, demand_max_weekly))
lines(x,y, col="red")
points(x,y, col="red")



#--------------------------------------------------------------------
# Fig 2.3: Monthly peak demands in MW (2015-2022)
#--------------------------------------------------------------------
# get the last day of each month
by <- timeSequence(from=start(demand_max_daily), to=end(zoneB), 
                   by="month", zone="NewYork", FinCenter="NewYork")
by <- timeLastDayInMonth(by, zone="NewYork", FinCenter="NewYork")

# aggregate average monthly load, by END of MONTH
demand_max_monthly <- aggregate(demand_max_daily, by, mean,
                                zone="NewYork", FinCenter="NewYork")
# - aggregate() of ts has issue, this is to fix
# - test demand_max_monthly: 1413.658065, 1453.464286, 1276.309677
demand_max_monthly$TS.1 <- GENESE_Monthly_avg$SS.1





# data still use end of month, we will modify the axis
demand.monthly <- series(window(demand_max_monthly,
                                start=timeDate("2015-01-01-00", 
                                               format="%Y-%m-%d-%H"),
                                end=timeDate("2022-12-31-23", 
                                             format="%Y-%m-%d-%H")))

plot(demand.monthly, axes=F,
     lty=1, type="l", ylim=c(min(demand.monthly),max(demand.monthly)),
     ylab="Demand (in MW)", xlab="Monthly")

# to view monthly points
points(demand.monthly)

axis(1, at=seq(1,12*8,by=1),
     labels=rep(c("01","02","03","04","05",
                  "06","07","08","09","10","11","12"),8))

axis(1, at=seq(1,12*9,by=12),
     labels=c("2015","2016","2017","2018","2019",
              "2020","2021","2022", "2023"), 
     col.axis = plt_color, padj=2)
axis(2, at=seq(0,max(demand.monthly),by=100), labels=T, cex.axis=0.8)

# plot vertical lines
plt_lines_seq <- seq(12,12*8,by=12)
fct_plt_lines(plt_lines_seq)


# title(main="Fig 4: Monthly peak demands in MW (2015-2022)")

# background quarterly color
# - more difficult to get the dates right by using end of month
# - february is special
# - impossible to start 1st blue background since no data
# - much simpler to do in photoshop
quarterly_seq.monthly <- c("2015-01-01 00:00:00", 
"2015-01-31 00:00:00", "2015-02-28 00:00:00", "2015-03-31 00:00:00", 
"2015-04-30 00:00:00", "2015-05-31 00:00:00", "2015-06-30 00:00:00", 
"2015-07-31 00:00:00", "2015-08-31 00:00:00", "2015-09-30 00:00:00", 
"2015-10-31 00:00:00", "2015-11-30 00:00:00", "2015-12-31 00:00:00", 
"2016-01-31 00:00:00", "2016-02-29 00:00:00", "2016-03-31 00:00:00", 
"2016-04-30 00:00:00", "2016-05-31 00:00:00", "2016-06-30 00:00:00", 
"2016-07-31 00:00:00", "2016-08-31 00:00:00", "2016-09-30 00:00:00", 
"2016-10-31 00:00:00", "2016-11-30 00:00:00", "2016-12-31 00:00:00", 
"2017-01-31 00:00:00", "2017-02-28 00:00:00", "2017-03-31 00:00:00", 
"2017-04-30 00:00:00", "2017-05-31 00:00:00", "2017-06-30 00:00:00", 
"2017-07-31 00:00:00", "2017-08-31 00:00:00", "2017-09-30 00:00:00", 
"2017-10-31 00:00:00", "2017-11-30 00:00:00", "2017-12-31 00:00:00", 
"2018-01-31 00:00:00", "2018-02-28 00:00:00", "2018-03-31 00:00:00", 
"2018-04-30 00:00:00", "2018-05-31 00:00:00", "2018-06-30 00:00:00", 
"2018-07-31 00:00:00", "2018-08-31 00:00:00", "2018-09-30 00:00:00", 
"2018-10-31 00:00:00", "2018-11-30 00:00:00", "2018-12-31 00:00:00", 
"2019-01-31 00:00:00", "2019-02-28 00:00:00", "2019-03-31 00:00:00", 
"2019-04-30 00:00:00", "2019-05-31 00:00:00", "2019-06-30 00:00:00", 
"2019-07-31 00:00:00", "2019-08-31 00:00:00", "2019-09-30 00:00:00", 
"2019-10-31 00:00:00", "2019-11-30 00:00:00", "2019-12-31 00:00:00", 
"2020-01-31 00:00:00", "2020-02-29 00:00:00", "2020-03-31 00:00:00", 
"2020-04-30 00:00:00", "2020-05-31 00:00:00", "2020-06-30 00:00:00", 
"2020-07-31 00:00:00", "2020-08-31 00:00:00", "2020-09-30 00:00:00", 
"2020-10-31 00:00:00", "2020-11-30 00:00:00", "2020-12-31 00:00:00", 
"2021-01-31 00:00:00", "2021-02-28 00:00:00", "2021-03-31 00:00:00", 
"2021-04-30 00:00:00", "2021-05-31 00:00:00", "2021-06-30 00:00:00", 
"2021-07-31 00:00:00", "2021-08-31 00:00:00", "2021-09-30 00:00:00", 
"2021-10-31 00:00:00", "2021-11-30 00:00:00", "2021-12-31 00:00:00", 
"2022-01-31 00:00:00", "2022-02-28 00:00:00", "2022-03-31 00:00:00", 
"2022-04-30 00:00:00", "2022-05-31 00:00:00", "2022-06-30 00:00:00", 
"2022-07-31 00:00:00", "2022-08-31 00:00:00", "2022-09-30 00:00:00", 
"2022-10-31 00:00:00", "2022-11-30 00:00:00", "2022-12-31 00:00:00")

quarterly_dates.monthly <- quarterly_seq.monthly[seq(1, 
  length(quarterly_seq.monthly), by=3)]

row_indices <- find_row_indices(demand_max_monthly, 
                                quarterly_dates.monthly)

plt_quarterly(row_indices, demand_max_monthly)

# put grid lines 
grid(nx = NA,
     ny = NULL,
     lty = 1, col = plt_color_grid, lwd = 2)

# plot mean quarterly
x <- as.numeric(row_indices)
x[1]<-1
y <- c(demand_max_monthly[1],
       fct_mean_quarterly(x, demand_max_monthly))

# - need to replace this value manually, since were using end of month
y[2] <- mean(demand_max_monthly[1:3])
lines(x,y, col="red")
points(x,y, col="red")





#--------------------------------------------------------------------
# Fig 2.4 : Zone B, Daily peak demands in MW (2015-2022)
#--------------------------------------------------------------------
# plot time series from 2015 to 2022 daily
df_max_daily <- as.data.frame(as.ts(demand_max_daily))
df_max_daily$year <- as.factor(format(time(demand_max_daily), "%Y"))
df_max_daily$month <- as.factor(format(time(demand_max_daily), "%m"))
df_max_daily$day <- as.factor(format(time(demand_max_daily), "%d"))

# Plot daily data by month group by year
# layout(matrix(1:8, ncol = 4, byrow = TRUE))
par(mfrow = c(2, 4), mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0))
for (year in unique(df_max_daily$year)) {
  plot(df_max_daily$month[df_max_daily$year == year], 
       df_max_daily$x[df_max_daily$year == year], xlab="Month",
       ylab="Demand in MW",main=year)
}



#--------------------------------------------------------------------
# Fig 2.5 : Zone B, Days of the week, daily peak demands 
# in MW (2015-2022)
#--------------------------------------------------------------------
# plot time series from 2015 to 2022 hourly

df_demand_day_of_week$ts <- time(demand_max_daily)
df_demand_day_of_week$year <- as.factor(format(time(demand_max_daily)
                                               ,"%Y"))

# Plot hourly data by month group by year

par(mfrow = c(2, 4), mar = c(3, 3, 1, 1), mgp = c(1.5, 0.5, 0))
for (year in unique(df_demand_day_of_week$year)) {
  plot(
    df_demand_day_of_week$day_shorthand[df_demand_day_of_week$year 
                                           == year], 
       df_demand_day_of_week$x[df_demand_day_of_week$year 
     == year], xlab="Days of the week",ylab="Demand in MW",main=year)
}


