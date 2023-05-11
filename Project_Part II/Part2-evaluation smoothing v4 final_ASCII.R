# references:
# rolling window: 
# https://robotwealth.com/rolling-and-expanding-windows-for-dummies/

# start_time <- Sys.time()
# end_time <- Sys.time()
# elapsed_time <- end_time - start_time
#____________________________________________________________________
# # TITLE: Imputation ====
#____________________________________________________________________

# Tropical Storm Isaias on Tuesday August 4, 2020
# Hurricane Dorian on Friday/Saturday September 6–7, 2019
# See https://en.wikipedia.org/wiki/List_of_New_York_hurricanes

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


# !!! proper way to do aggregation with dataframe
# 3_naiveMonth.R

# find row indices of dates

find_row_indices <- function(df, dates) {
  row_indices <- sapply(dates, 
                        function(date) 
                          which(rownames(df) == as.Date(date)))
  return(row_indices)
}

#____________________________________________________________________
# same code as part 1
# Convert data to daily, do not use aggregate() for hourly data
# - it ends at 00:00:00, instead of starting at 00:00:00
# - days of the week plot will not be representative
# - use dataframe
#____________________________________________________________________
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

demand_max_daily <- timeSeries(GENESE_Daily_max[[2]], 
                               GENESE_Daily_max[[1]],
                               format="%Y-%m-%d")

#____________________________________________________________________
# Imputation
# - need to make sure no conflict with other code
# - we impute from the peak of previous and next week
# - from August 03 to August 08 
# -- for 1 day before disaster, disaster and 3 days after disaster
# -- replacing 6 days only
#____________________________________________________________________
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
imputedays.indexPrevious <- 
  find_row_indices(demand_max_daily_impute,imputedays.previous)

# index 2049 2050 2051 2052 2053
# values 1945.496 1844.569 1665.711 1713.288 1608.730 1531.894
imputedays.indexAfter <- 
  find_row_indices(demand_max_daily_impute,imputedays.after)

# index 2042 2043 2044 2045 2046 2047
imputedays.indexCurrent <- 
  find_row_indices(demand_max_daily_impute,imputedays.current)

# mean
imputedays.imputeValues <- mapply(function(x, y) { (x + y) / 2 },
                    demand_max_daily_impute[imputedays.indexPrevious], 
                  demand_max_daily_impute[imputedays.indexAfter])

# values 1611.134 1416.236 1362.905 1329.931 1384.207 1432.184
demand_max_daily_impute[imputedays.indexCurrent]

# Impute values
# - Replace the values in demand_max_daily_impute 
# with the values in imputedays.imputeValues
demand_max_daily_impute[imputedays.indexCurrent] <- 
  imputedays.imputeValues

# values 1912.076 1773.594 1619.887 1658.255 1582.925 1547.667
demand_max_daily_impute[imputedays.indexCurrent]

#____________________________________________________________________
# # TITLE: Helper functions ====
#____________________________________________________________________

# To find row indices of dates

find_row_indices <- function(df, dates) {
  row_indices <- sapply(dates, 
                        function(date) 
                          which(rownames(df) == as.Date(date)))
  return(row_indices)
}

# Function to count if true value from validation
# is inside in the interval CI
CI_pct_inside <- function(true_values, lower_bounds, upper_bounds) {
  # Count number of true values falling within 
  # the prediction interval
  n_in_interval <- 
    sum(true_values >= lower_bounds & true_values <= upper_bounds)
  
  # Calculate percentage of true values falling 
  # within the prediction interval
  percent_in_interval <- n_in_interval / length(true_values) * 100
  
  # Return percentage
  return(percent_in_interval)
}
#____________________________________________________________________
# # TITLE: Dataset Splits ====
#____________________________________________________________________

# FREQUENCY 1
nyiso_impute.ts <- as.ts(demand_max_daily_impute)

# output:          1       1826 
find_row_indices(demand_max_daily_impute,c('2015-01-01','2019-12-31')
                 )
# ouptut:       1827       2557 
find_row_indices(demand_max_daily_impute,c('2020-01-01','2021-12-31')
                 )
# output:       2558       2922 
find_row_indices(demand_max_daily_impute,c('2022-01-01','2022-12-31')
                 )

# train from 2015-2019
windows.train <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[1]
                        , end=time(nyiso_impute.ts)[1826])

# validation from 2020-2021
windows.validation <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[1827]
                             , end=time(nyiso_impute.ts)[2557])

# test from 2022
windows.test <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[2558]
                       , end=time(nyiso_impute.ts)[2922])


#____________________________________________________________________
# # TITLE: Exponential Smoothing Methods ====
#____________________________________________________________________

#____________________________________________________________________
# ## Naive ====
#____________________________________________________________________


#____________________________________________________________________
# ## Simple Smoothing method ====
#____________________________________________________________________


# 1a. ses expanding window ====
model_ses_exp = NULL
model_ses_alphas_exp = NULL
train_ses_mape_exp = NULL
test_ses_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_ses_exp[[i]] <- ses(nyiso_impute.ts[1:(1825+i)], h=1)
  model_ses_alphas_exp[i] <- model_ses_exp[[i]]$model$par[1]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_ses_mape_exp[i] = 
    accuracy(model_ses_exp[[i]], windows.validation[i])[1,5]
  # from validation, here known as test
  test_ses_mape_exp[i] = 
    accuracy(model_ses_exp[[i]], windows.validation[i])[2,5]
}

mean(train_ses_mape_exp)
mean(test_ses_mape_exp)
mean(model_ses_alphas_exp)


# 1b. ses rolling window ====
model_ses_roll = NULL
model_ses_alphas_roll = NULL
train_ses_mape_roll = NULL
test_ses_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [i:(1825+i)] + i to roll the window
  model_ses_roll[[i]] <- ses(nyiso_impute.ts[i:(1825+i)], h=1)
  model_ses_alphas_roll[i] <- model_ses_roll[[i]]$model$par[1]
}

for(i in 1:length(windows.validation)){
  # from training - fitted
  train_ses_mape_roll[i] = 
    accuracy(model_ses_roll[[i]], windows.validation[i])[1,5]
  # from validation, here known as test
  test_ses_mape_roll[i] = 
    accuracy(model_ses_roll[[i]], windows.validation[i])[2,5]
}

mean(train_ses_mape_roll)
mean(test_ses_mape_roll)
mean(model_ses_alphas_roll)



#____________________________________________________________________
# ## Holt Double Exponential Smoothing method ====
#____________________________________________________________________


# 1a. holt expanding window ====
model_holt_exp = NULL
model_holt_alphas_exp = NULL
model_holt_betas_exp = NULL
train_holt_mape_exp = NULL
test_holt_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_holt_exp[[i]] <- holt(nyiso_impute.ts[1:(1825+i)], h=1)
  model_holt_alphas_exp[i] <- model_holt_exp[[i]]$model$par[1]
  model_holt_betas_exp[i] <- model_holt_exp[[i]]$model$par[2]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_holt_mape_exp[i] = 
    accuracy(model_holt_exp[[i]], windows.validation[i])[1,5]
  # from validation, here known as test
  test_holt_mape_exp[i] = 
    accuracy(model_holt_exp[[i]], windows.validation[i])[2,5]
}

mean(train_holt_mape_exp)
mean(test_holt_mape_exp)
mean(model_holt_alphas_exp)
mean(model_holt_betas_exp)



# 1b. holt rolling window ====
model_holt_roll = NULL
model_holt_alphas_roll = NULL
model_holt_betas_roll = NULL
train_holt_mape_roll = NULL
test_holt_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_holt_roll[[i]] <- holt(nyiso_impute.ts[i:(1825+i)], h=1)
  model_holt_alphas_roll[i] <- model_holt_roll[[i]]$model$par[1]
  model_holt_betas_roll[i] <- model_holt_roll[[i]]$model$par[2]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_holt_mape_roll[i] = 
    accuracy(model_holt_roll[[i]], windows.validation[i])[1,5]
  # from validation, here known as test
  test_holt_mape_roll[i] = 
    accuracy(model_holt_roll[[i]], windows.validation[i])[2,5]
}

mean(train_holt_mape_roll)
mean(test_holt_mape_roll)
mean(model_holt_alphas_roll)
mean(model_holt_betas_roll)

#____________________________________________________________________
# ## Holt Winter method ====
#____________________________________________________________________

# reference hw.R not ets.R

# 0. FREQUENCY 7 ====
# - need to change frequency for the weekly seasonality
nyiso_impute.ts7 <- 
  window(ts(nyiso_impute.ts,start=time(nyiso_impute.ts)[1],
            frequency=7))

# train from 2015-2019
windows.train <- 
  window(nyiso_impute.ts7, start=time(nyiso_impute.ts7)[1]
                        , end=time(nyiso_impute.ts7)[1826])

# validation from 2020-2021
windows.validation <- 
  window(nyiso_impute.ts7, start=time(nyiso_impute.ts7)[1827]
                             , end=time(nyiso_impute.ts7)[2557])

# test from 2022
windows.test <- window(nyiso_impute.ts7, 
                       start=time(nyiso_impute.ts7)[2558]
                       , end=time(nyiso_impute.ts7)[2922])



# 1a. hwA expanding window ====
# additive
model_hwA_exp = NULL
model_hwA_alphas_exp = NULL
model_hwA_betas_exp = NULL
model_hwA_gammas_exp = NULL
train_hwA_mape_exp = NULL
test_hwA_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_hwA_exp[[i]] <- hw(window(nyiso_impute.ts7, 
                                  start=time(nyiso_impute.ts7)[1], 
                                  end=time(nyiso_impute.ts7)[1825+i]
                                  ), 
                           h=1,seasonal = c("additive"))
  model_hwA_alphas_exp[i] <- model_hwA_exp[[i]]$model$par[1]
  model_hwA_betas_exp[i] <- model_hwA_exp[[i]]$model$par[2]
  model_hwA_gammas_exp[i] <- model_hwA_exp[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwA_mape_exp[i] = accuracy(model_hwA_exp[[i]], 
                                   windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwA_mape_exp[i] = accuracy(model_hwA_exp[[i]], 
                                  windows.validation[i])[2,5]
}

mean(train_hwA_mape_exp)
mean(test_hwA_mape_exp)
mean(model_hwA_alphas_exp)
mean(model_hwA_betas_exp)
mean(model_hwA_gammas_exp)


# 1b. hwA rolling window ====
# additive
model_hwA_roll = NULL
model_hwA_alphas_roll = NULL
model_hwA_betas_roll = NULL
model_hwA_gammas_roll = NULL
train_hwA_mape_roll = NULL
test_hwA_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_hwA_roll[[i]] <- hw(window(
    nyiso_impute.ts7, start=time(nyiso_impute.ts7)[i], 
    end=time(nyiso_impute.ts7)[1825+i]), h=1,seasonal = c("additive")
    )
  model_hwA_alphas_roll[i] <- model_hwA_roll[[i]]$model$par[1]
  model_hwA_betas_roll[i] <- model_hwA_roll[[i]]$model$par[2]
  model_hwA_gammas_roll[i] <- model_hwA_roll[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwA_mape_roll[i] = accuracy(model_hwA_roll[[i]], 
                                    windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwA_mape_roll[i] = accuracy(model_hwA_roll[[i]], 
                                   windows.validation[i])[2,5]
}

mean(train_hwA_mape_roll)
mean(test_hwA_mape_roll)
mean(model_hwA_alphas_roll)
mean(model_hwA_betas_roll)
mean(model_hwA_gammas_roll)


# 2a. hwM expanding window ====
# multiplicative
model_hwM_exp = NULL
model_hwM_alphas_exp = NULL
model_hwM_betas_exp = NULL
model_hwM_gammas_exp = NULL
train_hwM_mape_exp = NULL
test_hwM_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_hwM_exp[[i]] <- hw(window(nyiso_impute.ts7, 
                                  start=time(nyiso_impute.ts7)[1], 
                                  end=time(nyiso_impute.ts7)[1825+i]
                                  ), 
                           h=1,seasonal = c("multiplicative"))
  model_hwM_alphas_exp[i] <- model_hwM_exp[[i]]$model$par[1]
  model_hwM_betas_exp[i] <- model_hwM_exp[[i]]$model$par[2]
  model_hwM_gammas_exp[i] <- model_hwM_exp[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwM_mape_exp[i] = accuracy(model_hwM_exp[[i]], 
                                   windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwM_mape_exp[i] = accuracy(model_hwM_exp[[i]], 
                                  windows.validation[i])[2,5]
}

mean(train_hwM_mape_exp)
mean(test_hwM_mape_exp)
mean(model_hwM_alphas_exp)
mean(model_hwM_betas_exp)
mean(model_hwM_gammas_exp)


# 2b. hwM rolling window ====
# multiplicative
model_hwM_roll = NULL
model_hwM_alphas_roll = NULL
model_hwM_betas_roll = NULL
model_hwM_gammas_roll = NULL
train_hwM_mape_roll = NULL
test_hwM_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_hwM_roll[[i]] <- hw(window(nyiso_impute.ts7, 
                                   start=time(nyiso_impute.ts7)[i], 
                                   end=time(nyiso_impute.ts7)[1825+i]
                                   )
                            , h=1,seasonal = c("multiplicative"))
  model_hwM_alphas_roll[i] <- model_hwM_roll[[i]]$model$par[1]
  model_hwM_betas_roll[i] <- model_hwM_roll[[i]]$model$par[2]
  model_hwM_gammas_roll[i] <- model_hwM_roll[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwM_mape_roll[i] = accuracy(model_hwM_roll[[i]], 
                                    windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwM_mape_roll[i] = accuracy(model_hwM_roll[[i]], 
                                   windows.validation[i])[2,5]
}

mean(train_hwM_mape_roll)
mean(test_hwM_mape_roll)
mean(model_hwM_alphas_roll)
mean(model_hwM_betas_roll)
mean(model_hwM_gammas_roll)

# 3a. hwAd expanding window ====
# additive dampening
model_hwAd_exp = NULL
model_hwAd_alphas_exp = NULL
model_hwAd_betas_exp = NULL
model_hwAd_gammas_exp = NULL
train_hwAd_mape_exp = NULL
test_hwAd_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_hwAd_exp[[i]] <- hw(window(nyiso_impute.ts7, 
                                   start=time(nyiso_impute.ts7)[1], 
                                   end=time(nyiso_impute.ts7)[1825+i]
                                   )
                      , h=1,seasonal = c("additive"), damped = TRUE)
  model_hwAd_alphas_exp[i] <- model_hwAd_exp[[i]]$model$par[1]
  model_hwAd_betas_exp[i] <- model_hwAd_exp[[i]]$model$par[2]
  model_hwAd_gammas_exp[i] <- model_hwAd_exp[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwAd_mape_exp[i] = accuracy(model_hwAd_exp[[i]], 
                                    windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwAd_mape_exp[i] = accuracy(model_hwAd_exp[[i]], 
                                   windows.validation[i])[2,5]
}

mean(train_hwAd_mape_exp)
mean(test_hwAd_mape_exp)
mean(model_hwAd_alphas_exp)
mean(model_hwAd_betas_exp)
mean(model_hwAd_gammas_exp)


# 3b. hwAd rolling window ====
# additive dampening


model_hwAd_roll = NULL
model_hwAd_alphas_roll = NULL
model_hwAd_betas_roll = NULL
model_hwAd_gammas_roll = NULL
train_hwAd_mape_roll = NULL
test_hwAd_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_hwAd_roll[[i]] <- hw(window(nyiso_impute.ts7, 
                                    start=time(nyiso_impute.ts7)[i], 
                                end=time(nyiso_impute.ts7)[1825+i]), 
                         h=1,seasonal = c("additive"),damped = TRUE)
  model_hwAd_alphas_roll[i] <- model_hwAd_roll[[i]]$model$par[1]
  model_hwAd_betas_roll[i] <- model_hwAd_roll[[i]]$model$par[2]
  model_hwAd_gammas_roll[i] <- model_hwAd_roll[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwAd_mape_roll[i] = accuracy(model_hwAd_roll[[i]], 
                                     windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwAd_mape_roll[i] = accuracy(model_hwAd_roll[[i]], 
                                    windows.validation[i])[2,5]
}

mean(train_hwAd_mape_roll)
mean(test_hwAd_mape_roll)
mean(model_hwAd_alphas_roll)
mean(model_hwAd_betas_roll)
mean(model_hwAd_gammas_roll)


# 4a. hwMd expanding window ====
# multiplicative dampening
model_hwMd_exp = NULL
model_hwMd_alphas_exp = NULL
model_hwMd_betas_exp = NULL
model_hwMd_gammas_exp = NULL
train_hwMd_mape_exp = NULL
test_hwMd_mape_exp = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_hwMd_exp[[i]] <- hw(window(nyiso_impute.ts7, 
                                   start=time(nyiso_impute.ts7)[1], 
                               end=time(nyiso_impute.ts7)[1825+i]), 
                  h=1,seasonal = c("multiplicative"), damped = TRUE)
  model_hwMd_alphas_exp[i] <- model_hwMd_exp[[i]]$model$par[1]
  model_hwMd_betas_exp[i] <- model_hwMd_exp[[i]]$model$par[2]
  model_hwMd_gammas_exp[i] <- model_hwMd_exp[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwMd_mape_exp[i] = accuracy(model_hwMd_exp[[i]], 
                                    windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwMd_mape_exp[i] = accuracy(model_hwMd_exp[[i]], 
                                   windows.validation[i])[2,5]
}

mean(train_hwMd_mape_exp)
mean(test_hwMd_mape_exp)
mean(model_hwMd_alphas_exp)
mean(model_hwMd_betas_exp)
mean(model_hwMd_gammas_exp)


# 4b. hwMd rolling window ====
# multiplicative dampening
start_time <- Sys.time()

model_hwMd_roll = NULL
model_hwMd_alphas_roll = NULL
model_hwMd_betas_roll = NULL
model_hwMd_gammas_roll = NULL
train_hwMd_mape_roll = NULL
test_hwMd_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_hwMd_roll[[i]] <- hw(window(nyiso_impute.ts7, 
                                  start=time(nyiso_impute.ts7)[i], 
                                end=time(nyiso_impute.ts7)[1825+i]), 
                           h=1,seasonal = c("multiplicative"), 
                           damped = TRUE)
  model_hwMd_alphas_roll[i] <- model_hwMd_roll[[i]]$model$par[1]
  model_hwMd_betas_roll[i] <- model_hwMd_roll[[i]]$model$par[2]
  model_hwMd_gammas_roll[i] <- model_hwMd_roll[[i]]$model$par[3]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_hwMd_mape_roll[i] = accuracy(model_hwMd_roll[[i]], 
                                     windows.validation[i])[1,5]
  # from validation, here known as test
  test_hwMd_mape_roll[i] = accuracy(model_hwMd_roll[[i]], 
                                    windows.validation[i])[2,5]
}

mean(train_hwMd_mape_roll)
mean(test_hwMd_mape_roll)
mean(model_hwMd_alphas_roll)
mean(model_hwMd_betas_roll)
mean(model_hwMd_gammas_roll)

end_time <- Sys.time()
elapsed_time <- end_time - start_time
#____________________________________________________________________
# ## Double seasonal holt winter ====
# * long to train ====
# - at least 2 hours 
#____________________________________________________________________


# 1a. hwDS expanding window ====
# - dshw.R
# - seasonalities must be nested, i.e. m2 must 
# be an integer multiple of m1
model_dshw_expand = NULL
model_dshw_alphas_expand = NULL
model_dshw_betas_expand = NULL
model_dshw_gammas_expand = NULL
model_dshw_lambdas_expand = NULL #delta
train_dshw_mape_expand = NULL
test_dshw_mape_expand = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  model_dshw_expand[[i]] <- dshw(msts(window(nyiso_impute.ts7, 
                                     start=time(nyiso_impute.ts7)[1], 
                                 end=time(nyiso_impute.ts7)[1825+i]), 
                                 seasonal.periods=c(7,364)), h=1)
  model_dshw_alphas_expand[i] <- model_dshw_expand[[i]]$model$alpha
  model_dshw_betas_expand[i] <- model_dshw_expand[[i]]$model$beta
  model_dshw_gammas_expand[i] <- model_dshw_expand[[i]]$model$gamma
  model_dshw_lambdas_expand[i] <- model_dshw_expand[[i]]$model$lambda
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_dshw_mape_expand[i] = accuracy(model_dshw_expand[[i]], 
                                       windows.validation[i])[1,5]
  # from validation, here known as test
  test_dshw_mape_expand[i] = accuracy(model_dshw_expand[[i]], 
                                      windows.validation[i])[2,5]
}

mean(train_dshw_mape_expand)
mean(test_dshw_mape_expand)
mean(model_dshw_alphas_expand)
mean(model_dshw_betas_expand)
mean(model_dshw_gammas_expand)
mean(model_dshw_lambdas_expand)

# 1b. hwDS rolling window ====
model_dshw_roll = NULL
model_dshw_alphas_roll = NULL
model_dshw_betas_roll = NULL
model_dshw_gammas_roll = NULL
model_dshw_lambdas_roll = NULL #delta
train_dshw_mape_roll = NULL
test_dshw_mape_roll = NULL


for(i in 1:length(windows.validation)){
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  model_dshw_roll[[i]] <- dshw(msts(window(nyiso_impute.ts7, 
                                     start=time(nyiso_impute.ts7)[i], 
                               end=time(nyiso_impute.ts7)[1825+i]), 
                             seasonal.periods=c(7,364)), h=1)
  model_dshw_alphas_roll[i] <- model_dshw_roll[[i]]$model$alpha
  model_dshw_betas_roll[i] <- model_dshw_roll[[i]]$model$beta
  model_dshw_gammas_roll[i] <- model_dshw_roll[[i]]$model$gamma
  model_dshw_lambdas_roll[i] <- model_dshw_roll[[i]]$model$lambda
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_dshw_mape_roll[i] = accuracy(model_dshw_roll[[i]], 
                                     windows.validation[i])[1,5]
  # from validation, here known as test
  test_dshw_mape_roll[i] = accuracy(model_dshw_roll[[i]], 
                                    windows.validation[i])[2,5]
}

mean(train_dshw_mape_roll)
mean(test_dshw_mape_roll)
mean(model_dshw_alphas_roll)
mean(model_dshw_betas_roll)
mean(model_dshw_gammas_roll)
mean(model_dshw_lambdas_roll)




#____________________________________________________________________
# ## TBATS method ====
# - long to train depends how much you refit model
#____________________________________________________________________

# 1a. tbats S2F1 expanding window ====
# - weekly and yearly seasonality
# - refit the model every day
model_tbats_exp.fit = NULL
model_tbats_exp.fcast = NULL
model_tbats_alphas_exp = NULL
model_tbats_ps_exp = NULL
model_tbats_qs_exp = NULL
train_tbats_mape_exp = NULL
test_tbats_mape_exp = NULL


for(i in 1:length(windows.validation)){
  
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
    window_data <- window(nyiso_impute.ts7, start = 
                            time(nyiso_impute.ts7)[1], 
                          end = time(nyiso_impute.ts7)[1825+i])
    model_tbats_exp.fit[[i]] <- tbats(msts(window_data, 
                                 seasonal.periods = c(7,365.25)), 
                                 use.parallel = T, num.cores=2)

  model_tbats_exp.fcast[[i]] <- 
    forecast(model_tbats_exp.fit[[i]],h=1)
  model_tbats_alphas_exp[i] <- 
    model_tbats_exp.fcast[[i]]$model$alpha[1]
  model_tbats_ps_exp[i] <- model_tbats_exp.fcast[[i]]$model$p
  model_tbats_qs_exp[i] <- model_tbats_exp.fcast[[i]]$model$q
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_tbats_mape_exp[i] = accuracy(model_tbats_exp.fcast[[i]], 
                                     windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_mape_exp[i] = accuracy(model_tbats_exp.fcast[[i]], 
                                    windows.validation[i])[2,5]
}

mean(train_tbats_mape_exp)
mean(test_tbats_mape_exp)
mean(model_tbats_alphas_exp)
mean(model_tbats_ps_exp)
mean(model_tbats_qs_exp)


# 1b. tbats rolling window ====
# - weekly and yearly seasonality
# - refit the model every day
model_tbats_roll.fit = NULL
model_tbats_roll.fcast = NULL
model_tbats_alphas_roll = NULL
model_tbats_ps_roll = NULL
model_tbats_qs_roll = NULL
train_tbats_mape_roll = NULL
test_tbats_mape_roll = NULL


for(i in 1:length(windows.validation)){
  
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  window_data <- window(nyiso_impute.ts7, start = 
                          time(nyiso_impute.ts7)[i], 
                        end = time(nyiso_impute.ts7)[1825+i])
  model_tbats_roll.fit[[i]] <- tbats(msts(window_data, 
                                  seasonal.periods = c(7,365.25)), 
                                  use.parallel = T, num.cores=2)
  
  model_tbats_roll.fcast[[i]] <- 
    forecast(model_tbats_roll.fit[[i]],h=1)
  model_tbats_alphas_roll[i] <- 
    model_tbats_roll.fcast[[i]]$model$alpha[1]
  model_tbats_ps_roll[i] <- model_tbats_roll.fcast[[i]]$model$p
  model_tbats_qs_roll[i] <- model_tbats_roll.fcast[[i]]$model$q
  
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_tbats_mape_roll[i] = accuracy(model_tbats_roll.fcast[[i]], 
                                      windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_mape_roll[i] = accuracy(model_tbats_roll.fcast[[i]], 
                                     windows.validation[i])[2,5]
}

mean(train_tbats_mape_roll)
mean(test_tbats_mape_roll)
mean(model_tbats_alphas_roll)
mean(model_tbats_ps_roll)
mean(model_tbats_qs_roll)


# 2a. tbats S2F7 expanding window ====
# - weekly and yearly seasonality
# - refit the model every 7 days
model_tbats_S2F7_exp.fit = NULL
model_tbats_S2F7_exp.fcast = NULL
model_tbats_S2F7_alphas_exp = NULL
model_tbats_S2F7_ps_exp = NULL
model_tbats_S2F7_qs_exp = NULL
train_tbats_S2F7_mape_exp = NULL
test_tbats_S2F7_mape_exp = NULL

model_tbats_S2F7_lo80_exp = NULL
model_tbats_S2F7_hi80_exp = NULL
model_tbats_S2F7_lo95_exp = NULL
model_tbats_S2F7_hi95_exp = NULL

# set the refit interval to 7 days
refit_interval = 7

for(i in 1:length(windows.validation)){

  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  window_data <- window(nyiso_impute.ts7, start = 
                          time(nyiso_impute.ts7)[1], end = 
                          time(nyiso_impute.ts7)[1825+i])
  
  # refit only every refit_interval days
  if (i == 1 | (i - 1) %% refit_interval == 0) {
    model_tbats_S2F7_exp.fit[[i]] <- tbats(msts(window_data, 
                                        seasonal.periods = 
                      c(7,365.25)), use.parallel = T, num.cores=2)
  } else {
    model_tbats_S2F7_exp.fit[[i]] <- tbats(msts(window_data, 
                                    seasonal.periods = c(7,365.25)), 
                                use.parallel = T, num.cores=2, 
                              model=model_tbats_S2F7_exp.fit[[i-1]])

  }

  model_tbats_S2F7_exp.fcast[[i]] <- 
    forecast(model_tbats_S2F7_exp.fit[[i]],h=1)
  model_tbats_S2F7_alphas_exp[i] <- 
    model_tbats_S2F7_exp.fcast[[i]]$model$alpha[1]
  model_tbats_S2F7_ps_exp[i] <- 
    model_tbats_S2F7_exp.fcast[[i]]$model$p
  model_tbats_S2F7_qs_exp[i] <- 
    model_tbats_S2F7_exp.fcast[[i]]$model$q
  
  model_tbats_S2F7_lo80_exp[i] = 
    model_tbats_S2F7_exp.fcast[[1]]$lower[1]
  model_tbats_S2F7_lo95_exp[i] = 
    model_tbats_S2F7_exp.fcast[[1]]$lower[2]
  model_tbats_S2F7_hi80_exp[i] = 
    model_tbats_S2F7_exp.fcast[[1]]$upper[1]
  model_tbats_S2F7_hi95_exp[i] = 
    model_tbats_S2F7_exp.fcast[[1]]$upper[2]
}


for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_tbats_S2F7_mape_exp[i] = 
    accuracy(model_tbats_S2F7_exp.fcast[[i]], 
             windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_S2F7_mape_exp[i] = 
    accuracy(model_tbats_S2F7_exp.fcast[[i]], 
             windows.validation[i])[2,5]
}

mean(train_tbats_S2F7_mape_exp)
mean(test_tbats_S2F7_mape_exp)
mean(model_tbats_S2F7_alphas_exp)
mean(model_tbats_S2F7_ps_exp)
mean(model_tbats_S2F7_qs_exp)

CI_pct_inside(model_tbats_S2F7_exp.fcast, 
              model_tbats_S2F7_lo80_exp, 
              model_tbats_S2F7_hi80_exp)
CI_pct_inside(model_tbats_S2F7_exp.fcast, 
              model_tbats_S2F7_lo95_exp, 
              model_tbats_S2F7_hi95_exp)


# 2b. tbats S2F7 rolling window ====
# - weekly and yearly seasonality
# - refit the model every 7 days
model_tbats_S2F7_roll.fit = NULL
model_tbats_S2F7_roll.fcast = NULL
model_tbats_S2F7_alphas_roll = NULL
model_tbats_S2F7_lambdas_roll = NULL
model_tbats_S2F7_ps_roll = NULL
model_tbats_S2F7_qs_roll = NULL
train_tbats_S2F7_mape_roll = NULL
test_tbats_S2F7_mape_roll = NULL

model_tbats_S2F7_lo80_roll = NULL
model_tbats_S2F7_hi80_roll = NULL
model_tbats_S2F7_lo95_roll = NULL
model_tbats_S2F7_hi95_roll = NULL

# set the refit interval to 7 days
refit_interval = 7

for(i in 1:length(windows.validation)){
  
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  window_data <- window(nyiso_impute.ts7, start = 
                          time(nyiso_impute.ts7)[i], 
                        end = time(nyiso_impute.ts7)[1825+i])
  
  # refit only every refit_interval days
  if (i == 1 | (i - 1) %% refit_interval == 0) {
    model_tbats_S2F7_roll.fit[[i]] <- tbats(msts(window_data, 
                                   seasonal.periods = c(7,365.25)), 
                                   use.parallel = T, num.cores=2)
  } else {
    model_tbats_S2F7_roll.fit[[i]] <- tbats(msts(window_data, 
                                   seasonal.periods = c(7,365.25)), 
                               use.parallel = T, num.cores=2, 
                             model=model_tbats_S2F7_roll.fit[[i-1]])
  }
  
  model_tbats_S2F7_roll.fcast[[i]] <- 
    forecast(model_tbats_S2F7_roll.fit[[i]],h=1)
  model_tbats_S2F7_alphas_roll[i] <- 
    model_tbats_S2F7_roll.fcast[[i]]$model$alpha[1]
  model_tbats_S2F7_ps_roll[i] <- 
    model_tbats_S2F7_roll.fcast[[i]]$model$p
  model_tbats_S2F7_qs_roll[i] <- 
    model_tbats_S2F7_roll.fcast[[i]]$model$q
  
  model_tbats_S2F7_lo80_roll[i] = 
    model_tbats_S2F7_roll.fcast[[1]]$lower[1]
  model_tbats_S2F7_lo95_roll[i] = 
    model_tbats_S2F7_roll.fcast[[1]]$lower[2]
  model_tbats_S2F7_hi80_roll[i] = 
    model_tbats_S2F7_roll.fcast[[1]]$upper[1]
  model_tbats_S2F7_hi95_roll[i] = 
    model_tbats_S2F7_roll.fcast[[1]]$upper[2]
}


for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_tbats_S2F7_mape_roll[i] = 
    accuracy(model_tbats_S2F7_roll.fcast[[i]], 
             windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_S2F7_mape_roll[i] = 
    accuracy(model_tbats_S2F7_roll.fcast[[i]], 
             windows.validation[i])[2,5]
}

mean(train_tbats_S2F7_mape_roll)
mean(test_tbats_S2F7_mape_roll)
mean(model_tbats_S2F7_alphas_roll)
mean(model_tbats_S2F7_ps_roll)
mean(model_tbats_S2F7_qs_roll)

# CI_pct_inside(train_tbats_S2F7_mape_roll, 
# model_tbats_S2F7_lo80_roll, model_tbats_S2F7_hi80_roll)
# CI_pct_inside(train_tbats_S2F7_mape_roll, 
# model_tbats_S2F7_lo95_roll, model_tbats_S2F7_hi95_roll)


# 3a. tbats S1F7 expanding window ====
# - weekly seasonality
# - refit the model every 7 days
model_tbats_S1F7_exp.fit = NULL
model_tbats_S1F7_exp.fcast = NULL
model_tbats_S1F7_alphas_exp = NULL
model_tbats_S1F7_lambdas_exp = NULL
model_tbats_S1F7_ps_exp = NULL
model_tbats_S1F7_qs_exp = NULL
train_tbats_S1F7_mape_exp = NULL
test_tbats_S1F7_mape_exp = NULL

model_tbats_S1F7_lo80_exp = NULL
model_tbats_S1F7_hi80_exp = NULL
model_tbats_S1F7_lo95_exp = NULL
model_tbats_S1F7_hi95_exp = NULL

# set the refit interval to 7 days
refit_interval = 7

for(i in 1:length(windows.validation)){
  
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to expand the window
  window_data <- window(nyiso_impute.ts7, start = 
                          time(nyiso_impute.ts7)[1], 
                        end = time(nyiso_impute.ts7)[1825+i])
  
  # refit only every refit_interval days
  if (i == 1 | (i - 1) %% refit_interval == 0) {
    model_tbats_S1F7_exp.fit[[i]] <- tbats(msts(window_data, 
                              seasonal.periods = c(7)), 
                              use.parallel = T, num.cores=2)
  } else {
    model_tbats_S1F7_exp.fit[[i]] <- tbats(msts(window_data, 
                                          seasonal.periods = c(7)), 
                                      use.parallel = T, num.cores=2, 
                              model=model_tbats_S1F7_exp.fit[[i-1]])
  }
  
  model_tbats_S1F7_exp.fcast[[i]] <- 
  forecast(model_tbats_S1F7_exp.fit[[i]],h=1) 
  model_tbats_S1F7_alphas_exp[i] <- 
    model_tbats_S1F7_exp.fcast[[i]]$model$alpha[1]
  model_tbats_S1F7_ps_exp[i] <- 
    model_tbats_S1F7_exp.fcast[[i]]$model$p
  model_tbats_S1F7_qs_exp[i] <- 
  model_tbats_S1F7_exp.fcast[[i]]$model$q
  
  model_tbats_S1F7_lo80_exp[i] = 
    model_tbats_S1F7_exp.fcast[[1]]$lower[1]
  model_tbats_S1F7_lo95_exp[i] = 
    model_tbats_S1F7_exp.fcast[[1]]$lower[2]
  model_tbats_S1F7_hi80_exp[i] = 
    model_tbats_S1F7_exp.fcast[[1]]$upper[1]
  model_tbats_S1F7_hi95_exp[i] = 
    model_tbats_S1F7_exp.fcast[[1]]$upper[2]
}


for(i in 1:length(windows.validation)){
  # for(i in 1:10){
  # from training minus fitted
  train_tbats_S1F7_mape_exp[i] = 
    accuracy(model_tbats_S1F7_exp.fcast[[i]], 
             windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_S1F7_mape_exp[i] = 
    accuracy(model_tbats_S1F7_exp.fcast[[i]], 
             windows.validation[i])[2,5]
}

mean(train_tbats_S1F7_mape_exp)
mean(test_tbats_S1F7_mape_exp)
mean(model_tbats_S1F7_alphas_exp)
mean(model_tbats_S1F7_ps_exp)
mean(model_tbats_S1F7_qs_exp)

CI_pct_inside(model_tbats_S1F7_exp.fcast, 
              model_tbats_S1F7_lo80_exp, 
              model_tbats_S1F7_hi80_exp)
CI_pct_inside(model_tbats_S1F7_exp.fcast, 
              model_tbats_S1F7_lo95_exp, 
              model_tbats_S1F7_hi95_exp)


# 3b. tbats S1F7 rolling window ====
# - weekly seasonality
# - refit the model every 7 days
model_tbats_S1F7_roll.fit = NULL
model_tbats_S1F7_roll.fcast = NULL
model_tbats_S1F7_alphas_roll = NULL
model_tbats_S1F7_lambdas_roll = NULL
model_tbats_S1F7_ps_roll = NULL
model_tbats_S1F7_qs_roll = NULL
train_tbats_S1F7_mape_roll = NULL
test_tbats_S1F7_mape_roll = NULL

model_tbats_S1F7_lo80_roll = NULL
model_tbats_S1F7_hi80_roll = NULL
model_tbats_S1F7_lo95_roll = NULL
model_tbats_S1F7_hi95_roll = NULL

# set the refit interval to 7 days
refit_interval = 7

for(i in 1:length(windows.validation)){
  
  # validation start at 1827 so 1826-1=1825
  # [1:(1825+i)] to roll the window
  window_data <- window(nyiso_impute.ts7, 
                        start = time(nyiso_impute.ts7)[i], 
                        end = time(nyiso_impute.ts7)[1825+i])
  
  # refit only every refit_interval days
  if (i == 1 | (i - 1) %% refit_interval == 0) {
    model_tbats_S1F7_roll.fit[[i]] <- tbats(msts(window_data, 
                         seasonal.periods = c(7)), use.parallel = T, 
                         num.cores=2)
  } else {
    model_tbats_S1F7_roll.fit[[i]] <- tbats(msts(window_data, 
                         seasonal.periods = c(7)), use.parallel = T, 
               num.cores=2, model=model_tbats_S1F7_roll.fit[[i-1]])
  }
  
  model_tbats_S1F7_roll.fcast[[i]] <- 
    forecast(model_tbats_S1F7_roll.fit[[i]],h=1)
  model_tbats_S1F7_alphas_roll[i] <- 
    model_tbats_S1F7_roll.fcast[[i]]$model$alpha[1]
  model_tbats_S1F7_ps_roll[i] <- 
    model_tbats_S1F7_roll.fcast[[i]]$model$p
  model_tbats_S1F7_qs_roll[i] <- 
    model_tbats_S1F7_roll.fcast[[i]]$model$q
  
  model_tbats_S1F7_lo80_roll[i] = 
    model_tbats_S1F7_roll.fcast[[1]]$lower[1]
  model_tbats_S1F7_lo95_roll[i] = 
    model_tbats_S1F7_roll.fcast[[1]]$lower[2]
  model_tbats_S1F7_hi80_roll[i] = 
    model_tbats_S1F7_roll.fcast[[1]]$upper[1]
  model_tbats_S1F7_hi95_roll[i] = 
    model_tbats_S1F7_roll.fcast[[1]]$upper[2]
}

for(i in 1:length(windows.validation)){
  # from training minus fitted
  train_tbats_S1F7_mape_roll[i] = 
    accuracy(model_tbats_S1F7_roll.fcast[[i]], 
             windows.validation[i])[1,5]
  # from validation, here known as test
  test_tbats_S1F7_mape_roll[i] = 
    accuracy(model_tbats_S1F7_roll.fcast[[i]], 
             windows.validation[i])[2,5]
}

mean(train_tbats_S1F7_mape_roll)
mean(test_tbats_S1F7_mape_roll)
mean(model_tbats_S1F7_alphas_roll)
mean(model_tbats_S1F7_ps_roll)
mean(model_tbats_S1F7_qs_roll)

# CI_pct_inside(windows.validation, model_tbats_S1F7_lo80_roll, 
#model_tbats_S1F7_hi80_roll)
# CI_pct_inside(windows.validation, model_tbats_S1F7_lo95_roll, 
#model_tbats_S1F7_hi95_roll)


CI_pct_inside(unlist(lapply(model_tbats_S1F7_roll.fcast, 
                function(x) x$mean[1])), 
              model_tbats_S1F7_lo80_roll, 
              model_tbats_S1F7_hi80_roll)
CI_pct_inside(unlist(lapply(model_tbats_S1F7_roll.fcast, 
                function(x) x$mean[1])), 
              model_tbats_S1F7_lo95_roll, 
              model_tbats_S1F7_hi95_roll)



#____________
# ISSUE for tbats roll infinity: ====
# - obs. forecasted demand less than 0 
#   is same as less than 100
# - have obs. of more than 10k demand
# - we can replace with the point and not the whole output
#   but it will affect output of accuracy()
# - issue can be stem from updating the model with new window
# - or our training windows is too big
# 
# SOLUTION:
# - find the indexes of forecasted point issues
# - option1 - redo the forecast base on previous day 
#   or days till no negative demand
# - option2 (chosen) - forecast base on last fitted model 
# and set h higher
#   - replace value with forecast point, don't know how 
# to replace with the whole forecast and selecting that day
#   - won't be able to use unlist to find infinity
#   - need to recalculate accuracy on those days
#   b/c observations contain output of forecast 
#   OR forecast value
#____________

fix_inf <- function(idx_inf, model_fit, forecasted_days){
  # arguments: 
  # idx_inf: indexes
  # model_fit: fitted model
  # forecasted_days: forecasted demands
  # return:
  # the corrected forecasted demands
  for (i in idx_inf) {
    cat("Index:", i, "\n")
    horizon_day <- (i - 1) %% refit_interval
    cat("horizon_day:", horizon_day, "\n")
    index_refitted <- i - horizon_day
    cat("Index refitted:", index_refitted, "\n")
    
    # it will produce 1 or more forecast, we want to extract 
    # the last forecast
    new_forecasts <- forecast(model_fit[[index_refitted]],
                              h=horizon_day)$mean
    cat("new_forecastslist:", new_forecasts, "\n")
    
    if (horizon_day == 1) {
      forecasted_days[[i]] <-new_forecasts[1]
      cat("new_forecasts:", new_forecasts[1], "\n")
    } else {
      forecasted_days[[i]] <- new_forecasts[length(new_forecasts)]
      cat("new_forecasts:", new_forecasts[length(new_forecasts)], 
          "\n")
      cat("new_forecasts list:", new_forecasts, "\n")
    }
    
  }
  
  return(forecasted_days)
}

# find indexes of observations that are negative and inf
# - wether I put <0 or <100, it's same number
idx_inf_issues <- which(unlist(lapply(model_tbats_S2F7_roll.fcast, 
                                      function(x) x$mean[1])) < 100)
idx_inf_issues2 <- which(unlist(lapply(model_tbats_S2F7_roll.fcast, 
                                   function(x) x$mean[1])) > 10000)

model_tbats_S2F7_roll.fcast_NO_INF <- fix_inf(idx_inf_issues, 
                                        model_tbats_S2F7_roll.fit, 
                                        model_tbats_S2F7_roll.fcast)

model_tbats_S2F7_roll.fcast_NO_INF_10000 <- fix_inf(idx_inf_issues2, 
                                  model_tbats_S2F7_roll.fit, 
                                  model_tbats_S2F7_roll.fcast_NO_INF)

# to recalculate accuracy
test_tbats_S2F7_mape_roll_NO_INF_10000 <- test_tbats_S2F7_mape_roll 

for (i in idx_inf_issues) {
  cat("Index:", i, "\n") 
  test_tbats_S2F7_mape_roll_NO_INF_10000[i] <- 
    accuracy(model_tbats_S2F7_roll.fcast_NO_INF_10000[[i]], 
             windows.validation[i])[1,5]
  cat("new acc:", test_tbats_S2F7_mape_roll_NO_INF_10000[i])
}

# test_tbats_S2F7_mape_roll_NO_INF_10000<- 
# test_tbats_S2F7_mape_roll_NO_INF
# 
for (i in idx_inf_issues2) {
  cat("Index:", i, "\n")
  test_tbats_S2F7_mape_roll_NO_INF_10000[i] <- 
    accuracy(model_tbats_S2F7_roll.fcast_NO_INF_10000[[i]], 
             windows.validation[i])[1,5]
  cat("new acc:", test_tbats_S2F7_mape_roll_NO_INF_10000[i])
}

mean(test_tbats_S2F7_mape_roll_NO_INF_10000)
