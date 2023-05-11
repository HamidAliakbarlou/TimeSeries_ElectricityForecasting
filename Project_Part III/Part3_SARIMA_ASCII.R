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
1+1
# max daily peak
df <- data.frame(day=as.Date(day,"%Y-%m-%d"), GENESE=GENESE)

# aggregate df
GENESE_Daily_max <- aggregate(GENESE ~ day, df, 
                              function(x) c(dailydemand = max(x)))

head(GENESE_Daily_max[[2]])
head(GENESE_Daily_max[[1]])
Yt_max <- timeSeries(GENESE_Daily_max[[2]], GENESE_Daily_max[[1]],
                     format="%Y-%m-%d")

##head(GENESE_Daily_max[[2]])
##head(GENESE_Daily_max[[1]])
##head(Yt_max)

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
length(demand_max_daily_impute)
class(demand_max_daily_impute)
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
##### note: "demand_max_daily_impute" has  times series class.
#####       "nyiso_impute.ts" has  ts class.

# Changing the name ----------------
## it has "time series type not "ts"
Ymax_Demand = demand_max_daily_impute 

# FREQUENCY 1
nyiso_impute.ts <- as.ts(demand_max_daily_impute)
# class((demand_max_daily_impute))
# class(nyiso_impute.ts)
# head(demand_max_daily_impute)
# head(nyiso_impute.ts)
# class(nyiso_impute.ts)


# output:   train set(5 years)          1       1826 
find_row_indices(demand_max_daily_impute,c('2015-01-01','2019-12-31')
                 )
# output:   train set(2 years)        1097      1826 
find_row_indices(demand_max_daily_impute,c('2018-01-01','2019-12-31')
)
find_row_indices
# ouptut:   Validation set(2 years)   1827       2557 
find_row_indices(demand_max_daily_impute,c('2020-01-01','2021-12-31')
                 )
# output:   test set(1 year)          2558       2922 
find_row_indices(demand_max_daily_impute,c('2022-01-01','2022-12-31')
                 )

# Changing the name ----------------

# train from 2015-2019
windows.train <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[1]
                        , end=time(nyiso_impute.ts)[1826])

d_train = window(Ymax_Demand, start=time(Ymax_Demand)[1], 
                 end=time(Ymax_Demand)[1826])
d_train2 = window(Ymax_Demand, start=time(Ymax_Demand)[731], 
                 end=time(Ymax_Demand)[1826])## considers two years

# validation from 2020-2021
windows.validation <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[1827]
                             , end=time(nyiso_impute.ts)[2557])

d_valid = window(Ymax_Demand, start=time(Ymax_Demand)[1827], 
                 end=time(Ymax_Demand)[2557])


# test from 2022
windows.test <- 
  window(nyiso_impute.ts, start=time(nyiso_impute.ts)[2558]
                       , end=time(nyiso_impute.ts)[2922])

d_test = window(Ymax_Demand, start=time(Ymax_Demand)[2558], 
                         end=time(Ymax_Demand)[2922])


#===========  BOX-COX TRANSFORMATION & DIFF ANALYSIS  ==========

plot(d_train,main="Training set",
     ylab="Peak Demand for GENESE zone (in MW)",
     xlab="2015-01-01 to 2019-12-31", at="pretty")
#From this plot we can clearly 

#Plot of the ACF to show non stationarity
acf(Ymax_Demand,max.lag=50,main="ACF plot of the time series")

#Do we require BoxCox transformation?

lambda = BoxCox.lambda(Ymax_Demand)
lambda_train = BoxCox.lambda(d_train)
lambda_train2 = BoxCox.lambda(d_train2) #considers two years of train
lambda
lambda_train  #out put: Lambda = lambda_train = -0.99

YMt.mean = applySeries(Ymax_Demand,FUN=colMeans)
YMt.sd   = applySeries(Ymax_Demand,FUN=colSds)
YMt.mean.bc = applySeries(BoxCox(Ymax_Demand,lambda),FUN=colMeans)
YMt.sd.bc = applySeries(BoxCox(Ymax_Demand,lambda),FUN=colSds)
par(mfrow=c(2,2))

plot(series(YMt.mean),series(YMt.sd), 
     xlab=" Average monthly peak of the demand",
     ylab="Standard Deviation",
     main="Demand Peak Before Box-Cox Transfo")
BCB = lm(YMt.sd~YMt.mean)
abline(BCB, col="red", lwd=2)
plot(series(YMt.mean.bc),series(YMt.sd.bc),
     xlab="Average monthly peak of the deman",
     ylab="Standard Deviation",
     main=paste("Demand Peak After Box-Cox Transfo"))
BCA = lm(YMt.sd.bc~YMt.mean.bc)
abline(BCA, col="red", lwd=2)

hist(Ymax_Demand,main="Before Box-Cox Transfo",
     xlab="Demand Peak")
hist(BoxCox(Ymax_Demand,lambda),
     main="After Box-Cox Transfo",
     xlab="BoxCox of the Demand Peak")


#Showing the impact of the series (training set when we differentiate 
#at lag 7 and lag1)

diffl1 = diff(Ymax_Demand, lag=1)
head(diffl1)
plot(diffl1,main="Series with Lag 1 differencing",
     ylab="Difference in MW",xlab="Time", type="l", cex.main = 2)
abline(h=0, col="red", lwd=2)

diffl7 = diff(Ymax_Demand, lag=7)
#diffl7 = d_train
head(diffl7)
plot(diffl7,main="Series with Lag 7 differencing",
     ylab="Difference in MW",xlab="Time", type="l", cex.main = 2)
abline(h=0, col="red", lwd=2)


# Showing the impact of the transformed series
# (transformed training set when we differentiate 
# at lag 7 and lag1)

diffl1BC = diff(BoxCox(d_train,lambda_train), lag=1)
plot(diffl1BC,main="Series with Lag 1 differencing 
     and Box-Cox Transformation",ylab="Difference in MW"
     ,xlab="Years", type="l", cex.main = 1.7)
abline(h=0, col="red", lwd=2)

diffl7BC = diff(BoxCox(d_train,lambda_train), lag=7, cex.main = 2)
plot(diffl7BC,main="Series with Lag 7 differencing 
     and Box-Cox Transformation",ylab="Difference in MW"
     ,xlab="Years", type="l", cex.main = 1.7)
abline(h=0, col="red", lwd=2)


library(astsa)
par(mfrow=c(1,1))
acf2(diffl7, main="ACF & PACF of the differenced series at lag 7",
     max.lag = 50, cex.main = 1)
acf2(diffl7BC,main="ACF & PACF of the differenced series at lag 7 
     with BoxCox Transformation",  cex.main = 1)

#So both ACF & PACF plots show very similar results with and without 
#the BoxCox Transformation With diff at lag 7, we have stationnary 
#data!




#--------------------------------------------------------------------
#============================= SARIMA Model =========================
#--------------------------------------------------------------------

#--- Working on last 2 years of training set instead of whole 5 years
#d_train = d_train2  ## train data for last two years
lambda_train = lambda_train2

#--- Implement the selected BoxCox transformation
#lambda_train_NoBCox = 1
d_train_BCox <- as.numeric(BoxCox(d_train, lambda_train))


#--- SARIMA
#iterative process to investigate & reach adequate SARIMA models

# First attempt
#(Adding big_P +1)--> A bit improvement than previous--> Keep it
###SARIMA_2 = sarima(d_train_BCox,1,0,1, 1,0,1,7)

#(Adding q +1) --> much Improvement--> **ADEQUATE MODEL**
###SARIMA_4 = sarima(d_train_BCox,2,1,2, 1,0,1,7)

#(Adding big_P +1) --> Much Improvement -->  **ADEQUATE MODEL**
###SARIMA_5 = sarima(d_train_BCox,2,1,2, 2,0,1,7)  

#(Adding p +1) --> No difference  -->  but is an *ADEQUATE MODEL*
###SARIMA_8 = sarima(d_train_BCox,3,1,2, 2,0,1,7)   



#_____________Above models had an issue with BoxCox trnaformation
# We found there would be an issue when combining transformation
# and differencing functions through SARIMA function
#_____________ So, let's try a different way to fix the issue:

# In this way, first we difference lag 1 and then fit SARIMA model

d_train_BCox_diff = diff(d_train_BCox, lag=1)
#SARIMA_1_t = sarima(d_train_BCox, 2,1,2, 2,0,1,7)
#SARIMA_2_t = sarima(d_train_BCox_diff, 1,0,1, 1,0,1,7) #not good 
#SARIMA_2_t = sarima(d_train_BCox_diff, 2,0,2, 1,0,1,7) #not good 
#SARIMA_2_t = sarima(d_train_BCox_diff, 2,0,2, 2,0,1,7) #not good 
#SARIMA_2_t = sarima(d_train_BCox_diff, 2,0,2, 2,0,1,7) #not bad 
SARIMA_3_t = sarima(d_train_BCox_diff, 3,0,2, 2,0,1,7) #*Adequate***
#SARIMA_4_t = sarima(d_train_BCox_diff, 3,0,2, 1,0,1,7) #became worse
#SARIMA_5_t = sarima(d_train_BCox_diff, 3,0,1, 2,0,1,7) #much worse
#SARIMA_6_t = sarima(d_train_BCox_diff, 3,0,2, 2,0,2,7) #worse ( Q<2)
#SARIMA_7_t = sarima(d_train_BCox_diff, 3,0,3, 2,0,2,7) #worse ( q<2)
#SARIMA_9_t = sarima(d_train_BCox_diff, 3,0,2, 3,0,2,7) #worse ( P<3)
SARIMA_10_t = sarima(d_train_BCox_diff, 3,0,2, 1,0,2,7) #Adequate***
#SARIMA_11_t = sarima(d_train_BCox_diff, 3,0,2, 1,0,1,7) # Not good



#___________ Now lets try with diffrencing at lag 7 

d_train_BCox_diff7 = diff(d_train_BCox,lag=7)
#SARIMA_1_t = sarima(d_train_BCox, 2,1,2, 2,0,1,7)
# SARIMA_2_t = sarima(d_train_BCox_diff7, 1,0,1, 1,0,1,7)#not good 
# SARIMA_2_t = sarima(d_train_BCox_diff7, 2,0,2, 1,0,1,7)#not good 
# SARIMA_2_t = sarima(d_train_BCox_diff7, 2,0,2, 2,0,1,7)#not good 
# SARIMA_2_t = sarima(d_train_BCox_diff7, 2,1,2, 2,0,1,7)#not bad 
# SARIMA_3_t = sarima(d_train_BCox_diff7, 3,1,2, 2,0,1,7)#*Adequate**
# SARIMA_4_t = sarima(d_train_BCox_diff7, 3,1,2, 1,0,1,7)#became wors
# SARIMA_5_t = sarima(d_train_BCox_diff7, 3,1,1, 2,0,1,7)#much worse
# SARIMA_6_t = sarima(d_train_BCox_diff7, 3,1,2, 2,0,2,7)#worse (Q<2)
SARIMA_7_t = sarima(d_train_BCox_diff7, 3,1,3, 2,0,2,7) #worse ( q<2)
# SARIMA_9_t = sarima(d_train_BCox_diff7, 3,1,2, 3,0,2,7)#worse(P<3)
# SARIMA_10_t = sarima(d_train_BCox_diff7, 3,1,2, 1,0,2,7)#Adequate*
# SARIMA_11_t = sarima(d_train_BCox_diff7, 3,1,2, 1,0,1,7)#Not good
# ----- Now we achieved some adequate models-->


##----------SO for based on Training set ADEQUATE MODELS are:
SARIMA_3_t = sarima(d_train_BCox_diff, 3,0,2, 2,0,1,7) #*Adequate***
SARIMA_10_t = sarima(d_train_BCox_diff, 3,0,2, 1,0,2,7) #Adequate***
#-- and there was pretty no difference between them

#Overall performance of the SARIMA ADEQUATE MODELS
cat("ARIMA(3,0,2)(1,0,2)[7] - AIC:",
    SARIMA_3_t $AIC," BIC:",SARIMA_3_t $BIC,"\n")
cat("ARIMA(3,0,2)(2,0,1)[7] - AIC:",
    SARIMA_10_t $AIC," BIC:",SARIMA_10_t $BIC,"\n")

# out put:
# ARIMA(2,1,2)(2,0,1)[7] - AIC: -16.94243  BIC: -16.91224
# ARIMA(3,1,2)(2,0,1)[7] - AIC: -16.9424  BIC: -16.91221 


# Conclusion: Based on AIC: 
# no difference, so take the model with bigger P for forecasting
# Model ****SARIMA(3,1,2)(2,0,1)[7] is our SELECTED MODEL****
BEST_MODEL = sarima(d_train_BCox_diff, 3,0,2, 2,0,1,7) #**Adequate**


# ---------- Our SELECTED MODEL is SARIMA(3,1,2)(2,0,1)[7]
# **********SO let's try (3,1,2)(2,0,1)[7] for forecasting**********
#----------------------------------------------------------




#=============== SARIMA-- Expanding window ==========================
#=============== SARIMA-- Expanding window ==========================
#The sarima fixed value come from the coefficients from the SARIMA_6
#model above
#d_valid = d_test
# making the vector of train and valid set
d_all =  rbind(d_train, d_valid)                   

length(d_all)
SARIMA_exp = NULL
pred_sarima_exp = NULL
se_sarima_exp_1 = NULL
nrow(d_train)

length= length(d_valid)
lower = rep(0,length)
upper = rep(0,length)
val = rep(0,length)

#------Finding the SELECTED ADEQUATE MODEL fitted parameters
# fitted parameters for model with BoxCox transformation
BEST_MODEL <- arima(d_train_BCox,order=c(3,1,2),
                  seasonal=list(order=c(2,0,1),period=7))
print(BEST_MODEL)

fixed_Param_BCox=c(1.1465,  -0.4421,  0.046,  -1.3022,  0.3726,
                   1.0463, -0.0464,  -0.9895)


#-----A loop over the valid_set to apply sarima(2,1,2, 2,0,1,7)

for (i in (nrow(d_train)+1):nrow(d_all))
{
  SARIMA_exp = sarima.for(BoxCox(d_all[1:i-1],lambda_train),
                          n.ahead = 1,
                          p=3,d=1,q=2, P=2,D=0,Q=1,S=7,
                          fixed=fixed_Param_BCox,
                          no.constant = TRUE,plot = FALSE)                                             
  
  val[i - 1826] =InvBoxCox(SARIMA_exp$pred[1],lambda_train)
  
  lower[i - 1826] = InvBoxCox(SARIMA_exp$pred[1] -
                                (1.96*SARIMA_exp$se[1]),lambda_train)
  
  upper[i - 1826] = InvBoxCox(SARIMA_exp$pred[1] +
                                (1.96*SARIMA_exp$se[1]),lambda_train)
}


# -----Calculating accuracy of the model on validation set
Pred_SARIMA_exp<-ts(val)
acc_sarima_exp = accuracy(Pred_SARIMA_exp,d_valid)[,1:5]
print(acc_sarima_exp)

# -----Finding Coverage ratio of prediction interval
match = 0

for(i in 1:length){
  if(d_valid[i] <= upper[i] && d_valid[i] >= lower[i]){
    match = match + 1
  }
}

perc_match  = match / length
perc_match # 92.74%

#================= SARIMA-- Rolling window ==========================
#================= SARIMA-- Rolling window ==========================
lower_rol = rep(0,length)
upper_rol = rep(0,length)
val_rol = rep(0,length)

for (i in (nrow(d_train)+1):nrow(d_all))
{
  SARIMA_roll = sarima.for(BoxCox(d_all[(i-nrow(d_train)):i-1],
                                 lambda_train),
                          n.ahead = 1,
                          p=3,d=1,q=2, P=2,D=0,Q=1,S=7,
                          fixed=fixed_Param_BCox,
                          no.constant = TRUE,plot = FALSE)   
  
  val_rol[i-1826] =InvBoxCox(SARIMA_roll$pred[1],lambda_train)
  
  lower_rol[i - 1826] = InvBoxCox(SARIMA_roll$pred[1] -
                              (1.96*SARIMA_roll$se[1]),lambda_train)
  
  upper_rol[i - 1826] = InvBoxCox(SARIMA_roll$pred[1] +
                              (1.96*SARIMA_roll$se[1]),lambda_train)
}

Pred_SARIMA_rolling<-ts(val_rol)
acc_sarima_roll = accuracy(Pred_SARIMA_rolling,d_valid)[,1:5]
print(acc_sarima_roll)

match1 = 0

for(i in 1:length){
  if(d_valid[i] <= upper_rol[i] && d_valid[i] >= lower_rol[i]){
    match1 = match1 + 1
  }
}

perc_match1  = match1 / 731
perc_match1 # 92.88%


#================= Report the output by each Season =============
################# Report the output by each Season ##############

PI_Season_Function <- function(observed, predicted, lower_bound
                               , upper_bound, length){
  # arguments: 
    
  
  # Save variables x and y as an R object to do Diebold-Mariano test
  save(observed, predicted, file = "my_variables.RData")
  
  
  # (MAPE & coverage with upper and lower bounds for each season)
  
  train_index <- 1826
  n = train_index
  validation_length <- length
  #-------
  # Create an empty matrix to hold the results
  results <- matrix(NA, nrow = length(observed), ncol = 4)
  colnames(results) <- c("observed", "predicted", "lower_bound",
                         "upper_bound")
  
  # Assign the four time series variables to the matrix columns
  results[, "observed"] <- observed
  results[, "predicted"] <- predicted
  results[, "lower_bound"] <- lower_bound
  results[, "upper_bound"] <- upper_bound
  #---------
  
  results <- as.data.frame(results)
  #### results$date <- DATE[1827:2557]
  
  # Calculate coverage
  coverage <- sum(results$observed >= 
                    results$lower_bound & results$observed <=
                    results$upper_bound) / validation_length
  cat("Total Coverage:", coverage, '\n')
  
  # Plot coverage with upper and lower bounds for each season
  ## Whole validation set
  pred_total <- c(results$predicted)
  obsv_total <- c(results$observed)
  lw_bd_total <- c(results$lower_bound)
  up_bd_total <- c(results$upper_bound)
  
  plot(obsv_total, type = "l", col = "blue", 
       ylim = range(c(lw_bd_total, up_bd_total)), 
       main = "Whole Validation set", xlab = "", ylab = "")
  mtext("Time", side = 1, line = 2, cex = 0.8) 
  mtext("Daily demand", side = 2, line = 2, cex = 0.8) 
  lines(pred_total, col = "red")
  x_vals <- c(1:length(obsv_total), length(obsv_total):1)
  y_vals <- c(lw_bd_total, rev(up_bd_total))
  polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
  legend("topright", legend = c("Observed", "Predicted", "PI"),
         fill = c("blue", "red", "grey"), 
         bg = "white", x.intersp = 0.2, y.intersp = 0.7, cex = 0.8)
  
  
  
  #--------- Calculations to split into Seasons
  if (length == 731){
    # it calculates for validation set which consists of 2 years
    
    pred_wint <- c(results$predicted[1:91],
                   results$predicted[367:456])
    obsv_wint <- c(results$observed[1:91],
                   results$observed[367:456])
    lw_bd_wint <- c(results$lower_bound[1:91],
                    results$lower_bound[367:456])
    up_bd_wint <- c(results$upper_bound[1:91],
                    results$upper_bound[367:456])
    
    pred_spri <- c(results$predicted[92:182],
                   results$predicted[457:547])
    obsv_spri <- c(results$observed[92:182],
                   results$observed[457:547])
    lw_bd_spri <- c(results$lower_bound[92:182],
                    results$lower_bound[457:547])
    up_bd_spri <- c(results$upper_bound[92:182],
                    results$upper_bound[457:547])
    
    pred_summ <- c(results$predicted[183:274],
                   results$predicted[548:639])
    obsv_summ <- c(results$observed[183:274],
                   results$observed[548:639])
    lw_bd_summ <- c(results$lower_bound[183:274],
                    results$lower_bound[548:639])
    up_bd_summ <- c(results$upper_bound[183:274],
                    results$upper_bound[548:639])
    
    pred_fall <- c(results$predicted[275:366],
                   results$predicted[640:731])
    obsv_fall <- c(results$observed[275:366],
                   results$observed[640:731])
    lw_bd_fall <- c(results$lower_bound[275:366],
                    results$lower_bound[640:731])
    up_bd_fall <- c(results$upper_bound[275:366],
                    results$upper_bound[640:731])
    
  } else {    
    # it calculates for test set which consists of 1 year
    
    pred_wint <- c(results$predicted[1:91])
    obsv_wint <- c(results$observed[1:91])
    lw_bd_wint <- c(results$lower_bound[1:91])
    up_bd_wint <- c(results$upper_bound[1:91])
    
    pred_spri <- c(results$predicted[92:182])
    obsv_spri <- c(results$observed[92:182])
    lw_bd_spri <- c(results$lower_bound[92:182])
    up_bd_spri <- c(results$upper_bound[92:182])
    
    pred_summ <- c(results$predicted[183:274])
    obsv_summ <- c(results$observed[183:274])
    lw_bd_summ <- c(results$lower_bound[183:274])
    up_bd_summ <- c(results$upper_bound[183:274])
    
    pred_fall <- c(results$predicted[275:365])
    obsv_fall <- c(results$observed[275:365])
    lw_bd_fall <- c(results$lower_bound[275:365])
    up_bd_fall <- c(results$upper_bound[275:365])
    
  }
  
  
  # Function to calculate MAPE
  mape <- function(observed, predicted) {
    return (mean(abs((observed - predicted) / observed)) * 100)
  }
  
  # Function to calculate coverage
  coverage <- function(observed, lower_bound, upper_bound) {
    covered <- (observed >= lower_bound) & (observed <= upper_bound)
    return (mean(covered) * 100)
  }
  
  # Calculate MAPE and coverage for each season
  mape_wint <- mape(obsv_wint, pred_wint)
  coverage_wint <- coverage(obsv_wint, lw_bd_wint, up_bd_wint)
  
  mape_spri <- mape(obsv_spri, pred_spri)
  coverage_spri <- coverage(obsv_spri, lw_bd_spri, up_bd_spri)
  
  mape_summ <- mape(obsv_summ, pred_summ)
  coverage_summ <- coverage(obsv_summ, lw_bd_summ, up_bd_summ)
  
  mape_fall <- mape(obsv_fall, pred_fall)
  coverage_fall <- coverage(obsv_fall, lw_bd_fall, up_bd_fall)
  
  # Create a data frame to display MAPE&coverage for each season
  seasonal_metrics <- data.frame(
    Season = c("Winter", "Spring", "Summer", "Fall"),
    MAPE = c(mape_wint, mape_spri, mape_summ, mape_fall),
    Coverage = c(coverage_wint, coverage_spri,
                 coverage_summ, coverage_fall)
  )
  
  # Print the data frame
  print(seasonal_metrics)
  
  # The output for validation set: 
  #   Season     MAPE  Coverage
  # 1 Winter 2.903377 100.00000
  # 2 Spring 6.374293  86.81319
  # 3 Summer 7.706625  85.32609
  # 4   Fall 2.829626  98.91304
  
  
  # Plot coverage with upper and lower bounds for each season
  
  # Set up a 2x2 plot layout
  par(mfrow = c(2, 2))
  
  
  # Winter
  plot(obsv_wint, type = "l", col = "blue",
       ylim = range(c(lw_bd_wint, up_bd_wint)),
       main = "Winter", xlab = "", ylab = "")
  lines(pred_wint, col = "red")
  mtext("Time", side = 1, line = 2, cex = 0.8) 
  mtext("Daily demand", side = 2, line = 2, cex = 0.8) 
  x_vals <- c(1:length(obsv_wint), length(obsv_wint):1)
  y_vals <- c(lw_bd_wint, rev(up_bd_wint))
  polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
  legend("topright", legend = c("Observed", "Predicted", "PI"),
         fill = c("blue", "red", "grey"), 
         bg = "white", x.intersp = 0.2, y.intersp = 0.7, cex = 0.8)
  
  # Spring
  plot(obsv_spri, type = "l", col = "blue",
       ylim = range(c(lw_bd_spri, up_bd_spri)),
       main = "Spring", xlab = " ", ylab = " ")
  lines(pred_spri, col = "red")
  mtext("Time", side = 1, line = 2, cex = 0.8) 
  mtext("Daily demand", side = 2, line = 2, cex = 0.8) 
  x_vals <- c(1:length(obsv_spri), length(obsv_spri):1)
  y_vals <- c(lw_bd_spri, rev(up_bd_spri))
  polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
  legend("topright", legend = c("Observed", "Predicted", "PI"),
         fill = c("blue", "red", "grey"), 
         bg = "white", x.intersp = 0.2, y.intersp = 0.7, cex = 0.8)
  
  # Summer
  plot(obsv_summ, type = "l", col = "blue",
       ylim = range(c(lw_bd_summ, up_bd_summ)),
       main = "Summer", xlab = " ", ylab = " ")
  lines(pred_summ, col = "red")
  mtext("Time", side = 1, line = 2, cex = 0.8) 
  mtext("Daily demand", side = 2, line = 2, cex = 0.8) 
  x_vals <- c(1:length(obsv_summ), length(obsv_summ):1)
  y_vals <- c(lw_bd_summ, rev(up_bd_summ))
  polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
  legend("topright", legend = c("Observed", "Predicted", "PI"),
         fill = c("blue", "red", "grey"), 
         bg = "white", x.intersp = 0.2, y.intersp = 0.7, cex = 0.8)
  
  # Fall
  plot(obsv_fall, type = "l", col = "blue",
       ylim = range(c(lw_bd_fall, up_bd_fall)),
       main = "Fall", xlab = "", ylab = " ")
  lines(pred_fall, col = "red")
  mtext("Time", side = 1, line = 2, cex = 0.8) 
  mtext("Daily demand", side = 2, line = 2, cex = 0.8) 
  x_vals <- c(1:length(obsv_fall), length(obsv_fall):1)
  y_vals <- c(lw_bd_fall, rev(up_bd_fall))
  polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)
  legend("topright", legend = c("Observed", "Predicted", "PI"),
         fill = c("blue", "red", "grey"), 
         bg = "white", x.intersp = 0.2, y.intersp = 0.7, cex = 0.8)
  
  #return(3333)
  #return(seasonal_metrics)

}
#---------------
#SO, now lets call the function to have PI and coverage
#for each seasonal
# We want this for validation set to check
#if the function works well:

observed = d_valid
predicted = val_rol
lower_bound= lower_rol
upper_bound = upper_rol
length = length(d_valid)
length
#Calling the function
PI_Season_Function(observed, predicted, lower_bound,
                   upper_bound, length)


#####################################################################
#____________________________________________________________________
# # TITLE:Check the performance of SELECTED SARIMA MODEL on TEST SET
#____________________________________________________________________

#====================================================================
#--------------------------------------------------------------------
#==========================TEST SET!!!!!=============================
#--------------------------------------------------------------------
#====================================================================

#=====================    SARIMA TEST SET  ==========================
#Predictions for SARIMA                                                   
d_train_val = rbind(d_train,d_valid) 
d_all_test =  rbind(d_train,d_valid, d_test) 
length_train_val = length(d_train_val)
#length(d_train)
#length(d_test)

length(d_all_test)
SARIMA_exp = NULL
pred_sarima_exp = NULL
se_sarima_exp_1 = NULL
nrow(d_train)
nrow(d_train_val)


# Make the initial setting 
length = length(d_test)
length
lower_rol_test = rep(0,length)
upper_rol_test = rep(0,length)
val_rol_test = rep(0,length)

for (i in (nrow(d_train_val)+1):nrow(d_all_test))
{
  SARIMA_roll = sarima.for(
    BoxCox(d_all_test[(i-nrow(d_train_val)):i-1],
                                  lambda_train),
                           n.ahead = 1,
                           p=3,d=1,q=2, P=2,D=0,Q=1,S=7,
                           fixed=fixed_Param_BCox,
                           no.constant = TRUE,plot = FALSE)   
  
  val_rol_test[i - length_train_val] =
    InvBoxCox(SARIMA_roll$pred[1],lambda_train)
  
  lower_rol_test[i - length_train_val] =
    InvBoxCox(SARIMA_roll$pred[1] - 
                (1.96*SARIMA_roll$se[1]),lambda_train)
  
  upper_rol_test[i - length_train_val] =
    InvBoxCox(SARIMA_roll$pred[1] +
                 (1.96*SARIMA_roll$se[1]),lambda_train)
}


Pred_SARIMA_rolling<-ts(val_rol_test)
acc_sarima_roll = accuracy(Pred_SARIMA_rolling,d_test)[,1:5]
print(acc_sarima_roll)

match1 = 0

for(i in 1:length){
  if(d_test[i] <= upper_rol_test[i] && d_test[i] >=
     lower_rol_test[i]){
    match1 = match1 + 1
  }
}

perc_match1  = match1 / length
perc_match1 # 89%

#______________________
#============ Report the output of test set by each Season =======

# SO, now lets call the function to have
# PI and coverage for each season.
# We want this for validation set:
observed = d_test
predicted = val_rol_test
lower_bound= lower_rol_test
upper_bound = upper_rol_test
length = length(d_test)
length
PI_Season_Function(observed, predicted, lower_bound,
                   upper_bound, length)

