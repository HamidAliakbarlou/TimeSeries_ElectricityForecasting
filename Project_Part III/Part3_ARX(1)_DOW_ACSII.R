
####### functions
# Function to calculate MAPE
mape <- function(observed, predicted) {
  return (mean(abs((observed - predicted) / observed)) * 100)
}

# Function to calculate coverage
coverage <- function(observed, lower_bound, upper_bound) {
  covered <- (observed >= lower_bound) & (observed <= upper_bound)
  return (mean(covered) * 100)
}

##################################################################

######### Check residuals on in-sample

df.tmp <- data.frame(y=y[2:n],
                     xmon=xmon[2:n],
                     xtue=xtue[2:n],
                     xwed=xwed[2:n],
                     xthu=xthu[2:n],
                     xsat=xsat[2:n],
                     xsun=xsun[2:n],
                     lag1y=y[1:(n-1)])
lm <- lm(y ~ xmon 
         + xtue
         + xwed
         + xthu
         + xsat
         + xsun
         + lag1y, 
         data=df.tmp, x=T)

par(mfrow=c(2,2))

plot(lm$fitted, lm$residuals)

for (k in 2:dim(model$x)[2]) { 
  plot(model$x[,k], 
       model$residuals, 
       xlab=dimnames(model$x)[2][[1]][k]) }

par(mfrow=c(1,1))
qqnorm(lm$residuals); abline(a=0, b=1, col="blue")

par(mfrow=c(1,1))
acf(lm$residuals)

###ARX(2)

# Add the lag2y variable
df.tmp <- data.frame(y = y[3:n],
                     xmon = xmon[3:n],
                     xtue = xtue[3:n],
                     xwed = xwed[3:n],
                     xthu = xthu[3:n],
                     xsat = xsat[3:n],
                     xsun = xsun[3:n],
                     lag1y = y[2:(n - 1)],
                     lag2y = y[1:(n - 2)])

# Update the linear regression model
lm <- lm(y ~ xmon 
         + xtue
         + xwed
         + xthu
         + xsat
         + xsun
         + lag1y
         + lag2y, 
         data = df.tmp, x = T)

acf(lm$residuals)


################### ARX1 expanding window ########################
n = train_index
validation_length <- 731

results_ARX.1 <- matrix(NA, nrow = validation_length, ncol = 4)
colnames(results_ARX.1) <- c("observed", 
                             "predicted", 
                             "lower_bound", 
                             "upper_bound")

for (i in 1:validation_length) {
  window_end <- n + i
  y_expanded <- Yt[1:window_end]
  xmon_expanded <- DMont[1:window_end]
  xtue_expanded <- DTuet[1:window_end]
  xwed_expanded <- DWedt[1:window_end]
  xthu_expanded <- DThut[1:window_end]
  xsat_expanded <- DSatt[1:window_end]
  xsun_expanded <- DSunt[1:window_end]
  
  df.tmp <- data.frame(y=y_expanded[2:window_end],
                       xmon=xmon_expanded[2:window_end],
                       xtue=xtue_expanded[2:window_end],
                       xwed=xwed_expanded[2:window_end],
                       xthu=xthu_expanded[2:window_end],
                       xsat=xsat_expanded[2:window_end],
                       xsun=xsun_expanded[2:window_end],
                       lag1y=y_expanded[1:(window_end-1)])
  
  lm_expanded <- lm(y ~ xmon+xtue + xwed + xthu + xsat + xsun + lag1y, 
                    data = df.tmp, x = T)
  
  pred <- predict(lm_expanded, 
                  newdata = data.frame(
                    xmon = xmon_expanded[window_end], 
                    xtue = xtue_expanded[window_end], 
                    xwed = xwed_expanded[window_end], 
                    xthu = xthu_expanded[window_end], 
                    xsat = xsat_expanded[window_end], 
                    xsun = xsun_expanded[window_end], 
                    lag1y = y_expanded[window_end - 1]),
                  interval = "prediction", level = 0.95)
  
  results_ARX.1[i, 1] <- y_expanded[window_end]
  results_ARX.1[i, 2:4] <- pred
}

results_ARX.1 <- as.data.frame(results_ARX.1)
results_ARX.1$date <- DATE[1827:2557]

# ACF of residuals of forecasts
par(mfrow = c(1, 2))
acf(lm$residuals)
errors <- results_ARX.1$observed - results_ARX.1$predicted
acf(errors)

global_MAPE_ARX <- mape(results_ARX.1$observed, 
                        results_ARX.1$predicted)
global_coverage_ARX <- coverage(results_ARX.1$observed, 
                                results_ARX.1$lower_bound, 
                                results_ARX.1$upper_bound)

pred_wint <- c(results_ARX.1$predicted[1:91],
               results_ARX.1$predicted[367:456])
obsv_wint <- c(results_ARX.1$observed[1:91],
               results_ARX.1$observed[367:456])
lw_bd_wint <- c(results_ARX.1$lower_bound[1:91],
                results_ARX.1$lower_bound[367:456])
up_bd_wint <- c(results_ARX.1$upper_bound[1:91],
                results_ARX.1$upper_bound[367:456])

pred_spri <- c(results_ARX.1$predicted[92:182],
               results_ARX.1$predicted[457:547])
obsv_spri <- c(results_ARX.1$observed[92:182],
               results_ARX.1$observed[457:547])
lw_bd_spri <- c(results_ARX.1$lower_bound[92:182],
                results_ARX.1$lower_bound[457:547])
up_bd_spri <- c(results_ARX.1$upper_bound[92:182],
                results_ARX.1$upper_bound[457:547])

pred_summ <- c(results_ARX.1$predicted[183:274],
               results_ARX.1$predicted[548:639])
obsv_summ <- c(results_ARX.1$observed[183:274],
               results_ARX.1$observed[548:639])
lw_bd_summ <- c(results_ARX.1$lower_bound[183:274],
                results_ARX.1$lower_bound[548:639])
up_bd_summ <- c(results_ARX.1$upper_bound[183:274],
                results_ARX.1$upper_bound[548:639])

pred_fall <- c(results_ARX.1$predicted[275:366],
               results_ARX.1$predicted[640:731])
obsv_fall <- c(results_ARX.1$observed[275:366],
               results_ARX.1$observed[640:731])
lw_bd_fall <- c(results_ARX.1$lower_bound[275:366],
                results_ARX.1$lower_bound[640:731])
up_bd_fall <- c(results_ARX.1$upper_bound[275:366],
                results_ARX.1$upper_bound[640:731])


# Calculate MAPE and coverage for each season
mape_wint <- mape(obsv_wint, pred_wint)
coverage_wint <- coverage(obsv_wint, lw_bd_wint, up_bd_wint)

mape_spri <- mape(obsv_spri, pred_spri)
coverage_spri <- coverage(obsv_spri, lw_bd_spri, up_bd_spri)

mape_summ <- mape(obsv_summ, pred_summ)
coverage_summ <- coverage(obsv_summ, lw_bd_summ, up_bd_summ)

mape_fall <- mape(obsv_fall, pred_fall)
coverage_fall <- coverage(obsv_fall, lw_bd_fall, up_bd_fall)

# Create a data frame to display the MAPE and coverage for each season
seasonal_metrics_ARX1 <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  MAPE = c(mape_wint, mape_spri, mape_summ, mape_fall),
  Coverage = c(coverage_wint, 
               coverage_spri, 
               coverage_summ, 
               coverage_fall)
)

# Print the data frame
print(seasonal_metrics_ARX1)

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Winter
plot(obsv_wint, type = "l", col = "blue", 
     ylim = range(c(lw_bd_wint, up_bd_wint)), 
     main = "Winter", xlab = "Time", 
     ylab = "Daily Peak Hr Demand (Mw)")
lines(pred_wint, col = "red")
x_vals <- c(1:length(obsv_wint), length(obsv_wint):1)
y_vals <- c(lw_bd_wint, rev(up_bd_wint))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Spring
plot(obsv_spri, type = "l", col = "blue", 
     ylim = range(c(lw_bd_spri, up_bd_spri)), 
     main = "Spring", xlab = "Time", 
     ylab = "Daily Peak Hr Demand (Mw)")
lines(pred_spri, col = "red")
x_vals <- c(1:length(obsv_spri), length(obsv_spri):1)
y_vals <- c(lw_bd_spri, rev(up_bd_spri))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Summer
plot(obsv_summ, type = "l", col = "blue", 
     ylim = range(c(lw_bd_summ, up_bd_summ)), 
     main = "Summer", xlab = "Time", 
     ylab = "Daily Peak Hr Demand (Mw)")
lines(pred_summ, col = "red")
x_vals <- c(1:length(obsv_summ), length(obsv_summ):1)
y_vals <- c(lw_bd_summ, rev(up_bd_summ))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Fall
plot(obsv_fall, type = "l", col = "blue", 
     ylim = range(c(lw_bd_fall, up_bd_fall)), 
     main = "Fall", xlab = "Time", 
     ylab = "Daily Peak Hr Demand (Mw)")
lines(pred_fall, col = "red")
x_vals <- c(1:length(obsv_fall), length(obsv_fall):1)
y_vals <- c(lw_bd_fall, rev(up_bd_fall))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

################### ARX2 expanding window ########################
n = train_index
validation_length <- 731

results_2 <- matrix(NA, nrow = validation_length, ncol = 4)
colnames(results_2) <- c("observed", 
                         "predicted", 
                         "lower_bound", 
                         "upper_bound")

for (i in 1:validation_length) {
  window_end <- n + i
  y_expanded <- Yt[1:window_end]
  xmon_expanded <- DMont[1:window_end]
  xtue_expanded <- DTuet[1:window_end]
  xwed_expanded <- DWedt[1:window_end]
  xthu_expanded <- DThut[1:window_end]
  xsat_expanded <- DSatt[1:window_end]
  xsun_expanded <- DSunt[1:window_end]
  
  df.tmp <- data.frame(y = y_expanded[3:window_end],
                       xmon = xmon_expanded[3:window_end],
                       xtue = xtue_expanded[3:window_end],
                       xwed = xwed_expanded[3:window_end],
                       xthu = xthu_expanded[3:window_end],
                       xsat = xsat_expanded[3:window_end],
                       xsun = xsun_expanded[3:window_end],
                       lag1y = y_expanded[2:(window_end - 1)],
                       lag2y = y_expanded[1:(window_end - 2)])
  
  lm_expanded <- lm(y ~ xmon+xtue+xwed+xthu+xsat+xsun+lag1y+lag2y, 
                    data = df.tmp, x = T)
  
  pred <- predict(lm_expanded, 
                  newdata = data.frame(
                    xmon = xmon_expanded[window_end], 
                    xtue = xtue_expanded[window_end], 
                    xwed = xwed_expanded[window_end], 
                    xthu = xthu_expanded[window_end], 
                    xsat = xsat_expanded[window_end], 
                    xsun = xsun_expanded[window_end], 
                    lag1y = y_expanded[window_end - 1],
                    lag2y = y_expanded[window_end - 2]),
                  interval = "prediction", level = 0.95)
  
  results_2[i, 1] <- y_expanded[window_end]
  results_2[i, 2:4] <- pred
}

results_2 <- as.data.frame(results_2)
results_2$date <- DATE[1827:2557]


# ACF of residuals of forecasts
par(mfrow = c(1, 2))
acf(lm$residuals)
errors <- results_2$observed - results_2$predicted
acf(errors)



pred_wint <- c(results_2$predicted[1:91],
               results_2$predicted[367:456])
obsv_wint <- c(results_2$observed[1:91],
               results_2$observed[367:456])
lw_bd_wint <- c(results_2$lower_bound[1:91],
                results_2$lower_bound[367:456])
up_bd_wint <- c(results_2$upper_bound[1:91],
                results_2$upper_bound[367:456])

pred_spri <- c(results_2$predicted[92:182],
               results_2$predicted[457:547])
obsv_spri <- c(results_2$observed[92:182],
               results_2$observed[457:547])
lw_bd_spri <- c(results_2$lower_bound[92:182],
                results_2$lower_bound[457:547])
up_bd_spri <- c(results_2$upper_bound[92:182],
                results_2$upper_bound[457:547])

pred_summ <- c(results_2$predicted[183:274],
               results_2$predicted[548:639])
obsv_summ <- c(results_2$observed[183:274],
               results_2$observed[548:639])
lw_bd_summ <- c(results_2$lower_bound[183:274],
                results_2$lower_bound[548:639])
up_bd_summ <- c(results_2$upper_bound[183:274],
                results_2$upper_bound[548:639])

pred_fall <- c(results_2$predicted[275:366],
               results_2$predicted[640:731])
obsv_fall <- c(results_2$observed[275:366],
               results_2$observed[640:731])
lw_bd_fall <- c(results_2$lower_bound[275:366],
                results_2$lower_bound[640:731])
up_bd_fall <- c(results_2$upper_bound[275:366],
                results_2$upper_bound[640:731])


# Calculate MAPE and coverage for each season
mape_wint <- mape(obsv_wint, pred_wint)
coverage_wint <- coverage(obsv_wint, lw_bd_wint, up_bd_wint)

mape_spri <- mape(obsv_spri, pred_spri)
coverage_spri <- coverage(obsv_spri, lw_bd_spri, up_bd_spri)

mape_summ <- mape(obsv_summ, pred_summ)
coverage_summ <- coverage(obsv_summ, lw_bd_summ, up_bd_summ)

mape_fall <- mape(obsv_fall, pred_fall)
coverage_fall <- coverage(obsv_fall, lw_bd_fall, up_bd_fall)

# Create a df to display the MAPE and coverage for each season
seasonal_metrics <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  MAPE = c(mape_wint, mape_spri, mape_summ, mape_fall),
  Coverage = c(coverage_wint, 
               coverage_spri, 
               coverage_summ, 
               coverage_fall)
)

# Print the data frame
print(seasonal_metrics)


# Plot coverage with upper and lower bounds for each season
# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Winter
plot(obsv_wint, type = "l", col = "blue", 
     ylim = range(c(lw_bd_wint, up_bd_wint)), 
     main = "Winter", xlab = "Time", ylab = "Value")
lines(pred_wint, col = "red")
x_vals <- c(1:length(obsv_wint), length(obsv_wint):1)
y_vals <- c(lw_bd_wint, rev(up_bd_wint))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Spring
plot(obsv_spri, type = "l", col = "blue", 
     ylim = range(c(lw_bd_spri, up_bd_spri)), 
     main = "Spring", xlab = "Time", ylab = "Value")
lines(pred_spri, col = "red")
x_vals <- c(1:length(obsv_spri), length(obsv_spri):1)
y_vals <- c(lw_bd_spri, rev(up_bd_spri))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Summer
plot(obsv_summ, type = "l", col = "blue", 
     ylim = range(c(lw_bd_summ, up_bd_summ)), 
     main = "Summer", xlab = "Time", ylab = "Value")
lines(pred_summ, col = "red")
x_vals <- c(1:length(obsv_summ), length(obsv_summ):1)
y_vals <- c(lw_bd_summ, rev(up_bd_summ))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Fall
plot(obsv_fall, type = "l", col = "blue", 
     ylim = range(c(lw_bd_fall, up_bd_fall)), 
     main = "Fall", xlab = "Time", ylab = "Value")
lines(pred_fall, col = "red")
x_vals <- c(1:length(obsv_fall), length(obsv_fall):1)
y_vals <- c(lw_bd_fall, rev(up_bd_fall))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)


######### Check residuals on in-sample for ARX with HDD + CDD

df.tmp <- data.frame(y=y[2:n],
                     hdd=HDD45t[2:n],
                     cdd=CDD72t[2:n],
                     lag1y=y[1:(n-1)])

lm <- lm(y ~ hdd
         + cdd
         + lag1y, 
         data=df.tmp, x=T)


par(mfrow=c(2,2))

plot(lm$fitted, lm$residuals)

for (k in 2:dim(model$x)[2]) { 
  plot(model$x[,k], model$residuals, 
       xlab=dimnames(model$x)[2][[1]][k]) }

par(mfrow=c(1,1))
qqnorm(lm$residuals); abline(a=0, b=1, col="blue")

par(mfrow=c(1,1))
acf(lm$residuals)

################### ARX1 expanding window HDD CDD ###################
n = train_index
validation_length <- 731

results_ARX_CDD_HDD <- matrix(NA, nrow = validation_length, ncol = 4)
colnames(results_ARX_CDD_HDD) <- c("observed", 
                                   "predicted", 
                                   "lower_bound", 
                                   "upper_bound")

for (i in 1:validation_length) {
  window_end <- n + i
  y_expanded <- Yt[1:window_end]
  xmon_expanded <- DMont[1:window_end]
  xtue_expanded <- DTuet[1:window_end]
  xwed_expanded <- DWedt[1:window_end]
  xthu_expanded <- DThut[1:window_end]
  xsat_expanded <- DSatt[1:window_end]
  xsun_expanded <- DSunt[1:window_end]
  hdd_expanded <- HDD45t[1:window_end]
  cdd_expanded <- CDD72t[1:window_end]
  
  df.tmp <- data.frame(y=y_expanded[2:window_end],
                       xmon=xmon_expanded[2:window_end],
                       xtue=xtue_expanded[2:window_end],
                       xwed=xwed_expanded[2:window_end],
                       xthu=xthu_expanded[2:window_end],
                       xsat=xsat_expanded[2:window_end],
                       xsun=xsun_expanded[2:window_end],
                       hdd=hdd_expanded[2:window_end],
                       cdd=cdd_expanded[2:window_end],
                       lag1y=y_expanded[1:(window_end-1)])
  
  lm_expanded <- lm(y ~ xmon+xtue+xwed+xthu+xsat+xsun+hdd+cdd+lag1y, 
                    data = df.tmp, x = T)
  
  pred <- predict(lm_expanded, 
                  newdata = data.frame(
                    xmon = xmon_expanded[window_end], 
                    xtue = xtue_expanded[window_end], 
                    xwed = xwed_expanded[window_end], 
                    xthu = xthu_expanded[window_end], 
                    xsat = xsat_expanded[window_end], 
                    xsun = xsun_expanded[window_end], 
                    hdd = hdd_expanded[window_end],
                    cdd = cdd_expanded[window_end],
                    lag1y = y_expanded[window_end - 1]),
                  interval = "prediction", level = 0.95)
  
  results_ARX_CDD_HDD[i, 1] <- y_expanded[window_end]
  results_ARX_CDD_HDD[i, 2:4] <- pred
}

results_ARX_CDD_HDD <- as.data.frame(results_ARX_CDD_HDD)
results_ARX_CDD_HDD$date <- DATE[1827:2557]


# ACF of residuals of forecasts
par(mfrow = c(1, 2))
acf(lm$residuals)
errors <- results_ARX_CDD_HDD$observed - results_ARX_CDD_HDD$predicted
acf(errors)



pred_wint <- c(results_ARX_CDD_HDD$predicted[1:91],
               results_ARX_CDD_HDD$predicted[367:456])
obsv_wint <- c(results_ARX_CDD_HDD$observed[1:91],
               results_ARX_CDD_HDD$observed[367:456])
lw_bd_wint <- c(results_ARX_CDD_HDD$lower_bound[1:91],
                results_ARX_CDD_HDD$lower_bound[367:456])
up_bd_wint <- c(results_ARX_CDD_HDD$upper_bound[1:91],
                results_ARX_CDD_HDD$upper_bound[367:456])

pred_spri <- c(results_ARX_CDD_HDD$predicted[92:182],
               results_ARX_CDD_HDD$predicted[457:547])
obsv_spri <- c(results_ARX_CDD_HDD$observed[92:182],
               results_ARX_CDD_HDD$observed[457:547])
lw_bd_spri <- c(results_ARX_CDD_HDD$lower_bound[92:182],
                results_ARX_CDD_HDD$lower_bound[457:547])
up_bd_spri <- c(results_ARX_CDD_HDD$upper_bound[92:182],
                results_ARX_CDD_HDD$upper_bound[457:547])

pred_summ <- c(results_ARX_CDD_HDD$predicted[183:274],
               results_ARX_CDD_HDD$predicted[548:639])
obsv_summ <- c(results_ARX_CDD_HDD$observed[183:274],
               results_ARX_CDD_HDD$observed[548:639])
lw_bd_summ <- c(results_ARX_CDD_HDD$lower_bound[183:274],
                results_ARX_CDD_HDD$lower_bound[548:639])
up_bd_summ <- c(results_ARX_CDD_HDD$upper_bound[183:274],
                results_ARX_CDD_HDD$upper_bound[548:639])

pred_fall <- c(results_ARX_CDD_HDD$predicted[275:366],
               results_ARX_CDD_HDD$predicted[640:731])
obsv_fall <- c(results_ARX_CDD_HDD$observed[275:366],
               results_ARX_CDD_HDD$observed[640:731])
lw_bd_fall <- c(results_ARX_CDD_HDD$lower_bound[275:366],
                results_ARX_CDD_HDD$lower_bound[640:731])
up_bd_fall <- c(results_ARX_CDD_HDD$upper_bound[275:366],
                results_ARX_CDD_HDD$upper_bound[640:731])


# Calculate MAPE and coverage for each season
mape_wint <- mape(obsv_wint, pred_wint)
coverage_wint <- coverage(obsv_wint, lw_bd_wint, up_bd_wint)

mape_spri <- mape(obsv_spri, pred_spri)
coverage_spri <- coverage(obsv_spri, lw_bd_spri, up_bd_spri)

mape_summ <- mape(obsv_summ, pred_summ)
coverage_summ <- coverage(obsv_summ, lw_bd_summ, up_bd_su31mm)

mape_fall <- mape(obsv_fall, pred_fall)
coverage_fall <- coverage(obsv_fall, lw_bd_fall, up_bd_fall)

# Create a df to display the MAPE and coverage for each season
seasonal_metrics_ARX1_HDD_CDD <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  MAPE = c(mape_wint, mape_spri, mape_summ, mape_fall),
  Coverage = c(coverage_wint, 
               coverage_spri, 
               coverage_summ, 
               coverage_fall)
)

# Print the data frame
print(seasonal_metrics_ARX1_HDD_CDD)

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Winter
plot(obsv_wint, type = "l", col = "blue", 
     ylim = range(c(lw_bd_wint, up_bd_wint)), 
     main = "Winter", xlab = "Time", 
     ylab = "Daily Peak Hourly Demand (Mw)")
lines(pred_wint, col = "red")
x_vals <- c(1:length(obsv_wint), length(obsv_wint):1)
y_vals <- c(lw_bd_wint, rev(up_bd_wint))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Spring
plot(obsv_spri, type = "l", col = "blue", 
     ylim = range(c(lw_bd_spri, up_bd_spri)), 
     main = "Spring", xlab = "Time", 
     ylab = "Daily Peak Hourly Demand (Mw)")
lines(pred_spri, col = "red")
x_vals <- c(1:length(obsv_spri), length(obsv_spri):1)
y_vals <- c(lw_bd_spri, rev(up_bd_spri))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Summer
plot(obsv_summ, type = "l", col = "blue", 
     ylim = range(c(lw_bd_summ, up_bd_summ)), 
     main = "Summer", xlab = "Time", 
     ylab = "Daily Peak Hourly Demand (Mw)")
lines(pred_summ, col = "red")
x_vals <- c(1:length(obsv_summ), length(obsv_summ):1)
y_vals <- c(lw_bd_summ, rev(up_bd_summ))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

# Fall
plot(obsv_fall, type = "l", col = "blue", 
     ylim = range(c(lw_bd_fall, up_bd_fall)), 
     main = "Fall", xlab = "Time", 
     ylab = "Daily Peak Hourly Demand (Mw)")
lines(pred_fall, col = "red")
x_vals <- c(1:length(obsv_fall), length(obsv_fall):1)
y_vals <- c(lw_bd_fall, rev(up_bd_fall))
polygon(x_vals, y_vals, col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)




















