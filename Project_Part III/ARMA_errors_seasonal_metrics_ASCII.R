################## In sample ARMA errors################

fit <- auto.arima(Yt_train, xreg=cbind(
  HDD45t_train, CDD72t_train,
  lag(HDD45t_train, 1), lag(HDD45t_train, 2),
  lag(CDD72t_train, 1), lag(CDD72t_train, 2),
  WNt_train, Holidayt_train, priort_train,
  Nextt_train,
  DMon[1:1826], DTue[1:1826], DWed[1:1826], 
  DThu[1:1826], DSat[1:1826], DSun[1:1826]))

par(mfrow=c(1,1))
acf(residuals(fit)[-(1:2)])
title(main="With proper error structure (using auto.arima)")


################## run on validation expadning window ##############
train_length <- length(windows.train)-2
valid_length <- length(windows.validation)

# Create an empty data frame to store the observed values, forecasts, and prediction intervals
results_ARMA <- data.frame(Observed = numeric(valid_length), 
                           Predicted = numeric(valid_length), 
                           Lower = numeric(valid_length), 
                           Upper = numeric(valid_length))

# Add these lines before the loop
new_xreg_matrix <- matrix(NA, nrow = n, ncol = 16)

# Create an empty vector to store the one-step-ahead forecasts
forecasts <- NULL

# Record the start time
start_time <- Sys.time()

# Loop through the validation set
for (i in 1:valid_length) {
  # Combine the train set with the current obsv from the valid set
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
  
  # Create a matrix of the explanatory variables
  xreg_matrix <- 
    cbind(current_HDD45t@.Data[-length(current_HDD45t@.Data)],
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
        current_DSun@.Data[-length(current_DSun@.Data)])
  
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
                         current_DSun@.Data)                       
  

  # Fit the linear model with ARMA errors using auto.arima
  model <- auto.arima(current_data, xreg = xreg_matrix)
  
  # Make a one-step-ahead forecast
  new_xreg <- cbind(current_HDD45t@.Data[train_length + i + 2],
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
                    current_DSun@.Data[train_length + i + 2]
  )
  
  new_xreg_matrix[i, ] <- new_xreg
  
  forecast_result <- predict(model, n.head = 1, newxreg=new_xreg)
  
  # Store the forecast in the forecasts vector
  forecasts[i] <- forecast_result$pred
  
  results_ARMA$Observed[i] <- windows.validation[i]
  results_ARMA$Predicted[i] <- forecast_result$pred
  results_ARMA$Lower[i] <- 
    forecast_result$pred - 1.96 * forecast_result$se
  results_ARMA$Upper[i] <- 
    forecast_result$pred + 1.96 * forecast_result$se
  
}

# Record the end time 
end_time <- Sys.time()

end_time - start_time


################ For cost analysis ##############################


avg_winter_obs <- mean(obsv_wint_ARMA)
avg_spring_obs <- mean(obsv_spri_ARMA)
avg_summer_obs <- mean(obsv_summ_ARMA)
avg_fall_obs <- mean(obsv_fall_ARMA)

# Create a data frame with the average values for each season
obs_seasonal_averages <- data.frame(
  Season = c("Winter", "Spring", "Summer", "Fall"),
  Average_Observations = c(avg_winter_obs, 
                           avg_spring_obs, 
                           avg_summer_obs, 
                           avg_fall_obs)
)

# Print the table
print(obs_seasonal_averages)

######## getting MAPE and coverage by season and plotting PI #######

### function to calculate coverage, MAPE and PI for by season ###

PI_Season_Function <- function(observed, 
                               predicted, 
                               lower_bound, 
                               upper_bound, length){
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
  colnames(results) <- c("observed", 
                         "predicted", 
                         "lower_bound", 
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
  coverage_numerator <- sum(
    results$observed >= results$lower_bound & 
      results$observed <= results$upper_bound
  )
  
  coverage <- coverage_numerator / validation_length
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
  
  
}


# ACF of residuals of in smaple  and also outsample
par(mfrow = c(1, 2))
par(mfrow = c(1, 1))
acf(residuals(fit)[-(1:2)])
errors_ARMA <- results_ARMA$Observed - results_ARMA$Predicted
acf(errors_ARMA)

#results_ARMA <- results

# Save the results_ARMA data frame to a file
save(results_ARMA, file = "results_ARMA.RData")

# Load the results_ARMA data frame from the file
load("results_ARMA.RData")

global_MAPE_ARMA <- mape(results_ARMA$Observed, 
                         results_ARMA$Predicted)
global_coverage_ARMA <- coverage(results_ARMA$Observed, 
                                 results_ARMA$Lower, 
                                 results_ARMA$Upper)

Observed <- windows.validation$TS.1

observed = results_ARMA$Observed
predicted = results_ARMA$Predicted
lower_bound= results_ARMA$Lower
upper_bound = results_ARMA$Upper
length = length(results_ARMA$Observed)
#Calling thefunction
PI_Season_Function(observed, 
                   predicted, 
                   lower_bound, 
                   upper_bound, 
                   length)


#################### Run on test set ###############################



combine_train_test <- function(ts_data, train_indices, test_indices) {
  # Extract train and test windows using indices
  windows.train <- window(ts_data, 
                          start = time(ts_data)[train_indices[1]], 
                          end = time(ts_data)[train_indices[2]])
  
  windows.test <- window(ts_data, 
                         start = time(ts_data)[test_indices[1]], 
                         end = time(ts_data)[test_indices[2]])
  
  # Combine train and test windows
  combined_data <- c(windows.train, windows.test)
  
  # Get the time indices for the combined data
  combined_time <- c(time(windows.train), time(windows.test))
  
  # New TS with the combined data and proper start and end time
  train_and_test_ts <- timeSeries(combined_data, 
                                  charvec = combined_time)
  
  return(train_and_test_ts)
}

train_indices <- c(1, 1826)
test_indices <- c(2558, 2922)

train_and_test_ts <- combine_train_test(Yt, 
                                        train_indices, 
                                        test_indices)


#test to see if merged properly
train_and_test_ts[1826]
Yt[1826]

train_and_test_ts[1827]
Yt[2558]

train_and_test_ts[2191]
Yt[2922]

HDDt_train_test <- combine_train_test(HDD45t, 
                                      train_indices, 
                                      test_indices)

CDDt_train_test <- combine_train_test(CDD72t, 
                                      train_indices, 
                                      test_indices)

WNt_train_test <- combine_train_test(WNt, 
                                     train_indices, 
                                     test_indices)

Holidayt_train_test <- combine_train_test(Holidayt, 
                                          train_indices, 
                                          test_indices)

priort_train_test <- combine_train_test(priort, 
                                        train_indices, 
                                        test_indices)

Nextt_train_test <- combine_train_test(Nextt, 
                                       train_indices, 
                                       test_indices)

DMont_train_test <- combine_train_test(DMont, 
                                       train_indices, 
                                       test_indices)

DTuet_train_test <- combine_train_test(DTuet, 
                                       train_indices, 
                                       test_indices)

DWedt_train_test <- combine_train_test(DWedt, 
                                       train_indices, 
                                       test_indices)

DThut_train_test <- combine_train_test(DThut, 
                                       train_indices, 
                                       test_indices)

DSatt_train_test <- combine_train_test(DSatt, 
                                       train_indices, 
                                       test_indices)

DSunt_train_test <- combine_train_test(DSunt, 
                                       train_indices, 
                                       test_indices)

########################################################
train_length <- length(windows.train)-2
test_length <- length(windows.test)

# empty df to store the observed , forecasts, and prediction intervals
results_ARMA_test <- data.frame(Observed = numeric(test_length), 
                                Predicted = numeric(test_length), 
                                Lower = numeric(test_length), 
                                Upper = numeric(test_length))

# Add these lines before the loop
new_xreg_matrix <- matrix(NA, nrow = n, ncol = 16)

# Create an empty vector to store the one-step-ahead forecasts
forecasts_test <- NULL

# Record the start time
start_time <- Sys.time()

# Loop through the validation set
for (i in 1:test_length) {
  # Combine the train set with the current obsv from the valid set
  current_data <- c(windows.train, windows.test[1:i-1])
  
  # Combine the explanatory variables as well
  current_HDD45t <- rbind(window(HDDt_train_test, 
                  start = time(HDDt_train_test)[1], 
                  end = time(HDDt_train_test)[train_length + i + 2]))
  
  current_HDD45t_lag1 <- rbind(window(lag(HDDt_train_test,1), 
                  start = time(HDDt_train_test)[1], 
                  end = time(HDDt_train_test)[train_length + i + 2]))
  
  current_HDD45t_lag2 <- rbind(window(lag(HDDt_train_test,2), 
                  start = time(HDDt_train_test)[1], 
                  end = time(HDDt_train_test)[train_length + i + 2]))
  
  current_CDD72t <- rbind(window(CDDt_train_test, 
                  start = time(CDDt_train_test)[1], 
                  end = time(CDDt_train_test)[train_length + i+ 2]))
  
  current_CDD72t_lag1 <- rbind(window(lag(CDDt_train_test,1), 
                  start = time(CDDt_train_test)[1], 
                  end = time(CDDt_train_test)[train_length + i+ 2]))
  
  current_CDD72t_lag2 <- rbind(window(lag(CDDt_train_test,2), 
                  start = time(CDDt_train_test)[1], 
                  end = time(CDDt_train_test)[train_length + i+ 2]))
  
  current_WNt <- rbind(window(WNt_train_test, 
                  start = time(WNt_train_test)[1], 
                  end = time(WNt_train_test)[train_length + i+ 2]))
  
  current_Holidayt <- rbind(window(Holidayt_train_test, 
                start = time(Holidayt_train_test)[1], 
                end = time(Holidayt_train_test)[train_length + i+ 2]))
  
  current_Priort <- rbind(window(priort_train_test, 
                  start = time(priort_train_test)[1], 
                  end = time(priort_train_test)[train_length + i+ 2]))
  
  current_Nextt <- rbind(window(Nextt_train_test, 
                  start = time(Nextt_train_test)[1], 
                  end = time(Nextt_train_test)[train_length + i+ 2]))
  
  current_DMon <- rbind(window(lag(DMont_train_test,2), 
                  start = time(DMont_train_test)[1], 
                  end = time(DMont_train_test)[train_length + i+ 2]))
  
  current_DTue <- rbind(window(lag(DTuet_train_test,2), 
                  start = time(DTuet_train_test)[1], 
                  end = time(DTuet_train_test)[train_length + i+ 2]))
  
  current_DWed <- rbind(window(lag(DWedt_train_test,2), 
                  start = time(DWedt_train_test)[1], 
                  end = time(DWedt_train_test)[train_length + i+ 2]))
  
  current_DThu <- rbind(window(lag(DThut_train_test,2), 
                  start = time(DThut_train_test)[1], 
                  end = time(DThut_train_test)[train_length + i+ 2]))
  
  current_DSat <- rbind(window(lag(DSatt_train_test,2), 
                  start = time(DSatt_train_test)[1], 
                  end = time(DSatt_train_test)[train_length + i+ 2]))
  
  current_DSun <- rbind(window(lag(DSunt_train_test,2), 
                  start = time(DSunt_train_test)[1], 
                  end = time(DSunt_train_test)[train_length + i+ 2]))
  
  
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
    current_DSun@.Data[-length(current_DSun@.Data)])
  
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
                         current_DSun@.Data)                       
  
  
  
  
  # Fit the linear model with ARMA errors using auto.arima
  model <- auto.arima(current_data, xreg = xreg_matrix)
  
  # Make a one-step-ahead forecast
  new_xreg <- cbind(current_HDD45t@.Data[train_length + i + 2],
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
                    current_DSun@.Data[train_length + i + 2]
  )
  
  new_xreg_matrix[i, ] <- new_xreg
  
  forecast_result_test <- predict(model, n.head = 1, newxreg=new_xreg)
  
  # Store the forecast in the forecasts vector
  forecasts_test[i] <- forecast_result_test$pred
  
  results_ARMA_test$Observed[i] <- windows.test[i]
  results_ARMA_test$Predicted[i] <- forecast_result_test$pred
  results_ARMA_test$Lower[i] <- 
    forecast_result_test$pred - 1.96 * forecast_result_test$se
  results_ARMA_test$Upper[i] <- 
    forecast_result_test$pred + 1.96 * forecast_result_test$se
  
}

# Record the end time 
end_time <- Sys.time()

end_time - start_time


PI_Season_Function(results_ARMA_test$Observed,
                   results_ARMA_test$Predicted,
                   results_ARMA_test$Lower,
                   results_ARMA_test$Upper,
                   length(windows.test))























