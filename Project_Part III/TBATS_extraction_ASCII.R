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
                                      seasonal.periods = c(7,365.25)), 
                                      use.parallel = T, num.cores=2)
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


#####################################################################

# validation from 2020-2021
windows.validation <- window(Yt, start=time(Yt)[1827]
                             , end=time(Yt)[2557])
Observed <- windows.validation$TS.1


# empty vectors to store lower and upper bounds for each forecast
lw_bd_95_tbats <- numeric(length(model_tbats))
up_bd_95_tbats <- numeric(length(model_tbats))

# Loop through each forecast and extract the lower and upper bounds
for (i in 1:length(model_tbats)) {
  lw_bd_95_tbats[i] <- model_tbats[[i]]$lower[2]
  up_bd_95_tbats[i] <- model_tbats[[i]]$upper[2]
}

# Print the extracted values
cat("Lower bounds of 95% prediction intervals:\n")
print(lower_bounds)
cat("\nUpper bounds of 95% prediction intervals:\n")
print(upper_bounds)


pred_values_tbats <- sapply(model_tbats_S2F7_exp.fcast, function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(x$mean[1])
  }
})

observed_tb = results_ARMA$Observed
predicted_tb = pred_values_tbats
lower_bound_tb= lw_bd_95_tbats
upper_bound_tb = up_bd_95_tbats
length(observed_tb)
# Call the function
PI_Season_Function(observed_tb, 
                   predicted_tb, 
                   lower_bound_tb, 
                   upper_bound_tb, 
                   731)

















