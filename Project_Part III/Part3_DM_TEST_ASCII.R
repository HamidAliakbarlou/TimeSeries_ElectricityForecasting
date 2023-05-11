
Observed <- windows.validation$TS.1


split_data <- function(predicted, observed) {
  
  winter_indices <- c(1:91, 367:456)
  spring_indices <- c(92:182, 457:547)
  summer_indices <- c(183:274, 548:639)
  fall_indices <- c(275:366, 640:731)
  
  pred_wint <- predicted[winter_indices]
  obsv_wint <- observed[winter_indices]
  
  pred_spri <- predicted[spring_indices]
  obsv_spri <- observed[spring_indices]
  
  pred_summ <- predicted[summer_indices]
  obsv_summ <- observed[summer_indices]
  
  pred_fall <- predicted[fall_indices]
  obsv_fall <- observed[fall_indices]
  
  output_list <- list(
    winter = list(predicted = pred_wint,
                  observed = obsv_wint),
    spring = list(predicted = pred_spri,
                  observed = obsv_spri),
    summer = list(predicted = pred_summ,
                  observed = obsv_summ),
    fall = list(predicted = pred_fall,
                observed = obsv_fall)
  )
  
  return(output_list)
}

dm_by_season_df <- function(pred1, pred2, observed) {
  
  # Split the data by season
  mod1_splits <- split_data(pred1, observed)
  mod2_splits <- split_data(pred2, observed)
  
  # Run the Diebold-Mariano test for each season
  dm_results <- list()
  for (season in c("winter", "spring", "summer", "fall")) {
    # Get the predicted and observed values for the current season
    mod1_pred <- mod1_splits[[season]]$predicted
    mod1_obsv <- mod1_splits[[season]]$observed
    mod2_pred <- mod2_splits[[season]]$predicted
    mod2_obsv <- mod2_splits[[season]]$observed
    
    # Calculate the errors for each model
    mod1_errors <- mod1_pred - mod1_obsv
    mod2_errors <- mod2_pred - mod2_obsv
    
    # Run the Diebold-Mariano test
    dm_results[[season]] <- dm.test(mod1_errors, 
                                    mod2_errors, 
                                    alternative = "two.sided", 
                                    h = 1)
  }
  
  # Extract the test statistic and p-value combine into a data frame
  dm_df <- data.frame(season = c("winter", 
                                 "spring", 
                                 "summer", 
                                 "fall"),
                      test_statistic = sapply(dm_results, 
                                            function(x) x$statistic),
                      p_value = sapply(dm_results, 
                                       function(x) x$p.value))
  
  return(dm_df)
}

################################################################
rw_ARMA_compare_season <- dm_by_season_df(forecast_rw,
                                          results_ARMA$Predicted, 
                                          Observed)

#              season    test_statistic    p_value
# winter.DM    winter    7.623685          1.366577e-12
# spring.DM    spring    6.516458          6.919731e-10
# summer.DM    summer    8.620641          3.096435e-15
# fall.DM      fall      8.019615          1.224986e-13

#comparing RW to TBATS all seasons
#Result: p-value <0.05 for all seasons
# Conclusion: reject H0 for all seasons
# enough evidence to conclude that the forecast accuracy of 
#the two models are significantly different for all seasons




tb_ARMA_compare_season <- dm_by_season_df(predicted_tb,
                                          results_ARMA$Predicted, 
                                          Observed)

#              season    test_statistic    p_value
# winter.DM    winter    4.381468          1.998414e-05
# spring.DM    spring    4.839559          2.776605e-06
# summer.DM    summer    8.223156          3.570612e-14
# fall.DM      fall      1.687730          9.316670e-02

#comparing TBATS to regression with ARMA errors all seasons
#Result: p-value <0.05 for all seasons
# Conclusion: reject H0 for all seasons
# enough evidence to conclude that the forecast accuracy of 
#the two models are significantly different for all seasons


ARMA_ARX_compare_season <- dm_by_season_df(results_ARMA$Predicted,
                                           results_ARX.1$predicted,
                                           Observed)

#              season    test_statistic    p_value
# winter.DM    winter    -5.971972         1.223528e-08
# spring.DM    spring    -5.214681         4.996594e-07
# summer.DM    summer    -8.481275         7.337244e-15
# fall.DM      fall      -2.509767         1.294872e-02

#comparing regression with ARMA errors to ARX(1) all seasons
#Result: p-value <0.05 for all seasons
# Conclusion: reject H0 for all seasons
# enough evidence to conclude that the forecast accuracy of 
#the two models are significantly different for all seasons


ARMA_SARIMA_compare_season <- dm_by_season_df(results_ARMA$Predicted,
                                              results_df$predicted,
                                              Observed)

#              season    test_statistic    p_value
# winter.DM    winter    -4.128015         5.590279e-05
# spring.DM    spring    -5.379643         2.286800e-07
# summer.DM    summer    -8.484339         7.199840e-15
# fall.DM      fall      -1.657481         9.913563e-02

#comparing regression with ARMA errors to SARIMA all seasons
#Result: p-value <0.05 for all seasons
# Conclusion: reject H0 for all seasons
# enough evidence to conclude that the forecast accuracy of 
#the two models are significantly different for all seasons


# Combine results into a data frame
all_results <- rbind(rw_ARMA_compare_season, 
                     tb_ARMA_compare_season, 
                     ARMA_ARX_compare_season, 
                     ARMA_SARIMA_compare_season)

# View the combined results
all_results


















