#### "Ecological forecasting in R" Tutorial 4 exercises ####
# Written by Nicholas Clark; https://github.com/nicholasjclark

#### Load all data and modelling objects needed to complete exercises ####
# This function will load all data objects (as RDS files) from the 
# Tutorial1_exercise_data.zip directory. This assumes you have downloaded the 
# data directory and it is stored within your current working directory
load_tutorial_files = function(filepath){
  files = unzip(zipfile = filepath, list = TRUE)$Name[-1]
  
  alldat <- lapply(files, function (x){
    con <- unz(filepath, filename = x)
    con2 <- gzcon(con)
    readRDS(con2)
  })
  
  names(alldat) <- unlist(lapply(files, function(x){
    gsub('html/', '', strsplit(x, "_(?!.*_)", perl=TRUE)[[1]][1])
  }))
  
  rm(files)
  
  list2env(alldat, envir = globalenv())
}
load_tutorial_files(filepath = 'Tutorial4_exercise_data.zip')

#### Load libraries ####
# You may first need to run: 
# remotes::install_github('nicholasjclark/mvgam', force = TRUE)
library(dplyr)
library(mvgam) 
library(tidybayes)
library(bayesplot)
library(gratia)
library(ggplot2)
library(marginaleffects)

#### Manipulating for modeling exercises ####
# 1. Calculate the number of timepoints in the training data 
#    that have non-missing observations for all eight time series. 

# Solution
plankton_train %>%
  dplyr::filter(!is.na(y)) %>%
  dplyr::group_by(series) %>%
  dplyr::tally()


#### Multivariate model exercises ####
# 1. Plot conditional effects of month and temperature for each 
#    algal group. Hint, see the documentation in 
#    ?marginaleffects::plot_predictions and use the following template 
#    code for guidance (replace the ? with the correct value(s))

#    You can use 'plot_predictions' to generate conditional effects plots
#    that are stratified over a number of variables (up to three at once).
#    This will feed a particular grid of 'newdata' to the 'predict.mvgam' 
#    function, returning conditional predictions on the response scale
?marginaleffects::plot_predictions
plot_predictions(df_mod,
                 condition = c('?', '?', '?'),
                 conf_level = 0.8)

# Solution
plot_predictions(df_mod,
                 condition = c('month', 'temp', 'series'),
                 conf_level = 0.8)

# Or
plot_predictions(df_mod,
                 condition = c('temp', 'month', 'series'),
                 conf_level = 0.8)


# 2. Compare in-sample fits from the two models (`var_mod` and `df_mod`) 
#    using `loo_compare()`. Does this comparison agree with the forecast 
#    comparison above? Why might they differ?
?mvgam::loo_compare.mvgam


# Solution
# Have to refit var_mod as it was too big to include in the supplied objects
priors <- c(prior(normal(0.5, 0.1), class = sigma_obs, lb = 0.2),
            prior(normal(0.5, 0.25), class = sigma))
var_mod <- mvgam(  
  # observation formula, which is empty
  y ~ -1,
  
  # process model formula, which includes the smooth functions
  trend_formula = ~ te(temp, month, k = c(4, 4)) +
    te(temp, month, k = c(4, 4), by = trend),
  
  # VAR1 model with uncorrelated process errors
  trend_model = 'VAR1',
  family = gaussian(),
  data = plankton_train,
  newdata = plankton_test,
  
  # include the updated priors
  priors = priors)

loo_compare(var_mod, df_mod)
# df_mod     0.0       0.0 
# var_mod -531.5     149.9 

# Dynamic factor model is preferred for in-sample fits. This also agrees 
# with the findings from the out-of-sample forecast comparisons. The two models
# make different assumptions about what is going on:
# a) the dynamic factor assumes that the series are co-responding to some 
#    hidden, unmeasured variables (i.e. environmental changes in the water maybe,
#    or changes in nutrient availability)
# b) the VAR model assumes that the series' dynamics are controlled by their
#    own lagged interactions


# 3. Fit a second dynamic factor model that uses Gaussian Process factors 
#    in place of AR1 factors. Compare forecasts from this model to the AR1 
#    factor model using the energy and variogram scores. Which model is 
#    preferred?

# Solution
# First update the number of samples taken for the original DF model
# so we can get more fair comparisons
df_mod <- update(df_mod, burnin = 500, samples = 500)

# Now fit the GP dynamic factor model
priors <- prior(normal(0.5, 0.1), class = sigma_obs, lb = 0.2)
df_mod_gp <- mvgam(  
  y ~ te(temp, month, k = c(4, 4)) +
    te(temp, month, k = c(4, 4), by = series),
  trend_model = GP(),
  use_lv = TRUE,
  n_lv = 3,
  family = gaussian(),
  data = plankton_train,
  newdata = plankton_test,
  priors = priors)

# create forecast objects for each model
fcdf <- forecast(df_mod)
fcdf_gp <- forecast(df_mod_gp)

# plot the difference in variogram scores; 
# a negative value means the AR1 DF model is better, 
# while a positive value means the GP DF model is better
diff_scores <- score(fcdf, score = 'variogram')$all_series$score -
  score(fcdf_gp, score = 'variogram')$all_series$score
plot(diff_scores, pch = 16, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(variogram[DFAR]~-~variogram[DFGP]))
abline(h = 0, lty = 'dashed')

# The AR1 dynamic factor model does better in this case

# plot the difference in energy scores; 
# interpretation is the same as above
diff_scores <- score(fcdf, score = 'energy')$all_series$score -
  score(fcdf_gp, score = 'energy')$all_series$score
plot(diff_scores, pch = 16, col = 'darkred', 
     ylim = c(-1*max(abs(diff_scores), na.rm = TRUE),
              max(abs(diff_scores), na.rm = TRUE)),
     bty = 'l',
     xlab = 'Forecast horizon',
     ylab = expression(energy[DFAR]~-~energy[DFGP]))
abline(h = 0, lty = 'dashed')

# The AR1 dynamic factor model also does better in this case

# Plotting a few forecasts shows that the GP model tends is
# particularly worse for series 2 (Bluegreens) and series 3
# (Diatoms)
layout(matrix(1:2, ncol = 1))
for(i in 1:5){
  plot(fcdf, series = i)
  title('AR1 DF model')
  plot(fcdf_gp, series = i)
  title('GP DF model')
}
layout(1)
