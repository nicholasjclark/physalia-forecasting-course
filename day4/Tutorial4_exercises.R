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


# 2. Compare in-sample fits from the two models (`var_mod` and `df_mod`) 
#    using `loo_compare()`. Does this comparison agree with the forecast 
#    comparison above? Why might they differ?
?mvgam::loo_compare.mvgam


# 3. Fit a second dynamic factor model that uses Gaussian Process factors 
#    in place of AR1 factors. Compare forecasts from this model to the AR1 
#    factor model using the energy and variogram scores. Which model is 
#    preferred?