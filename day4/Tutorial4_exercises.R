#### "Ecological forecasting in R" Tutorial 3 exercises ####
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

#### Manipulating data exercises ####
# 1. Calculate the number of timepoints in the training data 
#    that have non-missing observations for all eight time series. 


#### Modelling tick abundance exercises ####
# 1. Fit each of the tick models using mgcv and interrogate them 
#    (hint, use `method = 'REML` for the best smoothing parameter selection results)
?gam

# 2. If you were selecting among the three mgcv models using approximate 
#    hypothesis tests with a generalized likelihood ratio test 
#    (using `anova.gam`), which model would you end up with? 
#    (hint, use `test = 'Chisq'` to perform model comparisons)
?anova.gam

#### State-Space VAR model exercises ####
# 1. Plot conditional effects of month and temperature for each 
#    algal group. Hint, see the documentation in 
#    ?marginaleffects::plot_predictions and use the following template 
#    code for guidance (replace the ? with the correct value(s))

#    You can use 'plot_predictions' to generate conditional effects plots
#    that are stratified over a number of variables (up to three at once).
#    This will feed a particular grid of 'newdata' to the 'predict.mvgam' 
#    function, returning conditional predictions on the response scale
?marginaleffects::plot_predictions
plot_predictions(varcor_mod,
                 condition = c('?', '?', '?'),
                 conf_level = 0.8)


# 2. Calculate the posterior median process error correlation matrix. 
#    Which two algal groups are the most negatively correlated in their 
#    process errors? Hint, see ?mvgam::as.matrix.mvgam and the 
#    following template code for guidance (replace the ? with the correct value(s))

#    Each posterior draw of Sigma will be a vector of length 25, so you will 
#    need to convert that vector to a matrix. Then you can see `?cov2cor` 
#    for guidance on calculating correlation matrices from a covariance matrix
#    You can use 'as.matrix.mvgam' to extract posterior estimates of 
#    the process error matrix Sigma
?mvgam::as.matrix.mvgam
Sigmas <- as.matrix(varcor_mod, variable = '?', regex = '?')

#    Each draw will be a vector of length 25 (representing the 
#    5x5 covariance matrix); calculate posterior medians for each column
?apply
?median
median_Sigma <- apply(Sigmas, '?', '?')

# Convert to a correlation matrix
?cov2cor
cov2cor(matrix(median_Sigma, nrow = 5, ncol = 5))