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
load_tutorial_files(filepath = 'Tutorial3_exercise_data.zip')

#### Load libraries ####
# You may first need to run: 
# remotes::install_github('nicholasjclark/mvgam', force = TRUE)
library(dplyr)
library(mvgam) 
library(tidybayes)
library(ggplot2)
library(marginaleffects)

#### Posterior simulation exercises ####
# 1. Plot a histogram of model1’s posterior intercept coefficients alpha
#   Hint, use the following template code and replace the ? with 
#   the correct value(s)

# check names of variables that can be extracted
?as.data.frame.mvgam
vars_available <- variables(model1)

# we are interested in the observation model coefficients
vars_available$observation_betas

# extract the intercepts using the as.data.frame.mvgam function
# you will have two options for extracting this variable. first, you
# can use the alias
alphas <- as.data.frame(model1, variable = '?')

# alternatively, use the original name
alphas <- as.data.frame(model1, variable = '?')

# plot a histogram
hist(alphas[,1], xlab = expression(alpha),
     main = '', col = 'darkred', border = 'white')


# 2. Plot a scatterplot of recruits vs log_spawners using the supplies model_data. 
#   Take a few notes on whether you think our primary modelling assumption, 
#   that the number of log(recruits) scales linearly with number of 
#   log_spawners, is justified.


#### Comparing dynamic models exercises ####
# 1. Calculate the Root Mean Squared Error (RMSE) for posterior mean 
#   predictions from the fc object using the formula provided 
#   by Hyndman and Athanasopoulos at https://otexts.com/fpp3/accuracy.html


# 1. Roll the training data one timepoint forward and refit model3 
#   (i.e. split the data on timepoint 37 rather than 36). Compare CRPS 
#   values from this model to the previous refit to see whether the scores 
#   for timepoints 38-43 have changed when conditioning on the extra 
#   information in timepoint 37


# 3. Consider watching the below video by Rob Hyndman on evaluating 
#   distributional forecasts: 
#   https://www.youtube.com/watch?t=1&v=prZH2TyrRYs&source_ve_path=OTY3MTQ&feature=emb_imp_woyt


