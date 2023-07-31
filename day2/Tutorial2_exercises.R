#### "Ecological forecasting in R" Tutorial 2 exercises ####
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
load_tutorial_files(filepath = 'Tutorial2_exercise_data.zip')

#### Load libraries ####
# You may first need to run: 
# remotes::install_github('nicholasjclark/mvgam', force = TRUE)
library(dplyr)
library(mvgam) 
library(gratia)
library(ggplot2)
library(marginaleffects)
library(zoo)
library(viridis)

#### Forecasting with temporal smooths exercises ####
# 1. Using advice from Gavin Simpson's blog post above, try changing the order 
#   of the penalty in the temporal smooth to see how the resulting predictions 
#   change. Hint, use the following template code and replace the ? with 
#   the correct value(s)
#   Using ?b.spline you will see examples in the Description and Details 
#   sections about how these splines can handle multiple penalties
?b.spline
model0 <- mvgam(count ~ s(time, bs = 'bs', k = 15,
                          m = c('?')) + 
                  ndvi_lag12 + 
                  mintemp,
                family = poisson(),
                data = data_train,
                newdata = data_test)


# 2. Try using a cubic regression spline in place of the b-spline and 
#   inspect the resulting predictions (use `bs = 'cr'`). 


#### Residual correlation strcuture exercises ####
# 1. Compare estimates for the latent residual error terms sigma_[error] from 
#   each model. In `mvgam`, this parameter is called `sigma[1]`, while in `brms`, 
#   it is called `sderr`


# 2. Compare estimates for the parametric effect of minimum temperature 
#   ($\beta_{mintemp}$) from each model. In `mvgam`, this parameter is 
#   called `mintemp`, while in `brms`, it is called `b_mintemp`


# 3. Look at the Dunn-Smyth residuals for model 3 and provide a few 
#   comments describing what you see (use `plot.mvgam` with `type = residuals`). 
#   Does this model seem to capture the relevant features of the autocorrelated 
#   observed data?


# 4. Inspect posterior hindcasts and forecasts from model 3 using the steps 
#   we carried out in Tutorial 1


#### Gaussian Process trend exercises ####
# 1. Plot the 1st derivatives for the GP trends from models 5 and 6 and 
#   describe how their forecasts differ (hint, pay attention to how quickly 
#   they converge toward flat functions at the end of the forecast horizon). 
#   Take a few notes describing how these GP predictions differ from the spline 
#   extrapolations from model0 and model0b


# 2. Plot the GP covariance kernel for model 6 and compare its shape to the 
#   kernels we estimated from model 5 above


# 3. Fit an `mvgam` model that uses a GP trend and a Poisson observation 
#   model in place of the Negative Binomial. How do predictions from 
#   this model differ?


#   4. Consider watching the below lecture by Richard McElreath
#   on Gaussian Processes and their wide uses in statistical modelling:
#   https://www.youtube.com/watch?v=Y2ZLt4iOrXU&t=1s

