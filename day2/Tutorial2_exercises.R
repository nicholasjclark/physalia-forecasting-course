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
# 1. Using advice from Gavin Simpson's blog post, try changing the order 
#   of the penalty in the temporal smooth to see how the resulting predictions 
#   change. Hint, use the following template code and replace the ? with 
#   the correct value(s)
#   Using ?b.spline you will see examples in the Description and Details 
#   sections about how these splines can handle multiple penalties
#   https://fromthebottomoftheheap.net/2020/06/03/extrapolating-with-gams/
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
# 1. Compare estimates for the latent residual error terms from 
#    model2 and model3. In `mvgam`, this parameter is called `sigma[1]`, 
#    while in `brms`, it is called `sderr`


# 2. Compare estimates for the parametric effect of minimum temperature 
#   from model2 and model3. In `mvgam`, this parameter is 
#   called `mintemp`, while in `brms`, it is called `b_mintemp`


# 3. Look at the Dunn-Smyth residuals for model3 and provide a few 
#    comments describing what you see (use `plot.mvgam()` with `type = residuals`). 
#    Does this model seem to capture the relevant features of the autocorrelated 
#    observed data?


# 4. Inspect posterior hindcasts and forecasts from model3 using the steps 
#    we carried out in Tutorial 1


#### Gaussian Process trend exercises ####
# 1. Fit a model that uses a spline of time (using `bs = 'bs'`) and 
#    a Negative Binomial family in `mvgam` for comparisons. Plot the 
#    1st derivative of this temporal spline and describe how (or if) it
#    differs from that of model5


# 2. Plot extrapolations from the spline model and take a few notes describing 
#    how these differ from the GP predictions in model5


# 3. Compare the in-sample predictive accuracies of the two models using 
#    `loo_compare()`. Is either model favoured over the other?
?loo_compare.mvgam

# 4. Consider watching the below lecture by Richard McElreath
#    on Gaussian Processes and their wide uses in statistical modelling:
#    https://www.youtube.com/watch?v=Y2ZLt4iOrXU&t=1s

