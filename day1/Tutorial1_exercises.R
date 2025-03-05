#### "Ecological forecasting in R" Tutorial 1 exercises ####
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
load_tutorial_files(filepath = 'Tutorial1_exercise_data.zip')

#### Load libraries ####
# You may first need to run: 
# remotes::install_github('nicholasjclark/mvgam', force = TRUE)
library(dplyr)
library(mvgam) 
library(gratia)
library(ggplot2)

#### Data manipulation exercises ####
#1. Plot the distributions of the `count`, `mintemp` and `ndvi_ma12` variables as 
#   histograms to get a better sense of the types of values they can take. 
#   See `?hist` for guidance
?hist


#2. Calculate what proportion of observations in `count` are zeros. 
#   See `?which` and `?==` for guidance
?which
?`==`


#### Visualization exercises ####
# 1. Calculate cross-correlations between our outcome variable `count` 
#   and `mintemp` at lags of 1 - 15 months. This procedure is often used 
#   to help identify lags of the x-variable that might be useful predictors 
#   of the outcome. Note, you will need to use `na.action = na.pass` 
#   so that the missing values in `count` don't cause problems. 
#   See `?ccf` for details about calculating cross-correlations using 
#   `type = 'correlation'`. Hint, use the following template code and 
#   fill in the "?" with the correct variable / value
?ccf
ccf(x = '?',
    y = '?',
    na.action = '?', 
    lag.max = '?',
    # compute correlations at each lag
    type = 'correlation',
    # not necessary but makes for a prettier plot
    bty = 'l',
    lwd = 2,
    ci.col = 'darkred',
    main = expression(paste(italic(Cor),"(",ndvi[lag],",",count, ")")))


#   Which lags of `mintemp` would you consider to be the most likely candidates for 
#   predicting our outcome `count`?


# 2. Plot an `STL` decomposition for the `mintemp` variable
?stl


# 3. How might you decompose a time series using a GAM? Take a few notes 
#   to explain what steps you would take (hint, look for inspiration from this
#   nice blog post by Gavin Simpson on additive time series models with GAMs:
#   https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/


#### Temporal random effect exercises ####
# 1. Inspect the residual plot and make some notes about any features that 
#   stand out to you. Do these diagnostics look reasonable to you? How might 
#   you rectify these issues by modifying the model?
plot(model1, type = 'residuals')

# 2. The model contains posterior draws of Dunn-Smyth residuals that can be 
#    used for further investigations. For each series in the data (only one 
#    series in this case), residuals are in a matrix of dimension n_draws 
#    x n_timepoints. Calculate posterior median residuals per time point and 
#    plot these (hint, use ?pp_check for guidance). 
#    fill in the "?" with the correct variable / value
pp_check(model1, 
         type = 'resid_ribbon',
         x = '?',
         ndraws = 200)

#   Do these residuals look worrisome to you?


#### Fixed effect predictor exercises ####
# 1. Make a scatterplot comparing `ndvi` and our outcome `count`
?graphics::plot


# 2. Add `ndvi_ma12` as a second predictor. Be sure to first check 
#   whether there is a strong pairwise correlation between `mintemp` and 
#   `ndvi_ma12` (hint, use `?cor.test` for guidance)
?cor.test


# 3. Does the model estimate any strong effect of `ndvi_ma12` on 
#   `log(counts)`? Or does inclusion of `ndvi_ma12` lead to different 
#   inference on the effect of `mintemp` compared to when `ndvi_ma12` 
#   was not in the model?


#### Nonlinear temporal functions exercises ####
# 1. Change the `ndvi_ma12` effect to a smooth function of `ndvi_12` using the `s()` 
#   argument (hint, see `?s` for guidance on how to define smooth terms in 
#   GAM formulae using `mgcv` syntax). Inspect the resulting function 
#   estimates for both `ndvi` and `time`
?mgcv::s
?mgcv::smooth.terms

#    Inspect the resulting function estimates for both `ndvi_ma12` and `time`, 
#    and consider using some of the strategies outlined in this blog post on
#    interpreting nonlinear effects in GAMs with mgcv and marginaleffects
#    https://ecogambler.netlify.app/blog/interpreting-gams/ to understand 
#    the smooths in more depth



# 2. Try increasing the complexity of the temporal smooth by changing 
#   `k` to a larger number, something like 30. Do you get any sense that 
#   there are problems with fitting this model? Do any of your 
#   conclusions change?


# 3. Inspect model residuals to look for any remaining un-modelled 
#   temporal patterns. What other steps might we take to address these?


# 4. Look over a nice blog post by Gavin Simpson on strategies for 
#   fitting nonlinear interactive effects with GAMs to model changing 
#   seasonal patterns in time series:
#   https://fromthebottomoftheheap.net/2015/11/21/climate-change-and-spline-interactions/ 
#   in particular, this post begins addressing the issue of autocorrelated errors, 
#   which will be a prominent feature of our next tutorial
