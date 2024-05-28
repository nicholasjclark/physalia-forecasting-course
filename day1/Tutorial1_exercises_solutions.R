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
library(marginaleffects)
library(zoo)
library(viridis)

#### Data manipulation exercises ####
#1. Plot the distributions of the `count`, `mintemp` and `ndvi` variables as 
#   histograms to get a better sense of the types of values they can take. 
#   See `?hist` for guidance
?hist

# Solution
hist(model_data$count)
hist(model_data$mintemp)
hist(model_data$ndvi)

# Or using ggplot2
ggplot(model_data, aes(x = count)) +
  geom_histogram(aes(y = stat(density)), col = 'white')
ggplot(model_data, aes(x = mintemp)) +
  geom_histogram(aes(y = stat(density)), col = 'white')
ggplot(model_data, aes(x = ndvi)) +
  geom_histogram(aes(y = stat(density)), col = 'white')

#2. Calculate what proportion of observations in `count` are zeros. 
#   See `?which` and `?==` for guidance
?which
?`==`

# Solution
length(which(model_data$count == 0)) / NROW(model_data)

# Or using dplyr
model_data %>%
  dplyr::select(count) %>%
  dplyr::summarise(prop_zero = length(which(count == 0)) / dplyr::n())



#### Visualization exercises ####
# 1. Calculate cross-correlations between our outcome variable `count` 
#   and `ndvi` at lags of 1 - 15 months. This procedure is often used 
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


#   Which lags of `ndvi` would you consider to be the most likely candidates for 
#   predicting our outcome `count`?

# Solution
ccf_vals <- ccf(x = model_data$ndvi,
                y = model_data$count,
                na.action = na.pass, 
                lag.max = 15,
                # compute correlations at each lag
                type = 'correlation')
plot(ccf_vals,
     bty = 'l',
     lwd = 2,
     ci.col = 'darkred',
     main = expression(paste(italic(Cor),"(",ndvi[lag],",",count, ")")))

# Correlatios are positive around lags of -10 lunar months, indicating that
# an above average value of NDVI is associated with an above average value of count
# about 10 months later

# 2. Plot an `STL` decomposition for the `ndvi` variable
?stl

# Solution, using a seasonal smoothing window of 11
plot(stl(ts(model_data$ndvi, frequency = 12),
         s.window = 11))

# 3. How might you decompose a time series using a GAM? Take a few notes 
#   to explain what steps you would take (hint, look for inspiration from this
#   nice blog post by Gavin Simpson on additive time series models with GAMs:
#   https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/

# Solution
# Typically we might start with an additive decomposition like:
# gam(y ~ s(season, bs = 'cc') + s(time), ...)

# This forces the seasonal component to be cyclic. 
# We could improve this model slightly by specifying the outer boundaries of the cycle,
# which ensures the smooth joins in the correct place. For monthly date (values in 1:12),
# we might do:
# gam(y ~ s(season, bs = 'cc') + s(time), knots = list(season = c(0.5, 12.5)), ...)

# Expanding up the complexity could be done by considering a multiplicative model
# gam(y ~ te(season, time, bs = c('cc', 'tp)), knots = list(season = c(0.5, 12.5)), ...)



#### Temporal random effect exercises ####
# 1. Inspect the residual plot and make some notes about any features that 
#   stand out to you. Do these diagnostics look reasonable to you? How might 
#   you rectify these issues by modifying the model?
plot(model1, type = 'residuals')

# Solution
model1
# There is clear temporal autocorrelation remaining in the residuals, suggesting
# we might need to include a spline of time or use a latent autoregressive residual process

# 2. The model contains posterior draws of Dunn-Smyth residuals that can 
#   be used for further investigations. For each series in the data 
#   (only one series in this case), residuals are in a matrix of dimension 
#   `n_draws x n_timepoints`. Calculate posterior median residuals per time 
#   point and plot these as a line plot (hint, use `?apply` and either 
#   `?median` or `?quantile` for guidance). Add a dashed horizontal 
#   line at `0` (hint, use `?abline` for guidance). 
#   hint, use the following template code and fill in the "?" 
#   with the correct variable / value
#   extract residuals for our rodent species of interest
model1_resids <- model1$resids$PP

#   check the dimensions of the residuals matrix
dim(model1_resids)

#   calculate posterior median residuals per timepoint
?apply
?median
?quantile
median_resids <- apply(model1_resids,
                       MARGIN = '?', 
                       FUN = '?')

#   plot median residuals over time
plot(median_resids,
     type = 'l',
     # not necessary but makes for a prettier plot
     lwd = 2,
     col = 'darkred',
     bty = 'l',
     ylab = 'Median residual',
     xlab = 'Time')

#   add a horizontal dashed line at zero
?abline
abline('?' = 0, lty = 'dashed')


#   Do these residuals look worrisome to you?

# Solution
median_resids <- apply(model1_resids,
                       MARGIN = 2, 
                       FUN = median)
plot(median_resids,
     type = 'l',
     lwd = 2,
     col = 'darkred',
     bty = 'l',
     ylab = 'Median residual',
     xlab = 'Time')
abline(h = 0, lty = 'dashed')

# These look very worrisome as they show a lot of seasonality and autocorrelation
# that has not been captured



#### Fixed effect predictor exercises ####
# 1. Make a scatterplot comparing `ndvi` and our outcome `count`
?graphics::plot

# Solution
plot(count ~ ndvi, data = model_data, pch = 16)

# Or
ggplot(model_data, aes(x = ndvi, y = count)) +
  geom_point()

# 2. Add `mintemp` as a second predictor. Be sure to first check 
#   whether there is a strong pairwise correlation between `mintemp` and 
#   `ndvi` (hint, use `?cor.test` for guidance)
?cor.test

# 3. Does the model estimate any strong effect of `mintemp` on 
#   `log(counts)`? Or does inclusion of `mintemp` lead to different 
#   inference on the effect of `ndvi` compared to when `mintemp` 
#   was not in the model?

# Solution
cor.test(model_data$ndvi, model_data$mintemp)
model2 <- mvgam(count ~ s(year, bs = 're') + 
                  ndvi + 
                  mintemp - 1,
                # Standard normal priors on all fixed effect coefficients
                priors = prior(normal(0, 1), class = b),
                data = model_data)
conditional_effects(model2, type = 'link')

# Yes the inclusion of mintemp has led to a weaker expected relationship 
# between ndvi and log(count)


#### Nonlinear temporal functions exercises ####
# 1. Change the `ndvi` effect to a smooth function of `ndvi` using the `s()` 
#   argument (hint, see `?s` for guidance on how to define smooth terms in 
#   GAM formulae using `mgcv` syntax). Inspect the resulting function 
#   estimates for both `ndvi` and `time`
?mgcv::s
?mgcv::smooth.terms

#    Inspect the resulting function estimates for both `ndvi` and `time`, 
#    and consider using some of the strategies outlined in this blog post on
#    interpreting nonlinear effects in GAMs with mgcv and marginaleffects
#    https://ecogambler.netlify.app/blog/interpreting-gams/ to understand 
#    the smooths in more depth

# Solution: try updating model3 to include thin plate spline of ndvi with k set to 10
model3b <- mvgam(count ~ s(time, bs = 'bs', k = 15) + 
                  s(ndvi, k = 10) + 
                  mintemp,
                 # Standard normal priors on all fixed effect coefficients
                 priors = prior(normal(0, 1), class = b),
                data = model_data)

# Inspect the conditional ndvi effect, on the link scale
plot_predictions(model3b, condition = 'ndvi', type = 'link')

# Inspect the first derivative (slope) of this effect
plot_slopes(model3b, variable = 'ndvi', condition = 'ndvi', type = 'link') +
  geom_hline(yintercept = 0, linetype = 'dashed')

# Find values in the span of ndvi measurements where the slope is negative, 
# suggesting that ndvi values in this range lead to decreases in expected counts
hypotheses(slopes(model3b, variable = 'ndvi', type = 'link')) %>%
  dplyr::mutate(significant_decline = dplyr::case_when(
    conf.low < 0 & conf.high < 0 ~ 'yes',
    TRUE ~ 'no')) %>%
  ggplot(., aes(x = ndvi, y = estimate, col = significant_decline)) +
  geom_point()

# There are many other quantities we could compute with marginaleffects, see
# here for more guidance: https://marginaleffects.com/


# 2. Try increasing the complexity of the temporal smooth by changing 
#   `k` to a larger number, something like 50. Do you get any sense that 
#   there are problems with fitting this model? Do any of your 
#   conclusions change?

# Solution
model3c <- mvgam(count ~ s(time, bs = 'bs', k = 50) + 
                   s(ndvi, k = 10) + 
                   mintemp,
                 # Standard normal priors on all fixed effect coefficients
                 priors = prior(normal(0, 1), class = b),
                 data = model_data)

# The model converges well, with no major warnings from Stan
summary(model3c)

# The spline of time is exceptionally wiggly now, but overall conclusions don't change
# too much
conditional_effects(model3c)
conditional_effects(model3c, type = 'link')


# 3. Inspect model residuals to look for any remaining un-modelled 
#   temporal patterns. What other steps might we take to address these?


# Solution
plot(model3c, type = 'residuals')

# There is a bit of autocorrelation remaining at a few lags, suggesting this model
# has not completely captured the seasonality in the data. We can look at this further:
model_data %>%
  
  # Extract residual point estimates and plot them against time
  dplyr::bind_cols(resids = residuals(model3c, summary = TRUE)[,1]) %>%
  ggplot(., aes(x = time, y = resids)) +
  geom_point() +
  geom_line()


# 4. Look over a nice blog post by Gavin Simpson on strategies for 
#   fitting nonlinear interactive effects with GAMs to model changing 
#   seasonal patterns in time series:
#   https://fromthebottomoftheheap.net/2015/11/21/climate-change-and-spline-interactions/ 
#   in particular, this post begins addressing the issue of autocorrelated errors, 
#   which will be a prominent feature of our next tutorial
