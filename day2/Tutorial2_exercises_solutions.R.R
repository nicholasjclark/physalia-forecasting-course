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


# Solution: try a penalty on the 1st derivative
model0 <- mvgam(count ~ s(time, bs = 'bs', k = 15,
                          m = c(1)) + 
                  ndvi_lag12 + 
                  mintemp,
                family = poisson(),
                data = data_train,
                newdata = data_test)
plot(forecast(model0))
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'link')
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'response')

# Now try penalizing the 1st, 2nd and 3rd derivatives simultaneously
model0 <- mvgam(count ~ s(time, bs = 'bs', k = 15,
                          m = c(3,2,1)) + 
                  ndvi_lag12 + 
                  mintemp,
                family = poisson(),
                data = data_train,
                newdata = data_test)
plot(forecast(model0))
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'link')
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'response')


# 2. Try using a cubic regression spline in place of the b-spline and 
#   inspect the resulting predictions (use `bs = 'cr'`). 

# Solution
model0 <- mvgam(count ~ s(time, bs = 'cr', k = 15) + 
                  ndvi_lag12 + 
                  mintemp,
                family = poisson(),
                data = data_train,
                newdata = data_test)
plot(forecast(model0))
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'link')
plot_predictions(model0, by = 'time',
                 newdata = datagrid(time = 1:(max(data_test$time))),
                 type = 'response')


#### Residual correlation structure exercises ####
# 1. Compare estimates for the latent residual error terms from 
#    model2 and model3. In `mvgam`, this parameter is called `sigma[1]`, 
#    while in `brms`, it is called `sderr`

# Solution
sigma_ests <- rbind(data.frame(
  sigma = as.data.frame(model3, variable = 'sigma[1]')$`sigma[1]`,
  model = 'mvgam'),
  data.frame(
    sigma = as.data.frame(model2, variable = 'sderr')$`sderr`,
    model = 'brms'))
ggplot(data = sigma_ests, aes(x = sigma, fill = model)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 40, col = 'white') +
  facet_grid(rows = 'model') +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c('darkblue', 'darkred')) +
  labs(x = expression(sigma[error]), y = 'Frequency')


# 2. Compare estimates for the parametric effect of minimum temperature 
#   from model2 and model3. In `mvgam`, this parameter is 
#   called `mintemp`, while in `brms`, it is called `b_mintemp`

# Solution
beta_ests <- rbind(data.frame(
  beta = as.data.frame(model3, variable = 'mintemp')$`mintemp`,
  model = 'mvgam'),
  data.frame(
    beta = as.data.frame(model2, variable = 'b_mintemp')$`b_mintemp`,
    model = 'brms'))
ggplot(data = beta_ests, aes(x = beta, fill = model)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 40, col = 'white') +
  facet_grid(rows = 'model') +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c('darkblue', 'darkred')) +
  labs(x = expression(beta[mintemp]), y = 'Frequency')


# 3. Look at the Dunn-Smyth residuals for model3 and provide a few 
#    comments describing what you see (use `plot.mvgam()` with `type = residuals`). 
#    Does this model seem to capture the relevant features of the autocorrelated 
#    observed data?

# Solution
plot(model3, type = 'residuals')

# There don't appear to be any remaining patterns in the residuals, at least with
# respect to time or to the fitted values


# 4. Inspect posterior hindcasts and forecasts from model3 using the steps 
#    we carried out in Tutorial 1

# Solution
hc <- hindcast(model3)
plot(hc)

fc <- forecast(model3) # Error! You probably got an error too, sorry about that :)

# This model was fit before a recent upgrade to mvgam that unfortunately causes
# backward compatibility issues; easy to fix though
model3$obs_data$index..time..index <- model3$obs_data$time
model3$test_data$index..time..index <- model3$test_data$time

# Should now work
fc <- forecast(model3)
plot(fc)


#### Gaussian Process trend exercises ####
# 1. Fit a model that uses a spline of time (using `bs = 'bs'`) and 
#    a Negative Binomial family in `mvgam` for comparisons. Plot the 
#    1st derivative of this temporal spline and describe how (or if) it
#    differs from that of model5

# Solution
model5b <- mvgam(count ~ 
                  s(ndvi_lag12, k = 9) +
                  mintemp +
                  s(time, k = 30, bs = 'bs', m = c(3,2)),
                family = nb(),
                data = data_train,
                newdata = data_test,
                priors = prior(normal(0, 2), class = b))
plot_slopes(model5b, variable = 'time', condition = 'time', type = 'link') +
  labs(y = 'First derivative (slope) of linear predictor') +
  geom_hline(yintercept = 0, linetype = 'dashed')

# This spline of time (which used a combo of 3rd and 2nd derivative penalties)
# is less smooth than the corresponding Gaussian Process from model5. It also has 
# wider uncertainties, particularly near the boundaries


# 2. Plot extrapolations from the spline model and take a few notes describing 
#    how these differ from the GP predictions in model5

# Solution
plot_predictions(model5b, by = 'time',
                 newdata = datagrid(time = 1:max(data_test$time)),
                 type = 'link') +
  geom_vline(xintercept = max(data_train$time), 
             linetype = 'dashed')

# These extrapolations are MUCH worse than those from the GP. They tend to increase
# indefinitely, leading to explosive forecasts
plot(forecast(model5b))


# 3. Compare the in-sample predictive accuracies of the two models using 
#    `loo_compare()`. Is either model favoured over the other?
?loo_compare.mvgam

# Solution (have to refit model5 as it wasn't provided)
model5 <- mvgam(count ~ 
                  s(ndvi_lag12, k = 9) +
                  mintemp +
                  gp(time, c = 5/4, k = 20, scale = FALSE),
                family = nb(),
                data = data_train,
                newdata = data_test,
                priors = prior(normal(0, 2), class = b))
loo_compare(model5, model5b)
#          elpd_diff se_diff
# model5b  0.0        0.0   
# model5  -3.7        2.2 

# The spline model is slightly favoured (higher elpd is better), suggesting it fits the 
# in-sample data slightly better than the GP model. A very good example of the dangers 
# of relying only on in-sample fit metrics (i.e. AIC, WAIC, DIC, BIC, LOO etc...) to 
# evaluate models, particularly if forecasting is one of your goals. Comparing a proper
# score metric of each model's forecasts (the CRPS, more on this in Lecture 4) clearly
# shows that the GP is strongy favoured (lower CRPS is better)
sum(score(forecast(model5),
          score = 'crps')$PP$score, 
    na.rm = TRUE)
sum(score(forecast(model5b),
          score = 'crps')$PP$score, 
    na.rm = TRUE)

# 4. Consider watching the below lecture by Richard McElreath
#    on Gaussian Processes and their wide uses in statistical modelling:
#    https://www.youtube.com/watch?v=Y2ZLt4iOrXU&t=1s

# Solution
# 😊🎥🍿