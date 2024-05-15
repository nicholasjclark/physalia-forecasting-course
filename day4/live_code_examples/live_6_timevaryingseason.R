library(mvgam)
library(forecast)
library(janitor)
library(gratia)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())
library(patchwork)

# Load the AirPassengers dataset and plot the time series
data("AirPassengers")
plot(AirPassengers, bty = 'l', lwd = 1.5,
     col = 'darkred')

# Look at an STL decomposition
stl(AirPassengers, s.window = 9) %>%
  autoplot()

# This plot suggests that the seasonal pattern changes over time,
# not just in magnitude but also perhaps a bit in shape. Convert
# to a data.frame() object that is suitable for mgcv / mvgam modeling
airdat <- series_to_mvgam(AirPassengers, freq = frequency(AirPassengers))
dplyr::glimpse(airdat$data_train)

# Plot features of the series
plot_mvgam_series(data = airdat$data_train,
                  newdata = airdat$data_test)

# Now fit a model. First the traditional way to model time-varying 
# seasonality using a tensor product of 'time' and 'season', 
# where 'season' is modeled with a cyclic spline
mod1 <- mvgam(y ~ te(season, time,
                     bs = c('cc', 'tp'),
                     k = c(6, 15)),
              knots = list(season = c(0.5, 12.5)),
              family = nb(),
              data = airdat$data_train,
              newdata = airdat$data_test)

# Predictions aren't too bad here, but perhaps the uncertainty 
# is a bit too wide
plot(mod1, type = 'forecast')

# We can conveniently use gratia::draw() to view the time * season
# smooth function
draw(mod1$mgcv_model)

# This model is not too bad, but it might not be preferable for a few 
# reasons:
# 1. We are capturing the temporal dimension with a spline, and we all 
#    know that splines generally give poor extrapolation behaviours
# 2. This model assumes that the seasonal pattern changes at the same 
#    rate that the temporal spline changes, and this might not always
#    be the most suitable model

# There is another way we can model time-varying seasonality, in this
# case using a fourier transform. First compute sine and cosine functions
# using the forecast::fourier() function
data.frame(forecast::fourier(AirPassengers, K = 4)) %>%
  # Use clean_names as fourier() gives terrible name types
  janitor::clean_names() -> fourier_terms
dplyr::glimpse(fourier_terms)

# Now add these new predictors to the training and testing sets
airdat$data_train = airdat$data_train %>%
  dplyr::bind_cols(fourier_terms[1:NROW(airdat$data_train), ])
airdat$data_test = airdat$data_test %>%
  dplyr::bind_cols(fourier_terms[(NROW(airdat$data_train) + 1):
                                   NROW(fourier_terms), ])

# Next we can fit an mvgam model that includes a smooth nonlinear 
# temporal trend and time-varying seasonality. This is done by first 
# using a monotonic smooth of time that ensures the temporal trend never
# declines, which seems appropriate here. The time-varying seasonality
# is captured by allowing the coefficients on the fourier terms to vary
# over time using separate Gaussian Processes
mod2 <- mvgam(y ~ 
                # Monotonic smooth of time to ensure the trend
                # either increases or flattens, but does not 
                # decrease
                s(time, bs = 'moi', k = 10) +
                
                # Time-varying fourier coefficients to capture
                # possibly changing seasonality
                s(time, by = s1_12, k = 5) +
                s(time, by = c1_12, k = 5) +
                s(time, by = s2_12, k = 5) +
                s(time, by = c2_12, k = 5) +
                s(time, by = s3_12, k = 5) +
                s(time, by = c3_12, k = 5),
              family = nb(),
              data = airdat$data_train,
              newdata = airdat$data_test)

# Of course this model still uses a spline of 'time' for the trend, 
# but the monotonic restriction ensures that the predictions are 
# at least somewhat controllable

# This second model is preferred based on in-sample fits
loo_compare(mod1, mod2)

# But what about forecasts? Second model is again slightly preferred
layout(matrix(1:2, nrow = 2))
plot(mod1, type = 'forecast')
plot(mod2, type = 'forecast')
layout(1)

# Look at the smooths to see how the fourier coefficients change
# over time
plot(mod2, type = 'smooths')
summary(mod2, include_betas = FALSE)

# Plotting this model takes a bit more work, as we need to use predictions
# to visualise the effects

# First the trend, using the condition argument in plot_predictions()
p1 <- plot_predictions(mod2, 
                       condition = 'time', 
                       type = 'expected') +
  labs(y = 'Expected passengers',
       title = 'Long-term trend')

# And the time-varying seasonal pattern, which requires subtracting
# the conditional trend predictions from the total predictions
with_season <- predict(mod2, summary = FALSE,
                       type = 'expected')
agg_over_season <- predict(mod2, 
                           newdata = datagrid(model = mod2,
                                              time = unique),
                           summary = FALSE,
                           type = 'expected')
season_preds <- with_season - agg_over_season
p2 <- ggplot(airdat$data_train %>%
               dplyr::mutate(pred = apply(season_preds, 2, mean),
                             upper = apply(season_preds, 2, function(x) 
                               quantile(x, probs = 0.975)),
                             lower = apply(season_preds, 2, function(x) 
                               quantile(x, probs = 0.025))),
             aes(x = time, y = pred)) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.2) +
  geom_line()+
  labs(y = '',
       title = 'Time-varying seasonality')

# Plot these effects together using patchwork
p1 + p2

#### Bonus tasks ####
# 1. Plot the 1st derivative of the temporal trend using plot_slopes()
#    to ensure the monotonic constraint is being respected
?marginaleffects::plot_slopes

# 2. Extract median residuals from both models and plot these against 'season'.
#    Does our model contain enough complexity to capture seasonal patterns well?
?residuals.mvgam

# 3. Since we might not still be comfortable forecasting from a monotonic
#    spline, fit a competing model that uses a piecewise logistic trend
#    instead

#    This requires a 'cap' variable in the data, which specifies the 
#    maximum value we'd expect the series to take
airdat$data_train$cap <- 700
airdat$data_test$cap <- 700
mod3 <- mvgam(y ~ 
                # Time-varying fourier coefficients to capture
                # possibly changing seasonality
                s(time, by = s1_12, k = 5) +
                s(time, by = c1_12, k = 5) +
                s(time, by = s2_12, k = 5) +
                s(time, by = c2_12, k = 5) +
                s(time, by = s3_12, k = 5) +
                s(time, by = c3_12, k = 5) - 1,
              
              # Specify a piecewise logistic growth model;
              # note how we suppressed the intercept in the formula,
              # which is usually necessary in piecewise trend models
              trend_model = PW(growth = 'logistic',
                               n_changepoints = 5),
              family = poisson(),
              data = airdat$data_train)

#    Plot the trend estimates
plot(mod3, type = 'trend')

#    Compute and plot the forecasts from this model 
fc <- mvgam::forecast(object = mod3, newdata = airdat$data_test)
plot(fc)

#    Look at estimates of key logistic growth parameters
summary(mod3, include_betas = FALSE)