library(mgcv)
library(dplyr)
library(marginaleffects)
library(gratia)
library(ggplot2); theme_set(theme_bw())

# Define a function to simulate from a squared exponential Gaussian Process
# N: number of timepoints to simulate
# c: constant (average coefficient value)
# alpha: amplitude of variation
# rho: length scale (how 'wiggly' should the time-varying effect be?)
sim_gp = function(N, c, alpha, rho){
  Sigma <- alpha ^ 2 *
    exp(-0.5 * ((outer(1:N, 1:N, "-") / rho) ^ 2)) +
    diag(1e-9, N)
  c + mgcv::rmvn(1,
                 mu = rep(0, N),
                 V = Sigma)
}

# Simulate a time-varying coefficient that has a mean value of 0.5
# (i.e. the effect tends to be positive, but can change smoothly over time)
set.seed(3)
N <- 150
beta_t <- sim_gp(alpha = 0.75,
                 rho = 15,
                 c = 0.5,
                 N = N)

# Plot the coefficient
plot(beta_t, type = 'l', lwd = 3,
     bty = 'l', xlab = 'Time',
     ylab = 'Coefficient',
     col = 'darkred')

# Now simulate the covariate itself
x <- rnorm(N, 5, sd = 1)
plot(x, type = 'l', lwd = 3,
     bty = 'l', xlab = 'Time',
     ylab = 'Covariate (x)',
     col = 'darkred')

# Simulate the linear predictor, which is a simple linear model:
# mu = beta_0 + x_t * beta_t
beta_0 <- 3
mu <- beta_0 + x * beta_t

# And simulate some Gaussian noise
y <- rnorm(N, mean = mu, sd = 0.5)
plot(y, type = 'l', lwd = 3,
     bty = 'l', xlab = 'Time',
     ylab = 'Outcome (y)',
     col = 'darkred')

# Fit a linear model that assumes the coefficient is static
dat <- data.frame(y, x, time = 1:N)
mod0 <- gam(y ~ x + time, data = dat, method = 'REML')
summary(mod0)

# Use plot_predictions() for effect interrogation
plot_predictions(mod0, condition = 'x', points = 0.5)
plot_predictions(mod0, by = 'time', points = 0.5)

# We can also calculate the average slope for the effect of x
avg_slopes(mod0, variables = 'x')

# Model diagnostics with gratia
appraise(mod0, n_simulate = 100, method = 'simulate')

# This model clearly is inadequate. 
# Now try a model that allows the coefficient to vary over time;
# this makes use of the 'by' argument in the s() function. The idea is
# that we fit a smooth function of 'time' that gets multiplied with the covariate
# x, effectively allowing us to estimate arbitrarily-nonlinear effects that change
# over time (look at the documentation on the 'by' argument in the s() help page
# for more details)
?mgcv::s
mod1 <- gam(y ~ s(time, by = x, k = 25),
            data = dat, method = 'REML')
summary(mod1)

# Now we can use draw() from gratia to inspect the estimated smooth
draw(mod1)

# And again use plot_predictions() to interrogate effects
summary_round = function(x){
  round(fivenum(x, na.rm = TRUE), 4)
}
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = summary_round,
                                    time = 1:N),
                 points = 0.5)

# It may be easier to visualise the time-varying effect if we fix the x value
mean_round = function(x){
  round(mean(x, na.rm = TRUE), 4)
}
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time = 1:N))

# Average slope is again easy to compute
avg_slopes(mod1, variables = 'x')

# And model diagnostics look better now
appraise(mod1, n_simulate = 100, method = 'simulate')

# But how would this smooth extrapolate if we wanted to forecast ahead?
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time = 1:(N + 20))) +
  geom_vline(xintercept = N, linetype = 'dashed')

# Not really very realistic, the exrapolation behaviour is completely different
# from the historical behaviour; what we want is a proper time series
# model for the coefficient. A quick way to do this in mgcv is to use
# a Random Walk basis
library(MRFtools) # devtools::install_github("eric-pedersen/MRFtools")
?mgcv::mrf

# Setting up a RW penalty requires a factor version of time, with factor 
# levels that extend as far ahead as we would like to forecast
rw_penalty <- mrf_penalty(object = 1:(N + 20),
                          type = 'linear')
dat$time_factor <- factor(1:N, levels = as.character(1:(N + 20)))
dat$time_factor

# Fit the model using the mrf basis and including the RW penalty
mod2 <- gam(y ~ s(time_factor, by = x,
                  bs = "mrf",
                  k = 25,
                  xt = list(penalty = rw_penalty)),
            data = dat,
            
            # Keep all factor levels in the data
            drop.unused.levels = FALSE,
            method = "REML")
summary(mod2)

# draw() doesn't work yet for MRFs
draw(mod2)

# plot_predictions() will work, but the result isn't what we're after
plot_predictions(mod2, by = c('time_factor', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time_factor = 1:(N + 20))) +
  geom_vline(xintercept = N, linetype = 'dashed')

# Because 'time' is a factor here, it is a bit harder to visualise. It is probably
# easier to resort to our own plots. Create a datagrid() for predicting over
newdat <- datagrid(model = mod2,
                   x = mean_round,
                   time_factor = 1:(N + 20)) %>%
  # Add a continuous version of 'time' for plotting
  dplyr::mutate(time = 1:(N + 20))

# Predict from the model using this grid
preds <- predict(mod2,
                 newdata = newdat,
                 type = 'response', se = TRUE)
newdat$pred <- preds$fit
newdat$upper <- preds$fit + 1.96*preds$se.fit
newdat$lower <- preds$fit - 1.96*preds$se.fit

# Visualise the predictions
ggplot(newdat, aes(x = time, y = pred)) +
  geom_line(linewidth = 1, alpha = 0.6, col = 'darkred') +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.3, col = NA, fill = 'darkred') +
  theme_classic() +
  theme(legend.position = 'none')

# These extrapolations are much more realistic

#### Bonus tasks ####
# 1. Plot residuals of mod2 against the predictor x, and against time, to 
#    look for any unmodelled autocorrelation

# 2. Compare the mgcv models (mod0, mod1 and mod2)
#    Generalized Likelihood Ratio test (hint, see anova.gam() for help)
?anova.gam

# 3. If you have mvgam installed, you can go one better using the dynamic() wrapper
#    to model the temporal effect with a Gaussian Process
library(mvgam)
mod3 <- mvgam(y ~ dynamic(x, k = 25, scale = FALSE),
              data = dat,
              family = gaussian())

#    Use mcmc_plot() to inspect posterior estimates for the GP parameters,
#    and compare them to the simulated truths
?mcmc_plot.mvgam

#    Use plot_predictions() as you did for mod1 to inspect how the time-varying
#    effect extrapolates
