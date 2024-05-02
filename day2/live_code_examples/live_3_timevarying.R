library(mgcv)
library(dplyr)
library(marginaleffects)
library(gratia)
library(ggplot2); theme_set(theme_bw())

#### Time-varying coefficients in mgcv ####

# First a function to simulate from a squared exponential Gaussian Process
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

# Fit a model that assumes the coefficient is static
dat <- data.frame(y, x, time = 1:N)
mod0 <- gam(y ~ x + time, data = dat, method = 'REML')
summary(mod0)
plot_predictions(mod0, condition = 'x', points = 0.5)
plot_predictions(mod0, by = 'time', points = 0.5)
avg_slopes(mod0, variables = 'x')
appraise(mod0, n_simulate = 100, method = 'simulate')

# Now a model that allows the coefficient to vary over time;
# this makes use of the 'by' argument in the s() function. The idea is
# that we fit a smooth function of 'time' that gets multiplied with the covariate
# x, effectively allowing us to estimate arbitrarily-nonlinear effects that change
# over time
mod1 <- gam(y ~ s(time, by = x, k = 50),
            data = dat, method = 'REML')
summary(mod1)
draw(mod1)
plot_predictions(mod1, by = c('time'), points = 0.5)

summary_round = function(x){
  round(fivenum(x, na.rm = TRUE), 4)
}
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = summary_round,
                                    time = 1:N),
                 points = 0.5)

# Easier to visualise the time-varying effect if we fix the x value
mean_round = function(x){
  round(mean(x, na.rm = TRUE), 4)
}
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time = 1:N))

avg_slopes(mod1, variables = 'x')
appraise(mod1, n_simulate = 100, method = 'simulate')

# How would this smooth extrapolate?
plot_predictions(mod1, by = c('time', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time = 1:(N + 20)))

# Not really very realistic; what we want is a proper time series
# model for the coefficient. A quick way to do this in mgcv is to use
# a Random Walk basis
library(MRFtools) # devtools::install_github("eric-pedersen/MRFtools")
rw_penalty <- mrf_penalty(object = 1:(N + 20),
                          type = 'linear')
dat$time_factor <- factor(1:N, levels = as.character(1:(N + 20)))
dat$time_factor

mod2 <- gam(y ~ s(time_factor, by = x,
                  bs = "mrf",
                  k = 25,
                  xt = list(penalty = rw_penalty)),
            data = dat,
            drop.unused.levels = FALSE,
            method = "REML")
summary(mod2)
draw(mod2)
plot_predictions(mod2, by = c('time_factor', 'x'),
                 newdata = datagrid(x = mean_round,
                                    time_factor = 1:(N + 20)))

# Because 'time' is a factor here, it is a bit harder to visualise. It is probably
# easier to resort to our own plots
newdat <- datagrid(model = mod2,
                   x = mean_round,
                   time_factor = 1:(N + 20)) %>%
  # Add a continuous version of 'time' for plotting
  dplyr::mutate(time = 1:(N + 20))
preds <- predict(mod2,
                 newdata = newdat,
                 type = 'response', se = TRUE)
newdat$pred <- preds$fit
newdat$upper <- preds$fit + 1.96*preds$se.fit
newdat$lower <- preds$fit - 1.96*preds$se.fit
ggplot(newdat, aes(x = time, y = pred)) +
  geom_line(linewidth = 1, alpha = 0.6) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.3, col = NA) +
  theme_classic() +
  theme(legend.position = 'none')

# These extrapolations are much more realistic!
