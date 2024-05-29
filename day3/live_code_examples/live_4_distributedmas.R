library(mgcv)
library(dplyr)
library(marginaleffects)
library(gratia)
library(ggplot2); theme_set(theme_bw())

# Load some pre-prepared Portal rodent data and filter to only include the Desert
# Pocket Mouse
portal_ts <- read.csv('https://raw.githubusercontent.com/nicholasjclark/EFI_seminar/main/data/portal_data.csv', as.is = T) %>%
  dplyr::filter(species == 'PP')

dplyr::glimpse(portal_ts)

# Number of timepoints
max(portal_ts$time)

# Plot the capture time series, noting there is clear seasonality
# (though this can be hard to see due to the fact that some observations are NA)
ggplot(portal_ts, aes(x = time, y = captures)) +
  geom_point() + 
  geom_smooth(method = gam, formula = y ~ s(x, k = 60))

# Building on from the distributed lag example, we can set up a model that
# allows the effect of the predictor to change smoothly over moving averages
# of lagged temperature. First, a function to create the matrix of lagged moving 
# averages of different orders
mv_mat <- function(x, n_lag = 6) {
  n <- length(x)
  X <- matrix(NA, n, n_lag)
  for (i in 1:n_lag){
    # Aligning to the 'right' in zoo::rollmean() will ensure each 
    # value represents an average of the last i preceeding values
    X[1:n, i] <- zoo::rollmean(x, 
                               k = i, 
                               na.pad = TRUE, 
                               align = 'right')
  } 
  X
}

# Create a matrix of lagged minimum temperature moving average values, with lags up to 
# 9 lunar months in the past
mintemp_mat <- mv_mat(portal_ts$mintemp, 9)
dim(mintemp_mat)
head(mintemp_mat) # Note the NAs; these will be silently omitted when fitting a model

# View the increasingly smoothed predictors
matplot(mintemp_mat, type = 'l')

# Carrying on from the interactions script, we can use a tensor product of this lag
# matrix and another matrix that indexes by lag
lag_mat <- matrix(0:8, nrow(portal_ts), 9, byrow = TRUE)
dim(lag_mat)
head(lag_mat)

# Gather the data in list() format as before
lag_data <- list(
  captures = portal_ts$captures,
  mintemp_mat = mintemp_mat,
  lag_mat = lag_mat
)

# Fit with bam() so we can include autocorrelated residuals as before
mod <- bam(captures ~ 
             te(mintemp_mat, lag_mat),
           data = lag_data,
           family = nb(),
           rho = 0.8,
           discrete = TRUE)
summary(mod)
appraise(mod, method = 'simulate', type = 'deviance')
draw(mod)

# Predict the mean response values (expectations, which ignore the overdispersion
# parameter) and plot against observations
preds <- predict(mod, newdata = lag_data, se.fit = TRUE,
                 type = 'response')
portal_ts %>%
  bind_cols(data.frame(pred = preds$fit,
                       upper = preds$fit + 2 * preds$se.fit,
                       lower = preds$fit - 2 * preds$se.fit)) %>%
  ggplot(., aes(x = time, y = captures)) +
  geom_line(aes(y = pred), linewidth = 1, alpha = 0.4) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.4) +
  geom_point()

# Is this model a better fit compared to the distributed lag model?
lagard <- function(x, n_lag = 6) {
  n <- length(x)
  X <- matrix(NA, n, n_lag)
  for (i in 1:n_lag){
    X[i:n, i] <- x[i:n - i + 1]
  } 
  X
}
mintemp_mat <- lagard(portal_ts$mintemp, 9)
lag_data <- list(
  captures = portal_ts$captures,
  mintemp_mat = mintemp_mat,
  lag_mat = lag_mat
)
mod2 <- bam(captures ~ 
              te(mintemp_mat, lag_mat),
            data = lag_data,
            family = nb(),
            rho = 0.8,
            discrete = TRUE)
AIC(mod); AIC(mod2)
anova(mod, mod2, test = 'Chisq')

#### Bonus tasks ####
# 1. Compare this model to the distributed lag example using AIC() and 
#    anova.gam(...). Why might these models give similar results?

# 2. If you have mvgam installed, refit mod but use a poisson() observation
#    family instead (simultaneously estimating AR1 and overdispersion parameters is 
#    very challenging!). Be sure to exclude the rows of data in which the lag matrices
#    are NAs first
library(mvgam)
mintemp_mat <- mv_mat(portal_ts$mintemp, 9)
lag_mat <- matrix(0:8, nrow(portal_ts), 9, byrow = TRUE)

# Gather the data in list() format as before
lag_data <- list(
  captures = portal_ts$captures,
  mintemp_mat = mintemp_mat,
  lag_mat = lag_mat
)
idx <- which(complete.cases(mintemp_mat))
lag_data_mvgam <- list(
  captures = portal_ts$captures[idx],
  mintemp_mat = mintemp_mat[idx, ],
  lag_mat = lag_mat[idx, ],
  time = portal_ts$time[idx],
  series = factor(rep('series1', length(idx)))
)
mod2 <- mvgam(captures ~ 1,
              trend_formula = ~ te(mintemp_mat, lag_mat),
              data = lag_data_mvgam,
              trend_model = AR(),
              family = nb())
summary(mod2, include_betas = FALSE)
plot(mod2, type = 'residuals')

#    Plot the estimated distributed lag smooth
plot(mod2, type = 'smooths', trend_effects = TRUE)
draw(mod2$trend_mgcv_model)

#    Plot the hindcast distribution
plot(hindcast(mod2))

#    Use pp_check() to see how the model would predict if we 
#    assumed the dynamic AR process was stationary
pp_check(mod2, type = 'ribbon')

# Some additional plots / investigations
pp_check(mod2, type = 'dens_overlay', ndraws = 20)
pp_check(mod2, type = 'ecdf_overlay', ndraws = 20)
mcmc_plot(mod2, variable = 'ar', regex = TRUE, type = 'hist')
mcmc_plot(mod2, variable = 'phi', regex = TRUE)
