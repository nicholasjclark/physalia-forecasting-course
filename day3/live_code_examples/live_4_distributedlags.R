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

# We expect captures for this species to vary in response to changing temperatures,
# but this response is delayed. First try a few models using different lags of mintemp
# as the smooth predictors
portal_ts %>%
  dplyr::mutate(mintemp_lag1 = lag(mintemp, n = 1),
                mintemp_lag2 = lag(mintemp, n = 2),
                mintemp_lag3 = lag(mintemp, n = 3)) -> lagged_ts
mod1 <- gam(captures ~ 
              s(mintemp, k = 10) +
              s(mintemp_lag1, k = 10) +
              s(mintemp_lag2, k = 10) +
              s(mintemp_lag3, k = 10),
            family = nb(),
            data = lagged_ts)
summary(mod1)

# The overdispersion parameter is small, suggesting there is substantial
# overdispersion in the observations
mod1$family$getTheta(TRUE)

# Residuals look ok
appraise(mod1, method = 'simulate')

# Smooths look strange though. Why?
draw(mod1)

# These effects are highly correlated, making estimation difficult / impossible
model_concurvity(mod1, pairwise = TRUE) %>%
  draw()

# Try just one lag at a time?
mod1a <- gam(captures ~ 
              s(mintemp_lag1, k = 10),
            family = nb(),
            data = lagged_ts)
summary(mod1a)
draw(mod1a)

mod1b <- gam(captures ~ 
               s(mintemp_lag2, k = 10),
             family = nb(),
             data = lagged_ts)
summary(mod1b)
draw(mod1b)

mod1c <- gam(captures ~ 
               s(mintemp_lag3, k = 10),
             family = nb(),
             data = lagged_ts)
summary(mod1c)
draw(mod1c)

# Perhaps you could use AIC or some other metric to choose among models
AIC(mod1a, mod1b, mod1c)

# But there is a better way to make use of ALL the data. We can set up a model that
# allows the effect of the predictor to change smoothly over lags. First a function
# to set up lag matrices
lagard <- function(x, n_lag = 6) {
  n <- length(x)
  X <- matrix(NA, n, n_lag)
  for (i in 1:n_lag){
    X[i:n, i] <- x[i:n - i + 1]
  } 
  X
}

# Create a lagged matrix of minimum temperature values, with lags up to 
# 9 lunar months in the past
mintemp_mat <- lagard(portal_ts$mintemp, 9)
dim(mintemp_mat)
head(mintemp_mat) # Note the NAs; these will be silently omitted when fitting a model

# Carrying on from the interactions script, we can use a tensor product of this lag
# matrix and another matrix that indexes by lag
lag_mat <- matrix(0:8, nrow(portal_ts), 9, byrow = TRUE)
dim(lag_mat)
head(lag_mat)

# Now we can fit a model that uses MATRICES as predictors. These need to be included
# in a list() of data, rather than the usual data.frame() structure
lag_data <- list(
  captures = portal_ts$captures,
  mintemp_mat = mintemp_mat,
  lag_mat = lag_mat
)

# Fit with bam() so we can include autocorrelated residuals (but we must fix phi)
mod2 <- bam(captures ~ 
              te(mintemp_mat, lag_mat),
            data = lag_data,
            family = nb(),
            rho = 0.8,
            discrete = TRUE)
summary(mod2)
appraise(mod2, method = 'simulate', type = 'deviance')
draw(mod2)

# Predict the mean response values (expectations, which ignore the overdispersion
# parameter) and plot against observations
preds <- predict(mod2, newdata = lag_data, se.fit = TRUE,
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

#### Bonus tasks ####
# 1. See this post: https://ecogambler.netlify.app/blog/distributed-lags-mgcv/ for 
#    for information on distributed lags (including hierarchical variants) and how to
#    produce more useful plots of their effects

# 2. If you have mvgam installed, refit mod2 but use a poisson() observation
#    family instead (simultaneously estimating AR1 and overdispersion parameters is 
#    very challenging!). Be sure to exclude the rows of data in which the lag matrices
#    are NAs first
library(mvgam)
idx <- which(complete.cases(mintemp_mat))
lag_data_mvgam <- list(
  captures = portal_ts$captures[idx],
  mintemp_mat = mintemp_mat[idx, ],
  lag_mat = lag_mat[idx, ],
  time = portal_ts$time[idx],
  series = factor(rep('series1', length(idx)))
)
mod3 <- mvgam(captures ~ 1,
              trend_formula = ~ te(mintemp_mat, lag_mat),
              data = lag_data_mvgam,
              trend_model = AR(),
              family = poisson())


#    Plot the estimated distributed lag smooth
plot(mod3, type = 'smooths', trend_effects = TRUE)
draw(mod3$trend_mgcv_model)

#    Plot the hindcast distribution
plot(hindcast(mod3))
