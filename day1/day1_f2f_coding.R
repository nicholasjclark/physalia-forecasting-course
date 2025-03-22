#### Live coding notes for Tutorial 1 ####

# Load packages
library(dplyr)
library(mvgam) 
library(gratia)
library(ggplot2); theme_set(theme_classic(base_size = 12))
library(marginaleffects)

# Access and inspect the portal data
data("portal_data")
glimpse(portal_data)

plot_mvgam_series(
  data = portal_data,
  y = 'captures',
  series = 'all'
) +
  theme_bw(base_size = 12)

# Extract PP time series
portal_data %>%
  
  # Filter the data to only contain captures of the 'PP' 
  dplyr::filter(series == 'PP') %>%
  droplevels() %>%
  
  # Create a 'count' variable for the outcome
  dplyr::mutate(
    count = captures
  ) %>%
  
  # Add a 'year' variable
  dplyr::mutate(
    year = sort(rep(1:8, 12))[time]
  ) %>%
  
  # Select the variables of interest to keep in the model_data
  dplyr::select(
    series, year, time, count, mintemp, ndvi_ma12
  ) -> model_data

# Plot the time series
plot_mvgam_series(
  data = model_data,
  y = 'count'
) &
  theme_classic(base_size = 12)


# Need to visualise temporal patterns, but difficult to do
# directly due to missingness, zeros etc...

# One solution is to fit a simple model first and then look
# at temporal patterns of residuals
mod0 <- mvgam(
  formula = count ~ 1,
  family = poisson(),
  data = model_data,
  backend = 'cmdstanr',
  silent = 1
)
class(mod0)
str(mod0, max.level = 1)
methods(class = 'mvgam')
summary(mod0)
how_to_cite(mod0)

# Some plots of residuals
plot(mod0) & # use & to add a theme to all subplots
  theme_bw(base_size = 12)
pp_check(
  mod0,
  type = 'resid_ribbon',
  x = 'time',
  ndraws = 50
)

# Can also look at residuals against other potential covariates
resid_data <- augment(
  mod0, robust = TRUE
)

ggplot(
  data = resid_data,
  aes(x = mintemp, y = .resid)
) +
  geom_point(col = 'darkred') +
  labs(x = 'Min Temp',
       y = 'Median residual')

# Add mintemp as a fixed effect
mod1 <- update(
  mod0,
  formula = count ~ mintemp,
  priors = prior(std_normal(),
                 class = mintemp)
)
summary(mod1)
mcmc_plot(
  mod1, 
  variable = 'mintemp',
  type = 'combo'
)
conditional_effects(
  mod1,
  type = 'link'
)

# Some plots of residuals
plot(mod1) &
  theme_bw(base_size = 12)
pp_check(
  mod1,
  type = 'resid_ribbon',
  x = 'time',
  ndraws = 50
)
loo_compare(mod0, mod1)

# Mintemp clearly important but we need to account
# for temporal autocorrelation; try a very wiggly spline?
library(mgcv)
modgam <- gam(
  count ~ mintemp + 
    s(time, k = 63, bs = 'cr'),
  data = model_data,
  family = poisson()
)
summary(modgam)
plot_predictions(
  modgam, 
  condition = 'mintemp'
)
plot_predictions(
  modgam, 
  condition = 'time'
)

# Bayesian regularization protects against this 
# somewhat
mod2 <- mvgam(
  count ~ mintemp + 
    s(time, k = 80, bs = 'cr'),
  data = model_data,
  family = poisson()
)

summary(mod2, include_betas = FALSE)
conditional_effects(
  mod2,
  type = 'link'
)
mcmc_plot(
  mod2, 
  variable = 'mintemp',
  type = 'combo'
)

plot(mod2) &
  theme_bw(base_size = 12)
pp_check(
  mod2,
  type = 'resid_ribbon',
  x = 'time',
  ndraws = 50
)
hc <- hindcast(mod2)
plot(hc)
loo_compare(mod0, mod1, mod2)

mod3 <- mvgam(
  count ~ -1,
  trend_formula = ~ mintemp,
  trend_model = AR(p = 1),
  noncentred = TRUE,
  data = model_data,
  family = poisson()
)
summary(mod3)
mcmc_plot(
  mod3, 
  variable = c('mintemp',
               'ar1'),
  regex = TRUE,
  type = 'combo'
)
hc <- hindcast(mod3)
plot(hc)

