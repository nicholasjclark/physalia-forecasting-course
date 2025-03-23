library(mgcv)
library(gamair)
library(marginaleffects)
library(gratia)
library(ggplot2)
theme_set(theme_bw(base_size = 12))

# Load the Canada Weather observations, 
# taken from 35 Canadian weather stations
data(canWeather)
dplyr::glimpse(CanWeather)
levels(CanWeather$region)
CanWeather$doy <- CanWeather$time

# Plot the temperature observations as a function 
# of day, coloured by 'region'
ggplot(
  CanWeather, 
  aes(x = doy, y = T, colour = region)
) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ region)

# Functional data analyses treat the observations 
# as 'functions' or 'curves'. Treating each station's 
# temperature observations as a function of 'day of year', 
# we might want to model these using the 'scalar' 
# covariates 'latitude' and 'region'

# We can fit a function on scalar regression, where
# Temperature(t) = f_r(t) + f(t)*latitude + e(t)
# where e(t) is AR1 Gaussian and f_r is
# a smooth for region r.

# But we don't know what value to use for the AR1 parameter
# (which needs to be fixed in bam() models). So 
# we use a profile likelihood approach to
# select the best-fitting
aic <- reml <- rho <- seq(0.90, 0.99, by = 0.01)
for (i in 1:length(rho)) {
  # Fit models using a grid of possible AR1 parameters
  b <- bam(
    T ~ s(region, bs = "re") +
      s(doy, k = 10, bs = "cr", by = region) +
      s(doy, k = 20, bs = "cr", by = latitude),
    data = CanWeather,

    # Tell bam() where each time series begins
    AR.start = time == 1,
    rho = rho[i],

    # Use discrete covariates for faster fitting
    discrete = TRUE
  )

  # Extract AIC and -REML scores; we would like to 
  # minimise both of these
  aic[i] <- AIC(b)
  reml[i] <- b$gcv.ubre
}
plot(x = rho, y = reml, pch = 16)
plot(x = rho, y = aic, pch = 16)

# 0.97 seems optimal; refit this model
mod1 <- bam(
  T ~ s(region, bs = "re") +
    s(doy, k = 10, bs = "cr", by = region) +
    s(doy, k = 20, bs = "cr", by = latitude),
  data = CanWeather,
  AR.start = time == 1,
  rho = 0.97,
  discrete = TRUE
)

# Plot the smooths
draw(mod1)

# It is difficult to understand what each region's 
# predicted function is because the total effect 
# for each region is spread across multiple smooths;
# use plot_predictions() instead
plot_predictions(
  mod1, 
  by = c("doy", "region")
)

plot_predictions(
  mod1,
  by = c("doy", "region", "region"),
  points = 0.1
)

# Obviously quite a lot of latitudinal variation;
# inspect latitudinally-varying predictions for a single region
plot_predictions(
  mod1,
  newdata = datagrid(
    region = "Continental",
    latitude = unique(CanWeather$latitude[
      which(CanWeather$region == "Continental")
    ]),
    doy = unique
  ),
  by = c("doy", "latitude")
) +
  labs(title = 'Continential region predictions')

# Check for any remaining autocorrelation in the residuals, 
# using the standardized residuals
acf(mod1$std)

#### Bonus tasks ####
# 1. Clearly there is still some autocorrelation left at lag 2. If you have
#    mvgam installed, consider a model that will deal with this directly

library(mvgam)
# Z-scoring the outcome can make placing priors on intercepts and variances
# much easier
CanWeather %>%
  dplyr::mutate(T_scaled = as.vector(scale(T)),
                series = place) -> CanWeather
mod2 <- mvgam(
  # Use a State-Space Gaussian model; no observation covariates
  formula = T_scaled ~ -1,
  
  # Hierarchical smooths in the process model
  trend_formula = ~
    region +
    s(doy, k = 10, by = region, bs = 'cr') +
    s(doy, k = 20, by = latitude, bs = 'cr'),
  
  # AR(1) process with informative priors on 
  # AR and sigma pars to speed up sampling
  trend_model = AR(p = 1),
  priors = c(
    prior(
      beta(5, 1),
      class = ar1,
      lb = 0,
      ub = 1,
    ),
    prior
    (std_normal(),
      class = b
    ),
    prior(
      std_normal(),
      class = Intercept_trend
    ),
    prior(
      beta(4, 4),
      class = sigma,
      lb = 0,
      ub = 1
    )
  ),
  
  # Shared Gaussian observation error for all series
  family = gaussian(),
  share_obs_params = TRUE,
  burnin = 250,
  samples = 250,
  
  # The data
  data = CanWeather,
  run_model = TRUE
)

# Interrogate the model
summary(
  mod2, 
  include_betas = FALSE
)

draw(
  mod2, 
  trend_effects = TRUE
)
plot_predictions(
  mod2, 
  by = c("doy", "region"), 
  type = "expected"
)
plot_predictions(
  mod2,
  newdata = datagrid(
    region = "Continental",
    latitude = unique(CanWeather$latitude[
      which(CanWeather$region == "Continental")
    ]),
    doy = unique
  ),
  by = c("doy", "latitude")
) +
  labs(title = 'Continential region predictions')
