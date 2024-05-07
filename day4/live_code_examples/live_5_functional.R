library(mgcv)
library(gamair)
library(marginaleffects)
library(gratia)
library(ggplot2); theme_set(theme_bw())

# Load the Canada Weather observations, taken from 35 Canadian weather stations
data(canWeather)
dplyr::glimpse(CanWeather) # T = mean temperature in Centigrade
levels(CanWeather$region)
CanWeather$doy <- CanWeather$time

# Plot the temperature observations as a function of day, coloured by 'region'
ggplot(CanWeather, aes(x = doy, y = T, colour = region)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~region)

# Functional data analyses treat the observations as 'functions' or 'curves'. Treating
# each station's temperature observations as a function of 'day of year', we might want to
# model these using the 'scalar' covariates 'latitude' and 'region'

# We can fit a function on scalar regression, where
# Temperature(t) = f_r(t) + f(t)*latitude + e(t)
# where e(t) is AR1 Gaussian and f_r is
# a smooth for region r. 

# But we don't know what value to use for the AR1 parameter
# (which needs to be fixed in bam() models). So we use a profile likelihood approach to
# select the best-fitting
aic <- reml <- rho <- seq(0.9, 0.99, by = 0.01)
for(i in 1:length(rho)){
  # Fit models using a grid of possible AR1 parameters
  b <- bam(T ~ s(region, bs = 're') + 
             s(doy, k = 10, bs = "cr", by = region) +
             s(doy, k = 20, bs = "cr", by = latitude),
           data = CanWeather, 
           
           # Tell bam() where each time series begins
           AR.start = time==1, 
           rho = rho[i],
           
           # Use discrete covariates for faster fitting
           discrete = TRUE)
  
  # Extract AIC and -REML scores; we would like to minimise both of these
  aic[i] <- AIC(b)
  reml[i] <- b$gcv.ubre
}
plot(x = rho, y = reml, pch = 16)
plot(x = rho, y = aic, pch = 16)

# 0.97 seems optimal; refit this model
mod1 <- bam(T ~ s(region, bs = 're') + 
              s(doy, k = 10, bs = "cr", by = region) +
              s(doy, k = 20, bs = "cr", by = latitude),
            data = CanWeather, 
            AR.start = time==1, 
            rho = .97,
            discrete = TRUE)

# Plot the smooths
draw(mod1)

# It is difficult to understand what each region's predicted function is because
# the total effect for each region is spread across multiple smooths
# use plot_predictions() instead
plot_predictions(mod1, by = c('doy', 'region'))

plot_predictions(mod1, by = c('doy', 'region', 'region'),
                 points = 0.1)

# Obviously quite a lot of latitudinal variation; 
# inspect latitudinally-varying predictions for a single region
plot_predictions(mod1, 
                 newdata = datagrid(region = 'Continental',
                                    latitude = unique(CanWeather$latitude[
                                      which(CanWeather$region == 'Continental')]),
                                    doy = unique),
                 by = c('doy', 'latitude'))

# Check for any remaining autocorrelation in the residuals, using the standardized
# residuals
acf(mod1$std)

#### Bonus tasks ####
# 1. Clearly there is still some autocorrelation left at lag 2. If you have
#    mvgam installed, fit a model that will deal with this directly
#   (but note, this model will be SLOW)
library(mvgam)
CanWeather$series <- CanWeather$place
mod2 <- mvgam(formula = T ~ 1,
              # Use a State-Space model and change 'region' to a parametric
              # effect to speed up sampling
              trend_formula = ~ region + 
                s(doy, k = 10, bs = "cr", by = region) +
                s(doy, k = 20, bs = "cr", by = latitude),
              data = CanWeather,
              trend_model = AR(p = 2),
              # Use an informative prior on AR1 to speed up sampling
              priors = prior(normal(0.8, 0.1), ub = 1, lb = 0.5,
                             class = ar1),
              family = gaussian())
summary(mod2, include_betas = FALSE)
plot(mod2, type = 'smooths', trend_effects = TRUE)
plot_predictions(mod2, by = c('doy', 'region'))


