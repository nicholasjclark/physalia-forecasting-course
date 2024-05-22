# Time-varying periodicity
library(dplyr)
library(forecast)
library(janitor)
library(gratia)
library(mvgam)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())
library(patchwork)

# A function to construct montonically increasing or decreasing
# coefficients
monotonic_fun = function(times, k = 4){
  x <- sort(as.vector(scale(times)))
  exp(k * x) / (1 + exp(k * x))
}

# Sample times
set.seed(123)
times <- 1:120

# Construct Fourier series for two different periodicities
data.frame(forecast::fourier(ts(times, frequency = 12), K = 3)) %>%
  dplyr::bind_cols(forecast::fourier(ts(times, frequency = 6), K = 3)) %>%
  # Use clean_names as fourier() gives terrible name types
  janitor::clean_names() -> fourier_terms

# Create the time-varying Fourier coefficients
betas <- matrix(NA, nrow = length(times), ncol = NCOL(fourier_terms))

# Period 12 coefficients drop montonically toward zero over time
p12_names <- grep('_12', names(fourier_terms), fixed = TRUE)
for(i in p12_names){
  betas[, i] <- monotonic_fun(times, k = runif(1, -3, -0.25))
}

# Period 6 coefficients increase monotonically over time
p6_names <- grep('_6', names(fourier_terms), fixed = TRUE)
for(i in p6_names){
  betas[, i] <- monotonic_fun(times, k = runif(1, 0.25, 3))
}

# Plot the coefficients through time
matplot(betas, type = 'l', xlab = 'Time', lwd = 1.5)

# Calculate the linear predictor (mu) and plot
mu <- vector(length = length(times))
for(i in 1:length(times)){
  mu[i] <- as.matrix(fourier_terms)[i, ] %*% betas[i, ] 
}
plot(mu, type = 'l', xlab = 'Time')

# Add Gaussian Random Walk noise and plot
y <- mu + cumsum(rnorm(length(times), sd = 0.25))
plot(y, type = 'l', xlab = 'Time')

# Construct the data for modeling; this assumes we've thought carefully
# about the problem and have included the Fourier series for both periodicities
# as predictors
dat <- data.frame(y, time = times) %>%
  dplyr::bind_cols(fourier_terms)

# Fit the dynamic GAM, which uses a State-Space representation
# to capture time-varying seasonality, periodicity and the Random Walk
# autocorrelation process
mod <- mvgam(
  
  # Observation formula; empty
  formula = y ~ -1,
  
  # Process formula contains the seasonal terms
  trend_formula = ~
    
    # Time-varying fourier coefficients to capture
    # possibly changing seasonality
    s(time, by = s1_12, k = 5) +
    s(time, by = c1_12, k = 5) +
    s(time, by = s2_12, k = 5) +
    s(time, by = c2_12, k = 5) +
    s(time, by = s3_12, k = 5) +
    s(time, by = c3_12, k = 5) +
    s(time, by = s1_6, k = 5) +
    s(time, by = c1_6, k = 5) +
    s(time, by = s2_6, k = 5) +
    s(time, by = c2_6, k = 5) +
    s(time, by = c3_6, k = 5),
  
  # Random walk for the trend
  trend_model = RW(),
  
  # Gaussian observations
  family = gaussian(),
  
  # Realistic priors on variance components
  priors = c(prior(exponential(2),
                   class = sigma),
             prior(exponential(2),
                   class = sigma_obs)),
  
  # Sampler settings
  burnin = 1000,
  samples = 1000,
  backend = 'cmdstanr',
  
  # Data
  data = dat)

# Model summary
summary(mod, include_betas = FALSE)

# Time-varying fourier coefficients
gratia::draw(mod$trend_mgcv_model)

# Plot the time-varying seasonality using posterior expectations;
# note this plot does not include the RW trend
season_preds <- predict(mod, summary = FALSE,
                       type = 'expected')
p1 <- ggplot(dat %>%
         dplyr::mutate(pred = apply(season_preds, 2, mean),
                       upper = apply(season_preds, 2, function(x) 
                         quantile(x, probs = 0.975)),
                       lower = apply(season_preds, 2, function(x) 
                         quantile(x, probs = 0.025))),
       aes(x = time, y = pred)) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.2) +
  geom_line() +
  labs(y = 'Posterior expectations',
       x = '',
       title = 'Time-varying seasonality')

# Extract the RW trend
trend_preds <- hindcast(mod, type = 'expected')$hindcasts[[1]] - 
  season_preds

p2 <- ggplot(dat %>%
         dplyr::mutate(pred = apply(trend_preds, 2, mean),
                       upper = apply(trend_preds, 2, function(x) 
                         quantile(x, probs = 0.975)),
                       lower = apply(trend_preds, 2, function(x) 
                         quantile(x, probs = 0.025))),
       aes(x = time, y = pred)) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.2) +
  geom_line() +
  labs(y = 'Posterior expectations',
       x = 'Time',
       title = 'RW trend')
p1 + p2 + plot_layout(ncol = 1)
