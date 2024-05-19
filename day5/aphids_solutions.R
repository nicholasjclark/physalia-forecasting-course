library(dplyr)
library(mvgam)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())

# Load the 'aphids' data from the ecostats package
load(url('https://github.com/dwarton/ecostats/raw/main/data/aphids.RData'))

# Bind the two datasets (experimental observations of aphid abundances
# over time in oat and wheat crop plots, under two experimental treatments)
aphid_dat <- aphids$oat %>%
  dplyr::mutate(crop = 'oat',
                series = paste0('oat_plot_', Plot)) %>%
  dplyr::bind_rows(aphids$wheat %>%
                     dplyr::mutate(crop = 'wheat',
                                   series = paste0('wheat_plot_', Plot)))
# View the data structure
dplyr::glimpse(aphid_dat)

# Wrangle data to improve variable names and create a
# time_since_treat variable
aphid_dat %>%
  dplyr::mutate(series = as.factor(series),
                crop = as.factor(crop),
                birds_excluded = as.factor(ifelse(
                  Treatment == 'excluded', 'yes', 'no')),
                time_since_treat = Time) %>%
  janitor::clean_names() %>%
  dplyr::select(-plot, -logcount, -treatment) %>%
  dplyr::arrange(series, time) -> aphid_ts

# Plot the data
aphid_ts %>%
  ggplot(., aes(x = time_since_treat,
                y = counts,
                col = birds_excluded)) +
  geom_line() +
  geom_point() +
  facet_wrap(~series)

# An mgcv model to get started
mod0 <- gam(counts ~
              s(series, bs = 're') +
              crop*birds_excluded +
              s(time_since_treat, k = 7) +
              s(time_since_treat,
                birds_excluded, crop,
                bs = 'sz', k = 7),
            data = aphid_ts,
            family = nb())
plot(mod0, pages = 1)
gratia::appraise(mod0, method = 'simulate')

# Fit against observed data
plot_predictions(mod0,
                 by = c('time_since_treat',
                        'birds_excluded',
                        'series'),
                 points = 0.5)

# Average predictions for each treatment
plot_predictions(mod0,
                 by = c('time_since_treat',
                        'birds_excluded',
                        'crop'))

# Smoother predictions on a fine grid
plot_predictions(mod0,
                 by = c('time_since_treat',
                        'birds_excluded'),
                 newdata = datagrid(model = mod0,
                                    time_since_treat = 3:40,
                                    crop = 'oat',
                                    birds_excluded = unique))

plot_predictions(mod0,
                 by = c('time_since_treat',
                        'birds_excluded'),
                 newdata = datagrid(model = mod0,
                                    time_since_treat = 30:70,
                                    crop = 'wheat',
                                    birds_excluded = unique))

# Inspect residuals for autocorrelation
aphid_ts %>%
  dplyr::bind_cols(resids = residuals(mod0)) %>%
  ggplot(., aes(time_since_treat, resids)) +
  geom_line() +
  geom_point() +
  facet_wrap(~series) +
  geom_hline(yintercept = 0, linetype = 'dashed')

# Need an mvgam model
mod1 <- mvgam(counts ~ -1,
              trend_formula = ~
                s(trend, bs = 're') +
                crop*birds_excluded +
                s(time_since_treat, k = 7) +
                s(time_since_treat,
                  birds_excluded, crop,
                  bs = 'sz', k = 7),
              priors = c(prior(beta(2, 2),
                               class = ar1,
                               ub = 1),
                         prior(exponential(5),
                               class = sigma,
                               ub = 1),
                         prior(normal(0, 10), class = lambda_trend)),
              data = aphid_ts,
              family = poisson(),
              trend_model = CAR())
summary(mod1, include_betas = FALSE)
gratia::draw(mod1$trend_mgcv_model)

conditional_effects(mod1) # Fails
mcmc_plot(mod1, variable = 'ar1', regex = TRUE,
          type = 'hist')
mcmc_plot(mod1, variable = 'sigma', regex = TRUE,
          type = 'hist')

# Fit against observed data
plot_predictions(mod1,
                 by = c('time_since_treat',
                        'birds_excluded',
                        'series'),
                 points = 0.5)

# Average predictions for each treatment
plot_predictions(mod1,
                 by = c('time_since_treat',
                        'birds_excluded',
                        'crop'),
                 type = 'expected')

# Smoother predictions on a fine grid
plot_predictions(mod1,
                 by = c('time_since_treat',
                        'birds_excluded'),
                 newdata = datagrid(model = mod1,
                                    time_since_treat = 3:40,
                                    crop = 'oat',
                                    birds_excluded = unique),
                 type = 'expected')

plot_predictions(mod1,
                 by = c('time_since_treat',
                        'birds_excluded'),
                 newdata = datagrid(model = mod1,
                                    time_since_treat = 30:70,
                                    crop = 'wheat',
                                    birds_excluded = unique),
                 type = 'expected')

# Marginal effects at each timepoint
avg_predictions(mod1,
                variables = c('time_since_treat',
                              'crop', 'birds_excluded'),
                type = 'expected')

# Inspect residuals for autocorrelation
aphid_ts %>%
  dplyr::bind_cols(resids = residuals(mod1)[,1]) %>%
  ggplot(., aes(time_since_treat, resids)) +
  geom_line() +
  geom_point() +
  facet_wrap(~series) +
  geom_hline(yintercept = 0, linetype = 'dashed')
