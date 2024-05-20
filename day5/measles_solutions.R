library(dplyr)
library(mvgam)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())

# Read the annual Texas measles data
measles <- read.csv("measles.csv")

# Inspect the data, which contains numbers of measles cases per year
# as well as log(population) and vaccine conditions per year in Texas
dplyr::glimpse(measles)

# Plot features of the incidence time series
plot_mvgam_series(data = measles, y = 'cases')

# A different plot using the date as the correct x-axis
plot(x = 1:max(measles$time),
     y = measles$cases,
     bty = 'l',
     ylab = 'Measles cases in Texas',
     xaxt = 'n',
     xlab = '',
     type = 'l',
     lwd = 2.5,
     col = 'darkred')

# Add an identifier for when the vaccine was rolled out
abline(v = 598, lwd = 26, col = "#BEBEBE55")
axis(1, at = seq(1, max(measles$time),
                 by = 52),
     labels = 1952:1971)
box(bty = 'l', lwd = 2)

# Estimate effect of vaccine
measles %>%
  dplyr::mutate(vaccine_era = as.factor(vaccine_era)) -> measles
mod0 <- bam(cases ~ offset(population) + 
              te(week, time, 
                 bs = c('cc', 'tp'), 
                 k = c(10, 25),
                 by = vaccine_era),
            knots = list(week = c(0.5, 52.5)),
            family = nb(),
            data = measles,
            discrete = TRUE,
            nthreads = 4)

summary(mod0)
gratia::draw(mod0)

# Plot predictions and conditional effects
plot_predictions(mod0, 
                 by = 'time', 
                 type = 'response',
                 points = 0.5)

# Average seasonal pattern
plot_predictions(mod0, condition = 'week')

# Seasonality over time for the pre-vaccine era
plot_predictions(mod0, by = c('week', 'time'),
                 newdata = datagrid(model = mod0,
                                    time = seq(1, 
                                               max(measles$time[
                                                 which(measles$vaccine == 0)]),
                                               length.out = 6),
                                    week = unique,
                                    population = max,
                                    vaccine_era = 'no'),
                 conf_level = 0.8)

# And the post-vaccine era
plot_predictions(mod0, by = c('week', 'time'),
                 newdata = datagrid(model = mod0,
                                    time = seq((max(measles$time[
                                      which(measles$vaccine == 0)]) + 1),
                                      max(measles$time),
                                      length.out = 4),
                                    week = unique,
                                    population = max,
                                    vaccine_era = 'yes'),
                 conf_level = 0.8)

# Comparing seasonal patterns among eras
plot_predictions(mod0, condition = c('week', 'vaccine_era'),
                 newdata = datagrid(model = mod0,
                                    week = unique,
                                    vaccine_era = unique,
                                    population = max),
                 conf_level = 0.8)

# Time trends in both eras (near the peak season at week 15)
plot_predictions(mod0, by = c('time'),
                 newdata = datagrid(model = mod0,
                                    time = unique(measles$time[
                                      which(measles$vaccine == 0)]),
                                    week = 15,
                                    population = max,
                                    vaccine_era = 'no'),
                 conf_level = 0.8)
plot_predictions(mod0, by = c('time'),
                 newdata = datagrid(model = mod0,
                                    time = unique(measles$time[
                                      which(measles$vaccine == 1)]),
                                    week = 15,
                                    population = max,
                                    vaccine_era = 'yes'),
                 conf_level = 0.8)

# Average (marginal) effect of vaccine intervention
plot_predictions(mod0, by = 'vaccine_era')

# Fit GP of time that must extrapolate flatly after
# vaccine rollout. This can be done by creating a new 'time' variable
# that halts when the vaccine is rolled out. A monotonically decreasing
# spline is then used to capture how the trend declined after vaccine
# rollout
measles %>%
  dplyr::mutate(prevacc_time = ifelse(time <= 597, time, 597),
                year_since_vaccine = as.numeric(year_since_vaccine)) -> measles

# There will be some divergences because the alpha parameter of the GP,
# and the intercept and overdispersion parameters, are all competing a
# bit to capture the variability in the data
mvgam_test <- mvgam(cases ~ offset(population),
                    trend_formula = ~
                      s(week, bs = 'cc', k = 10) +
                      gp(prevacc_time, c = 5/4, k = 70, scale = FALSE) +
                      s(year_since_vaccine, bs = 'mod', k = 8),
                    trend_model = AR(),
                    priors = prior(beta(1, 10), 
                                   class = ar1,
                                   ub = 0.5),
                    trend_knots = list(week = c(0.5, 52.5)),
                    family = nb(),
                    data = measles)
summary(mvgam_test, include_betas = FALSE)
plot(mvgam_test, type = 'forecast')

# Ensure predictions look reasonable
pp_check(mvgam_test, type = 'ribbon')

# Look at the estimated nonlinear effects
plot(mvgam_test, type = 'smooths', trend_effects = TRUE)
conditional_effects(mvgam_test, type = 'link')

# Look at the estimated effect of time since vaccination
plot_predictions(mvgam_test, 
                 condition = 'year_since_vaccine',
                 type = 'expected')

# Average (marginal) effect of vaccine intervention, as an expected 
# decline in cases per month
avg_slopes(mvgam_test, 
           variables = 'year_since_vaccine',
           type = 'expected')

# Visualise the estimated slope
plot_slopes(mvgam_test, 
            by = 'year_since_vaccine', 
            variable = 'year_since_vaccine',
            newdata = datagrid(year_since_vaccine = seq(0, 8, by = 0.25)),
            type = 'expected')

# Another approach: use a counterfactual forecast that extrapolates
# the GP into the vaccine period but that 'pretends' that the rollout
# of the vaccine never happened. This is possible becuase the GP is a 
# proper time series model and won't give wacky extrapolations
newdata <- measles
newdata$prevacc_time <- newdata$time
newdata$year_since_vaccine <- 0
preds <- predict(mvgam_test, 
                 newdata = newdata, 
                 type = 'response',
                 robust = TRUE)

ggplot(measles %>%
         dplyr::bind_cols(upper = preds[,4],
                          lower = preds[,3],
                          med = preds[,1]),
       aes(time, med)) + 
  geom_vline(xintercept = 597, linetype = 'dashed') +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower),
              alpha = 0.2,
              fill = 'darkred') +
  geom_line(col = 'darkred', linewidth = 0.75) +
  geom_point(aes(y = cases)) +
  ylab('Counterfactual predictions') +
  xlab('Time') +
  theme_classic()

# Comparisons between the actual observations and this 
# counterfactual forecast might give a more realistic and reasonable
# estimate of the true effect of the vaccine rollout
