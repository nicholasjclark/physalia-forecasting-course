library(dplyr)
library(mvgam)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())
library(quantmod)
library(zoo)

# Extract Texas state population (in thousands of people)
POP <- quantmod::getSymbols(paste('TX', 'POP', sep = ''), 
                            src = 'FRED', auto.assign = FALSE)

# Read the data and grab the Texas measles records
read.csv("measles.csv") %>%
  dplyr::filter(state == 'TX') %>%
  
  # Calculate temporal variables
  dplyr::mutate(year = floor(week / 100),
                week = week - year * 100) %>%
  
  # Add population data
  dplyr::left_join(data.frame(year = as.numeric(
    substr(paste(zoo::index(POP)), 1, 4)),
    population = log(as.numeric(POP)))) %>%
  
  # Keep only a subset of years
  dplyr::filter(year >= 1952 &
                  year < 1972) %>%
  
  # Add a time variable
  dplyr::mutate(time = dplyr::row_number()) %>%
  
  # Add vaccination variables
  dplyr::mutate(vaccine = ifelse(year >= 1963, 1, 0),
                year_since_vaccine = ifelse(
                  year >= 1963, year - 1963, 0)) %>%
  dplyr::mutate(vaccine_era = as.factor(ifelse(vaccine == 0, 'no', 'yes'))) -> measles
plot_mvgam_series(data = measles, y = 'cases')

jpeg('series.jpg', width = 11, height = 7, units = 'in', res = 300)
plot(x = 1:max(measles$time),
     y = measles$cases,
     bty = 'l',
     ylab = 'Measles cases in Texas',
     xaxt = 'n',
     xlab = '',
     type = 'l',
     lwd = 2.5,
     col = 'darkred')
abline(v = 598, lwd = 26, col = "#BEBEBE55")
axis(1, at = seq(1, max(measles$time),
                 by = 52),
     labels = 1952:1971)
box(bty = 'l', lwd = 2)
dev.off()

# Estimate effect of vaccine
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
plot_predictions(mod0, by = 'time', type = 'response',
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

# Using years since vaccination? If we code everything before vaccination as 
# a '1' then perhaps we can interact this in a three-dimensional tensor?
measles %>%
  dplyr::mutate(year_since_vaccine = as.numeric(year_since_vaccine)) -> measles

# Split into training and testing
data_train <- measles %>%
  dplyr::filter(year < 1970)

data_test <- measles %>%
  dplyr::filter(year >= 1970)

# Now a DGAM with dynamic trend and monotonic effect of years since vaccination
# began
mvgam_test <- mvgam(cases ~ offset(population) - 1,
                    trend_formula = ~
                      vaccine_era + 
                      s(week, bs = 'cc', k = 10) +
                      s(year_since_vaccine, bs = 'mod', k = 6),
                    knots = list(week = c(0.5, 52.5)),
                    trend_model = AR(),
                    family = nb(),
                    data = measles)

plot(mvgam_test, type = 'smooths', trend_effects = TRUE)
plot(mvgam_test, type = 'pterms', trend_effects = TRUE) # would have to constrain
plot_predictions(mvgam_test, condition = 'year_since_vaccine',
                 type = 'link')
summary(mvgam_test)
plot(mvgam_test, type = 'forecast')

jpeg('hindcast.jpg', width = 11, height = 7, units = 'in', res = 300)
plot_mvgam_fc(mvgam_test, series = 1, 
              ylab = 'Measles cases in Texas',
              hide_xlabels = TRUE)
abline(v = 598, lwd = 26, col = "#BEBEBE55")

axis(1, at = seq(1, max(data_test$time),
                 by = 52),
     labels = 1952:1971)
dev.off()

jpeg('forecast.jpg', width = 11, height = 7, units = 'in', res = 300)
plot_mvgam_fc(mvgam_test, series = 1, 
              newdata = data_test,
              ylab = 'Measles cases in Texas',
              hide_xlabels = TRUE)
abline(v = 598, lwd = 26, col = "#BEBEBE55")
axis(1, at = seq(1, max(data_test$time),
                 by = 52),
     labels = 1952:1971)
dev.off()

jpeg('inference.jpg', width = 11, height = 7, units = 'in', res = 300)
layout(matrix(c(1,1,2,3), ncol = 2, byrow = TRUE))
plot_mvgam_trend(mvgam_test,ylab = 'Dynamic trend component',
                 hide_xlabels = TRUE)
axis(1, at = seq(1, max(data_test$time),
                 by = 52),
     labels = 1952:1971)
plot_mvgam_smooth(mvgam_test)
hist(mvgam:::mcmc_chains(mvgam_test$model_output, 'phi'),
     main = '',
     ylab = 'Frequency',
     xlab = expression(phi),
     col = 'darkred',
     border = 'white')
dev.off()

plot(mvgam_test, type = 'residuals')


# Counterfactual forecast
trend_fc <- mvgam:::mcmc_chains(mvgam_test$model_output, params = 'trend')
counter_data <- measles
counter_data$year_since_vaccine <- rep(0, NROW(counter_data))
linpred_fc <- predict(mvgam_test, type = 'link',
                      newdata = counter_data)
preds <- matrix(NA, nrow = dim(trend_fc)[1],
                     ncol = dim(trend_fc)[2])
for(i in 1:dim(trend_fc)[1]){
  preds[i, ] <- rpois(dim(trend_fc)[2],
                           lambda = exp(trend_fc[i, ] + linpred_fc[i, ]))
}

orig_preds <- mvgam:::mcmc_chains(mvgam_test$model_output,
                                  params = 'ypred')
probs <- c(0.05, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.95)
cred <- sapply(1:NCOL(preds),
               function(n) quantile(preds[,n],
                                    probs = probs, na.rm = TRUE))
orig_cred <- sapply(1:NCOL(orig_preds),
               function(n) quantile(orig_preds[,n],
                                    probs = probs, na.rm = TRUE))

jpeg('counterfactual.jpg', width = 11, height = 7, units = 'in', res = 300)
plot(x = 1:max(data_train$time),
     y = data_train$cases[1:max(data_train$time)],
     ylim = c(0, 9000),
     pch = 16,
     col = 'white',
     bty = 'l',
     ylab = 'Measles cases in Texas',
     xaxt = 'n',
     xlab = '')

counter_indices <- 597:max(data_train$time)
orig_indices <- 1:max(data_train$time)

polygon(c(orig_indices,
          rev(orig_indices)),
        c(orig_cred[1,orig_indices],
          rev(orig_cred[9,orig_indices])),
        col = "#A25050", border = NA)
lines(x = orig_indices,
      y = orig_cred[5,orig_indices],
      lwd = 2.4,
      col = "#7C0000")
polygon(c(counter_indices,
          rev(counter_indices)),
        c(cred[1,counter_indices],
          rev(cred[9,counter_indices])),
        col = '#80808090', border = NA)
lines(x = counter_indices,
      y = cred[5,counter_indices],
      lwd = 2,
      col = '#808080')
box(bty = 'l', lwd = 2)
axis(1, at = seq(1, max(data_test$time),
                 by = 52),
     labels = 1952:1971)
dev.off()