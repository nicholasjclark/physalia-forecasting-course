library(mvgam)  
library(dplyr)     
library(ggplot2); theme_set(theme_bw())       
library(gratia)    
library(marginaleffects)
library(tidybayes)      

# Load the pre-prepared Portal rodent abundance data
portal_ts <- read.csv('portal.csv')

# Inspect the data structure, which contains lunar monthly total 
# captures across control plots for four different rodent species.
# It also contains a 12-month moving average of the unitless NDVI
# vegetation index, and monthly average minimum temperature
# (already scaled to unit variance)
dplyr::glimpse(portal_ts)

# The data contain 80 lunar monthly observations, though there are 
# plenty of NAs in the number of total captures (NAs are shown 
# as red bars in the below plot)
max(portal_ts$time)
image(is.na(t(portal_ts %>%
                dplyr::arrange(dplyr::desc(time)))), axes = F,
      col = c('grey80', 'darkred'))
axis(3, at = seq(0,1, len = NCOL(portal_ts)), 
     labels = colnames(portal_ts))

# The data are already in 'long' format, meaning each series x 
# time observation has its own entry in the dataframe.
# But {mvgam} needs a 'series' column that acts as a factor 
# indicator for the time series. Add one using {dplyr} commands:
portal_ts %>%
  dplyr::mutate(series = as.factor(species)) -> portal_ts
dplyr::glimpse(portal_ts)
levels(portal_ts$series)

# It is important that the number of levels matches the number of 
# unique series in the data to ensure indexing across series works 
# properly in the underlying modelling functions. For more information 
# on how to format data for {mvgam} modelling, see:
# https://nicholasjclark.github.io/mvgam/articles/data_in_mvgam.html

# Create a 'rel_abund' column that we can use as our outcome variable
# in a Beta regression model (see ?mvgam_families for more information
# on the kinds of observation models supported by {mvgam})
portal_ts %>%
  
  # Group by unique time points
  dplyr::group_by(time) %>%
  
  # Calculate total captures per time point
  dplyr::mutate(total = sum(captures, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  
  # Calculate relative abundance; {mvgam} does not yet allow zero- or 
  # one-inflated Beta observations, so we add small offsets for any
  # zeros and ones
  dplyr::mutate(rel_abund = 
                  pmin(0.999, pmax(0.001, 
                                   captures / total))) -> portal_ts
  
# Plot all of the time series together
plot_mvgam_series(data = portal_ts, y = 'rel_abund', series = 'all')

# Plot some more in-depth features for individual series
plot_mvgam_series(data = portal_ts, y = 'rel_abund', series = 1)
plot_mvgam_series(data = portal_ts, y = 'rel_abund', series = 2)
plot_mvgam_series(data = portal_ts, y = 'rel_abund', series = 3)
plot_mvgam_series(data = portal_ts, y = 'rel_abund', series = 4)

# How can you fit a model that allows you to make comparisons among
# the growth rates (i.e. 1st derivatives of the trends) among these 
# time series? Some useful resources:
# https://ecogambler.netlify.app/blog/interpreting-gams/
# https://marginaleffects.com/vignettes/slopes.html
# https://paul-buerkner.github.io/brms/reference/gp.html