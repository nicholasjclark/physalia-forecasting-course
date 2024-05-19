library(dplyr)
library(mvgam)
library(marginaleffects)
library(ggplot2); theme_set(theme_bw())

# Load the 'aphids' data from the ecostats package
load(url('https://github.com/dwarton/ecostats/raw/main/data/aphids.RData'))

# Read about the data here:
# https://search.r-project.org/CRAN/refmans/ecostats/html/aphids.html

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

# How would you model these data to ask whether the treatment
# (birds_excluded) results in a different count of aphids on these
# crops over time? Some resources that you might find useful:
# https://rdrr.io/cran/mgcv/man/smooth.construct.sz.smooth.spec.html
# https://ecogambler.netlify.app/blog/interpreting-gams/
# The CAR() example: https://nicholasjclark.github.io/mvgam/reference/RW.html


