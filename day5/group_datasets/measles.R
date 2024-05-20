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

# How might you fit a model that allows the seasonal pattern to change over
# time AND that allows you to ask how cases dropped after the vaccine was
# rolled out? Some useful resources:
?mgcv::bam
# https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/te
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# https://ecogambler.netlify.app/blog/interpreting-gams/
# https://nicholasjclark.github.io/mvgam/reference/monotonic.html