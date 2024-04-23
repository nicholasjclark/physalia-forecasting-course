library(mgcv)
library(marginaleffects)
library(dplyr)
library(gratia)
library(ggplot2); theme_set(theme_bw())

# Simulate some data in which multiple covariates may impact the conditional
# response in nonlinear ways
set.seed(0)
simdat <- gamSim()
dplyr::glimpse(simdat)

# Plot the data and fit some simple smooths
ggplot(simdat, aes(x = x0, y  = y)) +
  geom_point() +
  geom_smooth(fill = 'darkred', col = 'darkred')

ggplot(simdat, aes(x = x1, y  = y)) +
  geom_point() +
  geom_smooth(fill = 'darkred', col = 'darkred')

ggplot(simdat, aes(x = x2, y  = y)) +
  geom_point() +
  geom_smooth(fill = 'darkred', col = 'darkred')

ggplot(simdat, aes(x = x3, y  = y)) +
  geom_point() +
  geom_smooth(fill = 'darkred', col = 'darkred')

# The outcome is unbounded continuous (Gaussian appropriate?)
ggplot(simdat, aes(y)) +
  geom_histogram(col = 'white')
summary(simdat$y)

# We've seen smooths in one dimension so far; just to refresh
# Thin plate basis (default in unidimensional smooths)
basis(s(x0), data = simdat) %>%
  draw()

# Cubic regression basis
basis(s(x0, bs = 'cr'), data = simdat) %>%
  draw()

# Changing k
basis(s(x0, bs = 'cr', k = 20), data = simdat) %>%
  draw()

# B-splines with first-derivative penalty
basis(s(x0, bs = 'bs', m = 1), data = simdat) %>%
  draw()

# But how to include interactions in GAMs?

# Option 1: including multiple terms in s()
# This assumes all covariates wrapped in s() are on the same scale and 
# that smoothness should be the same in all dimensions
mod1 <- gam(y ~ s(x0, x1, k = 100, bs = 'tp'),
            data = simdat, family = gaussian(),
            method = 'REML')
summary(mod1)
draw(mod1)
plot_predictions(mod1, condition = c('x0', 'x1'))  
plot_predictions(mod1, condition = c('x1', 'x0'))  

# Option 2: including multiple terms in te()
# This does not assume covariates are on the same scale, or that 
# smoothness should be isotropic. It is much more flexible and is therefore
# preferred
?mgcv::gam.models
?mgcv::te
mod2 <- gam(y ~ te(x0, x1, k = c(10, 10), bs = c('tp', 'tp')),
            data = simdat, family = gaussian(),
            method = 'REML')
summary(mod2)
draw(mod2)
plot_predictions(mod2, condition = c('x0', 'x1'))  
plot_predictions(mod2, condition = c('x1', 'x0')) 

# Option 3: ANOVA decomposition with te() and ti()
# Same assumptions as with te() but this allows the 'main' effects
# of the covariates to be isolated from the 'interaction' effects
mod3 <- gam(y ~ s(x0, k = 10, bs = 'tp') +
              s(x1, k = 10, bs = 'tp') + 
              ti(x0, x1, k = c(10, 10), bs = c('tp', 'tp')),
            data = simdat, family = gaussian(),
            method = 'REML')
summary(mod3)

# Drawing these functions can be very useful but also can be confusing;
# i.e. the total effect of x0 is now spread among two different terms
draw(mod3)

# Using plot_predictions remedies this to show the total effects as before
plot_predictions(mod3, condition = c('x0', 'x1'))  
plot_predictions(mod3, condition = c('x1', 'x0'))

# mod2 and mod3 are roughly equivalent, but mod3 is much more flexible and 
# can allow you to accommodate highly complex models with many interactions
# For example, say we want x0:x1 and x1:x2 interactions, but we don't want
# the full three way interaction x0:x1:x2
mod4 <- gam(y ~ 
              # main, lower-order effects
              s(x0, k = 10, bs = 'tp') +
              s(x1, k = 10, bs = 'tp') + 
              s(x2, k = 10, bs = 'tp') + 
              
              # second-level interaction effects (notice how x1 is allowed
              # to be in both of these)
              ti(x0, x1, k = c(10, 10), bs = c('tp', 'tp')) + 
              ti(x1, x2, k = c(10, 10), bs = c('tp', 'tp')),
            data = simdat,
            family = gaussian(),
            method = 'REML',
            
            # impose extra penalties to help regularize smooths toward
            # straight lines
            select = TRUE)

# Interactions needed?
summary(mod4)

# Now draw() gets even more difficult to interpret
draw(mod4)

# Targeted visuals with plot_predictions(); use custom functions to determine
# how the underlying data_grid() will be set up. This can be useful when plotting
# many effects
summary_round = function(x){
  round(fivenum(x, na.rm = TRUE), 4)
}
seq_even = function(x){
  seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), 
      length.out = 100)
}
plot_predictions(mod4, by = c('x0', 'x1', 'x2'),
                 newdata = datagrid(x0 = seq_even,
                                    x1 = summary_round,
                                    x2 = summary_round))  
plot_predictions(mod4, by = c('x1', 'x0', 'x2'),
                 newdata = datagrid(x1 = seq_even,
                                    x0 = summary_round,
                                    x2 = summary_round))  

# Interactions can also be formed between a multidimensional smooth and
# another effect (i.e. if you want a spatial smooth to interact with a smooth
# of time)
mod5 <- gam(y ~ te(x0, x1, x2, 
                   # d tells te() that we want a 2-dimensional smooth
                   # of x0 and x1 to interact with a 1-dimensional 
                   # smooth of x2
                   d = c(2, 1),
                   # basis types and k values for the two smooths
                   bs = c('cr', 'cr'),
                   k = c(25, 10)),
               data = simdat, family = gaussian(),
               method = 'REML')

# Diagnostics and some plots
summary(mod5)
draw(mod5)
plot_predictions(mod5, by = c('x0', 'x1', 'x2'),
                 newdata = datagrid(x0 = seq_even,
                                    x1 = summary_round,
                                    x2 = summary_round))  

# Model comparisons
anova(mod1, mod2, mod3, mod4, mod5, test = 'Chisq')
