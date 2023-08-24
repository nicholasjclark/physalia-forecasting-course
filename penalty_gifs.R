## Produces an animation showing how penalties result in smoother splines

## packages required
library('ggplot2')
library('ggborderline')
library('dplyr')
library('mgcv')
library('gganimate')
library('magick')
library('rsvg')
library('transformr')
library('viridis')

theme_set(theme_classic(base_size = 18, base_family = 'serif'))

## generate some data
set.seed(7)
N <- 400
y <- mvgam::sim_mvgam(T = N, prop_missing = 0,
                      prop_train = 1,
                      family = gaussian(),
                      n_series = 1,
                      trend_model = 'GP',
                      trend_rel = 0.95)$data_train$y
x <- 1:N
data <- data.frame(x = x,
                   y = y)
plot(y ~ x, data = data)

bestmod <- gam(y ~ s(x, k = 40), data = data)
plot(bestmod)
best_sp <- unname(bestmod$sp)

mod <- gam(y ~ s(x, k = 100), data = data,
           sp = 0.9)
plot(mod)

# fit gams with different penalties
penalties <- c(1000, 0.00000001,
               50, 0.0000001,
               20, 0.00001, 10,
               0.00000005, 3,
               0.000005, 0.15, 2,  
               0.000009, 0.5,
               0.0000098, 0.3,
               0.000001, 0.009,
               0.00001, 0.008,
               0.0001, 0.006,
               0.0005, 0.005,
               0.002, 0.001,
               0.0007, 0.005,
               0.00099, 0.0015,
               0.000995, 0.001)
line_dat <- do.call(rbind, lapply(seq_along(penalties), function(sp){
  mod <- gam(y ~ s(x, k = 100), data = data,
              sp = penalties[sp])
  modpreds <- predict(mod,
                      newdata = data.frame(x = seq(1, N, length.out = 400)),
                       type = 'response')
  data.frame(pred = modpreds,
             x_line = seq(1, N, length.out = 400),
             penalty = penalties[sp],
             draw = sp)
}))


p1 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  geom_borderline(data = line_dat, aes(x = x_line, y = pred), 
                  linewidth = 1.5, bordercolour = "white") +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  ylab('f(Time)') + xlab('Time')
p1

gif1 <- animate(
  p1 + transition_states(draw, 
                         transition_length = 2, 
                         state_length = 2) + 
    ease_aes('cubic-in-out'),
  nframes = 160, device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)

line_dat$penalty <- log(line_dat$penalty)
line_dat$dens <- dnorm(line_dat$penalty, -6.9, 6)

line_dat %>%
  dplyr::select(penalty, draw, dens) %>%
  dplyr::distinct() -> plot_dat
stats_df <- data.frame(prior = rnorm(100000, -6.9, 6))
p2 <- ggplot(plot_dat, aes(x = penalty)) +
  geom_segment(aes(x = penalty, 
                   xend = penalty, 
                   y = 0, yend = dens),
             size = 1.5, col = 'white') +
  geom_segment(aes(x = penalty, 
                   xend = penalty, 
                   y = 0, yend = dens),
               size = 1.3, col = 'darkred') +
  geom_point(aes(x = penalty, y = dens), 
             shape = 21, size = 2,
             col = 'white', fill = 'darkred') +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  xlab(expression(rho)) + ylab('log(likelihood)')
  
p2
gif2 <- animate(
  p2 + transition_states(draw, 
                         transition_length = 2, 
                         state_length = 2) + 
    shadow_mark(exclude_layer = c(1,2)) +
    ease_aes('cubic-in-out'),
  nframes = 160, device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)


new_gif <- image_append(c(gif1[1], gif2[1]))
for(i in 2:160){
  combined <- image_append(c(gif1[i], gif2[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
anim_save(filename = 'day1/resources/penalty_spline.gif',
          animation = new_gif)
