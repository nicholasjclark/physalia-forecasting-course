## Produces an animation showing how a dynamic factor induces correlations among
# time series

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

## generate some correlated time series
set.seed(66)
dynfactor <- mvgam:::sim_ar3(h = 100, tau = 50)
dynfactor <- as.vector(scale(dynfactor))
plot(dynfactor, type = 'l')

series <- matrix(NA, nrow = 100, ncol = 4)
loadings <- c(-0.75, -0.6, 0.6, 0.9)
means <- c(-1, 1, -1, 1)
for(s in 1:4){
  series[, s] <- means[s] + (dynfactor * loadings[s]) +
    cumsum(rnorm(100, sd = 0.15))
                       
}

data <- data.frame(time = rep(1:100, 5),
                   y = c(series[,1],
                         series[,2],
                         series[,3],
                         series[,4],
                         dynfactor - 0.5),
                   series = c(sort(rep(paste0('series', 1:4), 100)),
                              rep('factor', 100))) %>%
  dplyr::mutate(series = factor(series, levels = c(paste0('series', 1:4),
                                                   'factor')))

mod <- gam(y ~ s(time, k = 50, by = series), data = data)
preds <- predict(mod, se.fit = TRUE)
data$ymin <- data$y - 2*preds$se.fit
data$ymax <- data$y + 2*preds$se.fit


## now animate the dynamic factor
static <- data %>%
  dplyr::filter(series != 'factor') %>%
  dplyr::mutate(static_time = time) %>%
  dplyr::select(-time)

title_dat = data.frame(time = 1:100,
                       title = c('Collection of time series',
                rep('Dynamic factor induces correlations', 99)))
p2 <- ggplot(data, aes(x = time, y = y, col = series, fill = series,
                       ymin = ymin, ymax = ymax)) +
  geom_borderline(data = static, aes(x = static_time), 
                  linewidth = 1.5, bordercolour = "white") +
  geom_ribbon(alpha = 0.3, colour = NA) +
  geom_borderline(data = data %>%
                    dplyr::filter(series == 'factor'),
                  linewidth = 1.5, bordercolour = "white") +
  scale_color_manual(values = c("black",
                                "#8F2727",
                                "#C79999",
                                "#630000",
                                "#A25050")) +
  scale_fill_manual(values = c("black",
                               "#8F2727",
                               "#C79999",
                               "#630000",
                               "#A25050")) +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  ylab('Value') + xlab('Time') +
  labs(title = '{title_dat$title[as.integer(frame_along)]}')
p2

animate(
  p2 + transition_reveal(time) + 
    ease_aes('cubic-in-out'),
  nframes = 190, fps = 20, 
  end_pause = 30,
  start_pause = 60,
  device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)
anim_save(filename = 'day4/resources/df_with_series.gif')