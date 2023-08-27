## Produces an animation showing how weighted basis functions combine to produce a spline
##  - this version is for a cubic regression spline basis
# Code slightly adapted from Gavin Simpson's original: 
# https://gist.github.com/gavinsimpson/727900bcd634fa530d2bd7316aa9d065

## packages required
library('ggplot2')
library('ggborderline')
library('tibble')
library('tidyr')
library('dplyr')
library('mgcv')
library('mvnfast')
library('purrr')
library('gganimate')
library('magick')
library('rsvg')
library('transformr')
library('viridis')

theme_set(theme_classic(base_size = 18, base_family = 'serif'))

##' wrapper around mvnfast::rmvn for coefs for basis functions
##'
##' @param n numeric, number of draws from MVN
##' @param k numeric, number of basis functions in basis
##' @param mu, sigma numeric vectors of means and **variances** for $\beta$
draw_beta <- function(n, k, mu = 1, sigma = 1) {
  mvnfast::rmvn(n = n, mu = rep(mu, k), sigma = diag(rep(sigma, k)))
}

##' Given a set of basis functions evaluated at `x`, weight them by coefs
##'
##' @param bf matrix od basis functions evaluated at `x`
##' @param x covariate values used to evaluated basis
##' @param n numeric, number of draws from MVN; probably should be 1
##' @param k, numeric; number of basis functions
##' @param ... additional arguments passed to `draw_beta`
weight_basis <- function(bf, x, n = 1, k, ...) {
  beta <- draw_beta(n = n, k = k, ...)
  out <- sweep(bf, 2L, beta, '*')
  colnames(out) <- paste0('f', seq_along(beta))
  out <- as_tibble(out)
  out <- add_column(out, x = x)
  out <- pivot_longer(out, -x, names_to = 'bf', values_to = 'y')
  out
}

##' Generate a set of draws from prior of a spline
##'
##' @param bf matrix of basis functions evaluated at `x`
##' @param x covariate values at which basis was evaluated
##' @param draws numeric; how many draws from the prior to make
##' @param k numeric; number of basis functions in basis
##' @param ... arguments passed to `weight_basis` and then on to `draw_beta`
random_bases <- function(bf, x, draws = 10, k, ...) {
  out <- rerun(draws, weight_basis(bf, x = x, k = k, ...))
  out <- bind_rows(out)
  out <- add_column(out, draw = rep(seq_len(draws), each = length(x) * k),
                    .before = 1L)
  class(out) <- c("random_bases", class(out))
  out
}

##' Plot method butchery for a set of draws
##'
##' Use `facet = FALSE` for animations
plot.random_bases <- function(x, facet = FALSE) {
  plt <- ggplot(x, aes(x = x, y = y, colour = bf)) +
    geom_line(lwd = 1) +
    guides(colour = FALSE)
  if (facet) {
    plt + facet_wrap(~ draw)
  }
  plt
}

## generate some outcome and covariate values to work with
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

## Generate the basis
k <- 12 # number of basis functions
## CRS needs knot locations given `k`, arrange evenly over `x`
knots <- with(data, list(x = seq(min(x), max(x), length = k)))
## generate a CRS basis using *mgcv* evaluated at `x`
sm <- smoothCon(s(x, k = k, bs = "cr"), data = data, knots = knots)[[1]]$X
colnames(sm) <- levs <- paste0("f", seq_len(k))
## convert to tidy form
basis <- pivot_longer(cbind(sm, data), -x, names_to = 'bf')
basis

## generate draws from basis functions
set.seed(10)
bfuns <- random_bases(sm, data$x, draws = 20, k = k)

## given draws from basis functions & random betas, summarise 
## to a spline for each draw
smooth <- bfuns %>%
  group_by(draw, x) %>%
  summarise(spline = sum(y)) %>%
  ungroup()

## animate the smooth on its own using the magick svg renderer
p1 <- ggplot(smooth) +
  geom_line(data = smooth, aes(x = x, y = spline), lwd = 2) +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  ylab('f(Time)') + xlab('Time') +
  ylim(c(min(data$y) - 0.1, max(smooth$spline) + 0.1))

animate(
  p1 + transition_states(draw, 
                        transition_length = 4, 
                        state_length = 2) + 
    ease_aes('cubic-in-out'),
  nframes = 200, device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)
anim_save(filename = 'day1/resources/smooth_only.gif')

## now animate the underlying basis functions that contribute
## to the smooth
p2 <- ggplot(bfuns, aes(x = x, y = y, col = bf)) +
  geom_borderline(linewidth=1.5, bordercolour = "white") +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  ylab('f(Time)') + xlab('Time') +
  geom_borderline(data = smooth, 
                  linewidth=2, bordercolour = "white",
                  aes(x = x, y = spline),
                  inherit.aes = FALSE) +
  ylim(c(min(data$y) - 0.1, max(smooth$spline) + 0.1))

animate(
  p2 + transition_states(draw, 
                        transition_length = 5, 
                        state_length = 2) + 
    ease_aes('cubic-in-out'),
  nframes = 200, device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)
anim_save(filename = 'day1/resources/basis_weights.gif')

## now show the observed data as points
sm2 <- smoothCon(s(x, k = k, bs = "cr"), data = data, knots = knots)[[1]]$X
beta <- coef(lm(y ~ sm2 - 1, data = data))
wtbasis <- sweep(sm2, 2L, beta, FUN = "*")
colnames(wtbasis) <- colnames(sm2) <- paste0("F", seq_len(k))

## create stacked unweighted and weighted basis
basis <- as_tibble(rbind(sm2, wtbasis)) %>%
  add_column(x = rep(data$x, times = 2),
             type = rep(c('unweighted', 'weighted'), each = nrow(sm2)),
             .before = 1L)
##data <- cbind(data, fitted = rowSums(scbasis))
wtbasis <- as_tibble(rbind(sm2, wtbasis)) %>%
  add_column(x      = rep(data$x, times = 2),
             fitted = rowSums(.),
             type   = rep(c('unweighted', 'weighted'), each = nrow(sm2))) %>%
  pivot_longer(-(x:type), names_to = 'bf')
basis <- pivot_longer(basis, -(x:type), names_to = 'bf')
basis %>%
  dplyr::mutate(value = dplyr::case_when(
    type == 'unweighted' ~ 0,
    TRUE ~ value
  )) -> basis
wtbasis %>%
  dplyr::mutate(fitted = dplyr::case_when(
    type == 'unweighted' ~ 0,
    TRUE ~ fitted
  )) -> wtbasis

p3 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(alpha = 0.4) +
  theme(legend.position = 'none',
        axis.line = element_line(size = 1),
        axis.ticks = element_line(colour = "black", size = 1)) +
  ylab('Y') + xlab('Time') +
  geom_borderline(data = basis, 
                  linewidth=1.5, bordercolour = "white",
                  aes(x = x, y = value, col = bf),
                  inherit.aes = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  geom_borderline(data = wtbasis, 
                  linewidth=2, bordercolour = "white",
                  aes(x = x, y = fitted),
                  inherit.aes = FALSE) +
  ylim(c(min(data$y) - 0.1, max(smooth$spline) + 0.1))

animate(
  p3 + transition_states(type, 
                         transition_length = 4, 
                         state_length = 2) + 
    ease_aes('cubic-in-out'),
  nframes = 80, device = 'svg', renderer = magick_renderer(),
  width = 8, height = 5)
anim_save(filename = 'day1/resources/smooth_to_data.gif')
