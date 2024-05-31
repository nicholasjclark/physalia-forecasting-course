library(dplyr)
library(readxl)
library(ggplot2); theme_set(theme_bw())

# Read in the mosquito data
mos_dat <- read_xlsx('mosquitos.xlsx') %>%
  dplyr::mutate(series = as.factor(Lugar),
                month = Mes) %>%
  dplyr::group_by(series) %>%
  dplyr::arrange(year, month) %>%
  dplyr::ungroup()

# Ensure all series have an observation for all possible timepoints;
# unfortunately this will result in missing values for covariates as well,
# so mvgam may not be an option here
all_times <- expand.grid(year = seq(min(mos_dat$year),
                                    max(mos_dat$year)),
                         month = 1:12,
                         series = unique(mos_dat$series))

mos_dat %>%
  dplyr::select(-Time) %>%
  dplyr::right_join(all_times) %>%
  dplyr::group_by(series) %>%
  dplyr::arrange(year, month) %>%
  dplyr::mutate(time = dplyr::row_number()) %>%
  dplyr::ungroup() -> mos_dat

# Number of missing values for each region
mos_dat %>%
  dplyr::filter(is.na(Nhembras)) %>%
  dplyr::group_by(series) %>%
  dplyr::tally()

# Plot the region-level time series
ggplot(mos_dat, aes(time, Nhembras, col = series)) +
  geom_point() +
  facet_wrap(~series, scales = 'free_y') +
  theme(legend.position = 'none')

# Look at region-level histograms
ggplot(mos_dat, aes(Nhembras, fill = series)) +
  geom_histogram(col = 'white') +
  facet_wrap(~series, scales = 'free_x') +
  theme(legend.position = 'none')

# Some very big differences in counts here, hopefully some of the covariates
# can help explain these 