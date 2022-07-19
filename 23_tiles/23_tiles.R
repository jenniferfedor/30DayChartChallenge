# 30 Day Chart Challenge
# Day 23: tiles
# Monthly global temperature anomalies relative to 20th century average, 2000-2021

#### set up ----

library(tidyverse)
library(PNWColors)
library(showtext)

font_add_google('Josefin Sans', family = 'josefin-sans')
showtext_auto()

#### import data ----

# data source: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series
# global times series, land and ocean temperature anomalies, monthly, 2000-2021
# note: global anomalies are with respect to the 20th century average

# read in data, skipping first 4 rows which contain metadata
anomalies_raw <- read.csv(here::here('23_tiles/data/global_temperature_anomaly_data_2000_2021.csv'), skip = 4)
                           
#### prep data ----

# light cleaning
anomalies <- anomalies_raw %>%
  rename_all(tolower) %>%
  # there is a single column for year/month in format e.g., 200001, so we'll separate that into two columns
  mutate(month = as.integer(gsub(year, pattern = '^[2][0][0-2][0-9]', replacement = ''))) %>%
  mutate(month_lab = lubridate::month(month, label = TRUE)) %>%
  mutate(year = gsub(year, pattern = '[0-2][0-9]$', replacement = '')) %>%
  mutate(year = as.factor(year)) 

# max and min temp anomaly values, degrees celsius
max_val <-max(anomalies$value)
min_val <- min(anomalies$value)

# number of unique temp anomaly values in the data
n_vals <- length(unique(anomalies$value))
  
#### plot ----

# color palette
pal <- pnw_palette('Shuksan', n_vals) 
text_color <- pal[1]

# create the plot
anomalies %>%
  ggplot() +
  geom_tile(aes(x = month_lab, y = year, fill = value)) +
  # reverse y axis so it's in descending order
  scale_y_discrete(limits = rev, breaks = seq(2000, 2020, by = 5)) +
  scale_x_discrete(position = 'top') +
  scale_fill_gradientn(
    colors = pal, 
    breaks = c(min_val, max_val),
    # hacky way to increase spacing around legend labels so they won't overlap with the colorbar when we change parameters for guide_colorbar() below
    labels = c(
      paste0(min_val, '°C', strrep(' ', 11)),
      paste0(strrep(' ', 9), max_val, '°C')
    )
  ) +
  labs(
    title = 'the planet\'s heating up',
    subtitle = 'monthly global land and surface temperature anomalies relative to 20th century average, 2000-2021',
    caption = '\nData: NOAA National Centers for Environmental Information (ncei.noaa.gov)'
  ) +
  guides(
    fill = guide_colorbar(
      ticks = FALSE,
      label.position = 'top',
      # setting a negative value for vjust when legend position = 'top' moves the labels down so they appear on either side of the colorbar (with our spacing above)
      label.vjust = -7,
      label.theme = element_text(color = text_color, family = 'josefin-sans')
    )
  ) +
  theme_void() +
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box.margin = margin(0, 0, 20, 0),
    legend.title = element_blank(),
    axis.text.x = element_text(color = text_color, family = 'josefin-sans'),
    axis.text.y = element_text(color = text_color, family = 'josefin-sans'),
    plot.title = element_text(color = text_color, hjust = 0.5, family = 'josefin-sans', face = 'bold', size = 30),
    plot.subtitle = element_text(color = text_color, hjust = 0.5, family = 'josefin-sans'),
    plot.caption = element_text(color = text_color, hjust = 0.5, family = 'josefin-sans'),
    plot.margin = margin(0, 20, 0, 0)
  )

# save the plot
ggsave(here::here('23_tiles/23_tiles.png'), height = 10, width = 8, units = 'in', dpi = 2000)
