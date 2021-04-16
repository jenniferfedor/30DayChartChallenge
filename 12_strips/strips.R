# 30 Day Chart Challenge
# Day 12: Strips
# Strip chart of daily precipitation by month in Pittsburgh

library(here)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggtext)
library(showtext)
library(PNWColors)

font_add(family = 'FontAwesome5Free-Solid', regular = '/Library/Fonts/fa-solid-900.ttf')
font_add_google('Cabin Sketch', family = 'cabin')
font_add_google('Roboto', family = 'roboto')
showtext_auto()

# 2020 GCHN daily data for station #US1PAAL0074 in Pittsburgh via NOAA (https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND)
precip <- read_csv(here::here('12_strips', 'data', '2546784.csv'))

# station location
station_lat <- unique(precip$LATITUDE)
station_long <- unique(precip$LONGITUDE)

# prep data to plot
precip <- precip %>%
  rename_all(tolower) %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  select(date, month, prcp) 

# monthly summaries
precip %>%
  group_by(month) %>%
  summarize(total_precip = sum(prcp),
            avg_precip = mean(prcp),
            max_precip = max(prcp),
            rainy_days = sum(prcp != 0))

# which day in 2020 was the rainiest?
precip %>%
  filter(prcp == max(prcp))

# color palette
backgroundcol <- pnw_palette('Winter', 8)[8]
textcol <- pnw_palette('Winter', 8)[1]
fillcol <- pnw_palette('Winter', 8)[4]

# plot
ggplot() +
  geom_jitter(data = precip,
              aes(x = month, y = prcp),
              size = 2.5,
              alpha = 0.4,
              fill = fillcol,
              color = fillcol,
              position = position_jitter(width = 0.2, height = 0.05, seed = 1)) +
  geom_text(aes(x = 9, y = 2.68, label = 'It rained 2.95" on August 29th!'),
            size = 3,
            hjust = 0,
            family = 'roboto',
            color = textcol) +
  geom_curve(aes(x = 8.9, xend = 8.1, y = 2.7, yend = 2.85), 
             curvature = 0.2,
             size = 0.25,
             color = textcol,
             arrow = arrow(length = unit(0.015, 'npc'))) +
  # hacky x tick labels: first plot cloud shapes, then add the month labels over top
  geom_richtext(aes(x = 1:length(levels(precip$month)), 
                    y = rep(-0.15, length(levels(precip$month))), 
                    label = rep("<span style='font-size:38px; font-family: \"FontAwesome5Free-Solid\"'> &#61634;</span>", length(levels(precip$month)))),
                color = textcol,
                fill = NA, 
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), 'pt')) +
  geom_richtext(aes(x = 1:length(levels(precip$month)), 
                    y = rep(-0.2, length(levels(precip$month))), 
                    label = sprintf("<span style='font-size:18px; font-family:cabin'; font-weight:bold>%s</span>", levels(precip$month))),
                color = backgroundcol,
                fill = NA, 
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), 'pt')) +
  scale_y_reverse() +
  scale_x_discrete(position = 'top') +
  labs(x = "",
       y = "Precipitation (inches)\n",
       title = "\nIt's a rainy day in the neighborhood",
       subtitle = "Daily precipitation in Pittsburgh, PA, 2020\n",
       caption = "\nData: NOAA") +
  theme(panel.background = element_rect(fill = backgroundcol, color = backgroundcol),
        plot.background = element_rect(fill = backgroundcol, color = backgroundcol),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x.top = element_blank(),
        panel.grid.major.y = element_line(color = textcol, linetype = 'dotted'),
        plot.title = element_text(family = 'cabin', hjust = 0.5, color = fillcol, size = 21, face = 'bold'),
        plot.subtitle = element_text(family = 'cabin', hjust = 0.5, color = fillcol, size = 15),
        plot.caption = element_text(family = 'cabin', hjust = 0.5, color = textcol, size = 11),
        axis.title.y = element_text(family = 'cabin', size = 14, color = textcol),
        axis.text.y = element_text(family = 'roboto', color = textcol, size = 11),
        plot.margin = margin(t = 0, r = 30, b = 5, l = 10, unit = "pt"))

ggsave(here::here('12_strips', 'strips.png'), width = 7.4, height = 6.8, units = 'in', dpi = 500)
