# 30 Day Chart Challenge
# Day 8: Animals
# Distributions of selling prices for bugs, fish, and sea creatures in ACNH

library(here)
library(rvest)
library(dplyr)
library(ggplot2)
library(showtext)

showtext_auto()
font_add_google('Fresca', family = 'fresca')
font_add_google('Gorditas', family = 'gorditas')

# data via Animal Crossing Wiki
fish_url <- read_html('https://animalcrossing.fandom.com/wiki/Fish_(New_Horizons)#Northern_Hemisphere')
bugs_url <- read_html('https://animalcrossing.fandom.com/wiki/Bugs_(New_Horizons)#Northern_Hemisphere')
seacreatures_url <- read_html('https://animalcrossing.fandom.com/wiki/Deep-sea_creatures_(New_Horizons)#Northern_Hemisphere')

# scrape the table containing fish selling prices
fish <- fish_url %>%
  html_nodes('table') %>%
  .[[3]] %>%
  html_table(fill = TRUE) %>%
  rename_all(tolower) %>%
  select(name, price, location) %>%
  mutate(price = gsub(price, pattern = ',', replacement = ''),
         price = as.numeric(price),
         type = 'Fish')

# scrape the table containing bug selling prices
bugs <- bugs_url %>%
  html_nodes('table') %>%
  .[[3]] %>%
  html_table(fill = TRUE) %>%
  rename_all(tolower) %>%
  select(name, price, location) %>%
  mutate(price = gsub(price, pattern = ',', replacement = ''),
         price = as.numeric(price),
         type = 'Bugs')

# scrape the table containing sea creature selling prices
seacreatures <- seacreatures_url %>%
  html_nodes('table') %>%
  .[[5]] %>%
  html_table(fill = TRUE) %>%
  rename_all(tolower) %>%
  select(name, price) %>%
  mutate(price = gsub(price, pattern = ',', replacement = ''),
         price = as.numeric(price),
         location = 'Deep Sea',
         type = 'Sea creatures')

# combine the fish, bug, and sea creature data into a single dataframe
critters <- bind_rows(fish, bugs, seacreatures) %>%
  mutate(type = as.factor(type))

save(critters, file = here::here('08_animals', 'data', 'critters.RData'))

# most expensive critters per type
critters %>%
  group_by(type) %>%
  filter(price == max(price)) %>%
  arrange(price, type)

# create the plot
pal <- c('#a4d4a2', '#ff7c69', '#8ecfca')

ggplot(data = critters,
       aes(x = price, y = type, fill = type, color = type)) +
  geom_jitter(alpha = 0.5, size = 2, height = 0.2) +
  geom_boxplot(alpha = 0.2, outlier.shape = NA) +
  scale_x_continuous(breaks = c(0, 5000, 10000, 15000), labels = c('0', '5,000', '10,000', '15,000')) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(x = '\nSelling price (Bells)',
       title = 'In-game selling prices of  "Animal Crossing: New Horizons" critters',
       subtitle = "\nIf you're looking to quickly pay off your mortgage to Tom Nook, focus on catching fish! Although the median selling\nprice of sea creatures is slightly higher, there are six species of fish that will fetch you top Bells compared to only\none such species of sea creature. Bugs have the lowest median selling price, and the maximum selling price is\nsubstantially less than that of the other critter types\n",
       caption = '\nData: Animal Crossing Wiki') +
  theme(legend.position = 'none',
        plot.title = element_text(family = 'gorditas', size = 12, face = 'bold', hjust = 0.5, color = 'white'),
        plot.subtitle = element_text(family = 'fresca', size = 9, hjust = 0.5, color = 'white'),
        plot.caption = element_text(family = 'fresca', size = 8, hjust = 0.5, color = 'white'),
        axis.title.x = element_text(family = 'fresca', size = 9, color = 'white'),
        axis.title.y = element_blank(),
        axis.text.x = element_text(family = 'fresca', size = 8, color = 'white'),
        axis.text.y = element_text(family = 'gorditas', size = 10, color = pal),
        panel.grid.minor = element_line(color = 'grey40', linetype = 'dotted'),
        panel.grid.major = element_line(color = 'grey40', linetype = 'dotted'),
        #plot.margin = unit(c(1, 1, 1.5, 1.2),'pt'),
        plot.background = element_rect(fill = 'grey20', color = 'grey20'),
        panel.background = element_rect(fill = 'grey20', color = 'grey20'))

ggsave(here::here('08_animals', 'animals.png'), width = 8, height = 4.5, units = 'in', dpi = 500)
