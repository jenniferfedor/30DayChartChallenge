# 30 Day Chart Challenge
# Day 3: Historical
# Recreating Florence Nightingale's rose diagram from 1858
# Diagram of causes of mortality during the Crimean war

library(here)
library(tidyr) 
library(dplyr)
library(ggplot2)
library(showtext)
library(HistData)

showtext_auto()
font_add_google('Smythe', family = 'smythe')
font_add_google('IM Fell English SC', family = 'imfell')

# data originally collected and reported by Florence Nightingale, via HistData package
data('Nightingale')

# prep the data to plot
night <- Nightingale %>%
  rename_all(tolower) %>%
  filter(date < as.Date('1855-04-01')) %>%
  select(month, year, wounds, other, disease) %>%
  pivot_longer(cols = c(disease, wounds, other),
               names_to = 'cause',
               values_to = 'deaths') %>%
  mutate(month_year = paste0(as.character(month), ' ', as.character(year)),
         month_year = factor(month_year, ordered = TRUE,
                             levels = c('Apr 1854', 'May 1854', 'Jun 1854', 'Jul 1854', 'Aug 1854', 'Sep 1854',
                                        'Oct 1854', 'Nov 1854', 'Dec 1854', 'Jan 1855', 'Feb 1855', 'Mar 1855')),
         deaths_to_plot = sqrt(deaths/pi),
         cause = factor(cause, ordered = TRUE, levels = c('disease', 'wounds', 'other'))) %>%
  arrange(-deaths_to_plot)

# create labels for the plot
labels <- night %>%
  group_by(month_year) %>%
  filter(deaths_to_plot == max(deaths_to_plot)) %>%
  select(month_year, deaths_to_plot) %>%
  mutate(label = tolower(case_when(month_year == 'Apr 1854' ~ 'Apr. 1854',
                                   month_year == 'Jan 1855' ~ 'Jan. 1855',
                                   month_year == 'May 1854' ~ 'May',
                                   TRUE ~ gsub(month_year, pattern = ' 185[4|5]$', replacement = '.'))),
         offset = case_when(label == 'apr. 1854' ~ 7,
                            label == 'may' ~ 6.5,
                            label == 'jun.' ~ 7,
                            TRUE ~ 2.5))

# create the plot
ggplot() +
  geom_bar(data = night,
           aes(x = month_year, y = deaths_to_plot, fill = cause),
           stat = 'identity',
           position = position_identity(),
           alpha = 1,
           width = 1,
           size = 0.2,
           color = 'black') +
  scale_fill_manual(values = c('#8cc0d1', '#e0a4a4', '#636060')) +
  coord_polar(start = -pi/2, 
              clip = 'off') +
  geom_text(aes(x = 3.5, y = 28.5, 
                label = 'the number deaths from preventable or mitigable zymotic\ndiseases, wounds, and all other causes among british soliders\nduring the crimean war, april 1854 - march 1855'),
            family = 'imfell',
            hjust = 0.5,
            size = 3.5) +
  geom_text(data = labels,
            aes(x = month_year, y = deaths_to_plot + offset, label = label),
            family = 'imfell',
            size = 2.5) +
  labs(title = 'Diagram of the Causes of Mortality\nIn the Army in the East',
       caption = '\n\nrecreation of the original rose diagram by florence nightingale, 1858\ndata via {histdata}') +
  theme_void() +
  theme(plot.title = element_text(family = 'smythe', hjust = 0.5, size = 20),
        plot.caption = element_text(family = 'imfell', hjust = 0.5, size = 8),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(family = 'imfell', size = 9),
        plot.background = element_rect(fill = '#faf9f0', color = '#faf9f0'),
        panel.background = element_rect(fill = '#faf9f0', color = '#faf9f0'))

ggsave(here('03_historical', 'historical.png'), width = 4.28, height = 5.77, units = 'in', dpi = 500)
