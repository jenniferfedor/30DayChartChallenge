# 30 Day Chart Challenge
# Day 2: Pictogram
# Pictogram plot of tree cover loss in the Midwest, 2001-2020

library(here)
library(dplyr)
library(ggplot2)
library(waffle)
library(showtext)
library(PNWColors)

showtext_auto()
font_add_google('Rubik', family = 'rubik')
font_add(family = 'FontAwesome5Free-Solid', regular = '/Library/Fonts/fa-solid-900.ttf')

# US tree cover loss data (in hectares/ha) via Global Forest Watch (globalforestwatch.org)
meta <- read.csv(here('02_pictogram', 'data', 'adm1_metadata.csv'), stringsAsFactors = FALSE)
treecoverloss <- read.csv(here('02_pictogram', 'data', 'treecover_loss_by_region__ha.csv'), stringsAsFactors = FALSE)

# list of midwestern states
midwest <- c('Illinois', 'Indiana', 'Iowa', 'Kansas', 'Michigan', 'Minnesota',
             'Missouri', 'Nebraska', 'North Dakota', 'Ohio', 'South Dakota', 'Wisconsin')

# for midwest states, calculate total tree cover loss between 2001 and 2020
trees <- treecoverloss %>%
  left_join(meta, by = c('adm1' = 'adm1__id')) %>%
  select(name, umd_tree_cover_loss__ha, umd_tree_cover_loss__year) %>%
  `colnames<-`(c('state', 'yearly_loss_ha', 'year')) %>%
  filter(state %in% midwest) %>%
  group_by(state) %>%
  summarize(total_loss_ha = sum(yearly_loss_ha),
            total_loss_kha = total_loss_ha/1000,
            total_loss_10kha = total_loss_kha/10) %>%
  ungroup() %>%
  mutate(state = reorder(as.factor(state), -total_loss_10kha))

# color palette by @awsmcolor on instagram
textcolor <- '#767000'
titlecolor <- '#f45744'
backgroundcolor <- '#f7f0d6'
iconcolor <- '#554f0e'

# create the pictogram plot
ggplot(data = trees) +
  geom_pictogram(aes(values = ceiling(total_loss_10kha), label = state),
                 color = iconcolor,
                 n_rows = 1,
                 size = 7) +
  scale_label_pictogram(name = 'null',values = 'tree') +
  geom_rect(aes(xmin = total_loss_10kha+0.5,
                xmax = total_loss_10kha+2.5,
                ymin = 0.75,
                ymax = 2),
            fill = backgroundcolor) +
  facet_wrap(~state, 
             scales = 'fixed', 
             nrow = 12,
             strip.position = 'left') +
  scale_x_continuous(limits = c(0, 70), expand = c(0, 0), breaks = c(20, 40, 60)) +
  ylim(c(0.75, 2)) +
  labs(title = 'Total tree cover loss in the Midwest, 2001-2020',
       subtitle = 'Each tree represents 10 kilohectares (about 25,000 acres) of tree cover lost due to human or natural causes\n',
       caption = '\nData: Global Forest Watch') +
  theme(legend.position = 'none',
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.line.x = element_line(color= textcolor),
        panel.background = element_rect(fill = backgroundcolor, color = backgroundcolor),
        plot.background = element_rect(fill = backgroundcolor, color = backgroundcolor),
        strip.text.y.left = element_text(family = 'rubik', face = 'bold', color = textcolor, size = 11, hjust = 1, angle = 0),
        plot.title = element_text(family = 'rubik', face = 'bold', color = titlecolor, size = 16, hjust = 0),
        plot.subtitle = element_text(family = 'rubik', color = textcolor, size = 11, hjust = 0),
        plot.caption = element_text(family = 'rubik', color = textcolor, size = 10, hjust = 1),
        axis.text.x = element_text(family = 'rubik', color = textcolor, size = 10),
        axis.ticks.x = element_line(color = textcolor))

ggsave(here('02_pictogram', 'pictogram.png'), width = 11.5, height = 7, units = 'in', dpi = 500)
