# 30 Day Chart Challenge
# Day 1: Part to whole
# Waffle plot of waffle recipe ingredients

library(rvest)
library(tidyr)
library(dplyr)
library(ggplot2)
library(waffle)
library(showtext)
library(wesanderson)

showtext_auto()
font_add_google('Reenie Beanie', family = 'reenie')
font_add_google('Fredericka the Great', family = 'fredericka')

# recipe via NYT Cooking
recipe <- read_html('https://cooking.nytimes.com/recipes/1017409-waffles')

# scrape the list of ingredients and measurements
ingredients <- recipe %>% 
  html_nodes('li') %>% 
  html_text() %>%
  .[3:11]

# clean up the list
ingredients <- ingredients %>%
  gsub(., pattern = '[[:space:]]+', replacement = ' ') %>% 
  gsub(., pattern = '^ | $', replacement = '') %>% 
  gsub(., pattern = '½', replacement = '0.5') %>% 
  gsub(., pattern = '/.* grams', replacement = '') %>% 
  gsub(., pattern = ' \\(.*|,.*', replacement = '') 

# convert the list into a dataframe
ingredients <- ingredients %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(., ingredient_string = `.`) %>%
  separate(ingredient_string, 
           into = c('measurement', 'units', 'ingredient'), 
           sep = ' ', 
           extra = 'merge',
           convert = TRUE) %>%
  mutate(units = gsub(units, pattern = 's$', replacement = '')) %>%
  select(ingredient, everything())

# measurement conversions to teaspoons via Google
conversions <- tribble(
  ~units, ~tsp,
  'teaspoon', 1,
  'tablespoon', 3,
  'cup', 48,
  'large', 9 # (large egg)
)

# convert the measurements to a common unit (half teaspoon)
ingredients <- ingredients %>%
  left_join(conversions, by = 'units') %>%
  mutate(half_tsp = measurement*tsp*2) %>%
  arrange(desc(half_tsp), ingredient) %>%
  mutate(ingredient = factor(ingredient))

save(ingredients, file = 'ingredients.RData')

# create labels and arrows for the ingredients on the plot
coords <- tribble(
  ~x, ~y, ~xstart, ~xend, ~ystart, ~yend, ~curvature,
  25.25, 5.5, 25.5, 24.5, 6, 7, 0.5,
  25.25, 9.5, 25.5, 24.5, 10, 11, 0.5,
  25.25, 13.5, 25.5, 24.5, 14, 15, 0.5,
  25.25, 16.5, 25.5, 24.5, 17, 18, 0.5,
  1.25, 22.5, 6, 7, 22.5, 21.5, -0.5,
  10.25, 22.5, 12, 13, 22.5, 21.5, -0.5,
  13.75, 22.5, 18, 19, 22.5, 21.5, -0.5,
  22.25, 23, 22, 21, 23, 21.5, 0.5,
  25, 20.5, 25.5, 23.5, 21, 21.5, 0.5
) 

labels <- coords %>% 
  cbind(., as.character(ingredients$ingredient)) %>%
  rename('label' = `as.character(ingredients$ingredient)`) 

# create a color palette for the plot
cols <- c(wes_palette('Royal1'), wes_palette('Royal2'))
pal <- cols[c(5, 3, 8, 9, 1, 6, 4, 7, 2)]

# create the plot
ggplot() +
  geom_waffle(data = ingredients,
              aes(fill = ingredient, values = half_tsp),
              color = '#2D2D2D',
              size = 0.33,
              n_rows = 24,
              flip = TRUE) +
  scale_fill_manual(values = pal) +
  geom_text(data = labels,
            aes(x = x, y = y, label = label, color = label), 
            hjust = 0, 
            size = 4,
            family = 'reenie') +
  geom_curve(data = labels %>% filter(curvature == 0.5),
             aes(x = xstart, xend = xend, y = ystart, yend = yend, color = label),
             curvature = 0.5,
             size = 0.25, 
             arrow = arrow(length = unit(0.01, 'npc'))) +
  geom_curve(data = labels %>% filter(curvature == -0.5),
             aes(x = xstart, xend = xend, y = ystart, yend = yend, color = label),
             curvature = -0.5,
             size = 0.25, 
             arrow = arrow(length = unit(0.01, 'npc'))) +
  scale_color_manual(values = c(pal, '#2D2D2D')) +
  labs(title = 'there are "a waffle" lot of ingredients in waffle batter',
       subtitle = '1 square is ½ teaspoon\n',
       caption = 'recipe via NYT Cooking ') +
  xlim(c(0, 30)) +
  coord_equal() +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, family = 'fredericka', size = 16, color = 'white'),
        plot.subtitle = element_text(hjust = 0.5, family = 'reenie', size = 13, color = 'white'),
        plot.caption = element_text(hjust = 1, family = 'reenie', size = 10, color = 'white'),
        panel.background = element_rect(fill = '#2D2D2D', color = '#2D2D2D'),
        plot.background = element_rect(fill = '#2D2D2D'),
        legend.position = 'none')

ggsave('part_to_whole.png', width = 5.835, height = 5.3, units = 'in', dpi = 500)
