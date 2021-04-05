# 30 Day Chart Challenge
# Day 5: Slope
# Line plot of change in dog breed popularity from 2013 to 2020 based on AKC rankings

library(here)
library(rvest)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(showtext)
library(PNWColors)

showtext_auto()
font_add_google('Oswald', family = 'oswald')
font_add_google('Lilita One', family = 'lilita')

# colors from PNWColors package
backgroundcol <- pnw_palette('Cascades')[1]
increasecol <- pnw_palette('Cascades')[3]
decreasecol <- pnw_palette('Cascades')[6]

# data via American Kennel Club rankings
akc2013 <- read_html('https://www.akc.org/most-popular-breeds/2013-full-list/')
akc2020 <- read_html('https://www.akc.org/expert-advice/dog-breeds/the-most-popular-dog-breeds-of-2020/')

# 2013 breed rankings
breeds2013 <- akc2013 %>%
  html_nodes('table') %>%
  html_table(header = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename_all(tolower) %>%
  rename('rank_2013' = 'rank') %>%
  mutate(breed = gsub(breed, pattern = '[[:space:]]', replacement = ' '))

# 2020 breed rankings
breeds2020 <- akc2020 %>%
  html_nodes('table') %>%
  html_table(header = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename_all(tolower) %>%
  rename('rank_2020' = 'x2020.rank') %>%
  mutate(breed = gsub(breed, pattern = '[[:space:]]', replacement = ' '))

# join the two dataframes and calculate change in rank for each breed
rankings <- breeds2013 %>%
  full_join(breeds2020, by = 'breed') %>%
  # move parenthetical substrings in breed names to beginning of the string and clean up
  mutate(breed = gsub(breed, pattern = '(.*)(\\(.*\\))', replacement = '\\2 \\1')) %>%
  mutate(breed = gsub(breed, pattern = '\\(|\\)', replacement = '')) %>%
  mutate(breed = gsub(breed, pattern = ' $', replacement = '')) %>%
  # calc change, absolute change, and assign color based on direction of change
  mutate(change = rank_2013 - rank_2020,
         abs_change = abs(change),
         color = case_when(change < 0 ~ decreasecol, 
                           change > 0 ~ increasecol, 
                           change == 0 ~ 'white'))

# select the 10 breeds that had the largest change and convert dataframe to long format for plotting 
top10 <- rankings %>%
  arrange(-abs_change) %>%
  head(10) %>%
  select(breed, rank_2013, rank_2020, color) %>%
  pivot_longer(c(rank_2013, rank_2020),
               names_to = 'year',
               names_prefix = 'rank_',
               values_to = 'rank') %>%
  mutate(year = as.numeric(year))
  
# create a palette with line and point colors assigned to each breed
pal <- top10 %>%
  filter(year == 2020) %>%
  arrange(breed) %>%
  select(color) %>%
  as.list() %>%
  .[[1]]

# create the plot
ggplot() +
  geom_line(data = top10, 
            aes(x = year, y = rank, color = breed),
            size = 1.5) +
  geom_point(data = top10, 
             aes(x = year, y = rank, color = breed),
             size = 3) +
  geom_text_repel(data = top10 %>% filter(year == 2020),
                  aes(x = year+0.1, y = rank, label = breed, color = breed),
                  family = 'oswald',
                  direction = 'y',
                  nudge_x = 0.2,
                  min.segment.length = 1,
                  hjust = 0,
                  size = 3.5) +
  scale_color_manual(values = pal) +
  scale_y_reverse(limits = c(195, 65), 
                  breaks = c(175, 150, 125, 100, 75)) +
  scale_x_continuous(limits = c(2013, 2023), 
                     breaks = c(2013, 2020)) +
  labs(title = 'Pup-ularity contest',
       subtitle = "<span style = 'color:#FFFFFF;'>Which ten dog breeds had the largest absolute changes in popularity ranking<br>(</span><span style = 'color:#dec000;'>increase</span><span style = 'color:#FFFFFF;'> or </span><span style = 'color:#88a2b9;'>decrease</span><span style = 'color:#FFFFFF;'>) from 2013 to 2020?</span><br><br>",
       caption = '\n\nData: American Kennel Club',
       y = 'Rank') +
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(family = 'lilita', color = 'white', size = 24, hjust = 0),
        plot.subtitle = element_markdown(family = 'oswald', size = 12, hjust = 0),
        plot.caption = element_text(family = 'oswald', color = 'white', size = 10),
        axis.title.y = element_text(family = 'oswald', color = 'white', size = 10, vjust = 0.5),
        axis.text.x = element_text(family = 'oswald', color = 'white', size = 10),
        axis.text.y = element_text(family = 'oswald', color = 'white'),
        plot.background = element_rect(fill = backgroundcol, color = backgroundcol),
        panel.background = element_rect(fill = backgroundcol, color = backgroundcol))

ggsave(here('05_slope', 'slope.png'), width = 6, height = 8, units = 'in', dpi = 500)
