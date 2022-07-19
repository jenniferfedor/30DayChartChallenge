# 30 Day Chart Challenge
# Day 4: Magical
# LOTR and Hobbit movies Academy Awards wins and Rotten Tomatoes ratings

library(here)
library(readr)
library(rvest)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gggibbous)
library(showtext)
library(ggtext)

font_add_google('Macondo', family = 'macondo')
font_add_google('Lato', family = 'lato')
showtext_auto()

# academy awards ----
# scrape academy awards data from the wiki tables
lotr_awards <- read_html('https://en.wikipedia.org/wiki/List_of_accolades_received_by_The_Lord_of_the_Rings_film_series') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[3:5]

hobbit_awards <- read_html('https://en.wikipedia.org/wiki/List_of_accolades_received_by_The_Hobbit_film_series') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[2:4]

# number of total nominations, losses, and wins per film 
awards <- bind_rows(lotr_awards, hobbit_awards, .id = 'film') %>%
  rename_all(tolower) %>%
  mutate(organization = gsub(organization, pattern = '\\[.*\\]', replacement = ''),
         film = factor(film, 
                       labels = c('The Fellowship of the Ring', 'The Two Towers', 'The Return of the King',
                                  'An Unexpected Journey', 'The Desolation of Smaug', 'The Battle of the Five Armies'))) %>%
  filter(organization == 'Academy Awards') %>%
  group_by(film) %>%
  summarize(nominations = n(),
            lost = sum(result == 'Nominated'),
            won = sum(result == 'Won'))

# rotten tomatoes ----
# scrape rotten tomatoes data from the wiki tables
lotr_ratings <- read_html('https://en.wikipedia.org/wiki/The_Lord_of_the_Rings_(film_series)') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[6]]

hobbit_ratings <- read_html('https://en.wikipedia.org/wiki/The_Hobbit_(film_series)') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[6]]

# rotten tomatoes rating per film
ratings <- bind_rows(lotr_ratings, hobbit_ratings) %>%
  rename_all(tolower) %>%
  mutate(film = gsub(film, pattern = 'The Hobbit: ', replacement = ''),
         film = factor(film, levels = levels(awards$film)),
         rating = gsub(`rotten tomatoes`, pattern = '%.*', replacement = ''),
         rating = as.numeric(rating))

# worldwide box office gross and budget per film ----
# scrape these data from the wiki tables
lotr_budget <- read_html('https://en.wikipedia.org/wiki/The_Lord_of_the_Rings_(film_series)') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[5]]

hobbit_budget <- read_html('https://en.wikipedia.org/wiki/The_Hobbit_(film_series)') %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[5]]

# the column names span multiple rows, so we'll fix this
collapse_col_names <- function(data, col_name_rows = 1){

  temp <- data
  col_names <- colnames(temp)
  
  for(i in 1:col_name_rows){
    for(j in 1:length(col_names)){
      col_names[j] <- ifelse(col_names[j] == lotr_money[i,j],
                             col_names[j],
                             paste(col_names[j], lotr_money[i, j]))
    }
  }
  
  colnames(temp) <- col_names
  temp <- temp[(1+col_name_rows):nrow(temp), ]
  return(temp)
}

# fix col names spanning multiple rows and clean up the two tables
lotr_budget <- collapse_col_names(lotr_budget, col_name_rows = 2) %>%
  rename_all(tolower) %>%
  select(film, `u.s. release date`, `box office gross worldwide worldwide`, budget) %>%
  filter(film != 'Total')

hobbit_budget <- collapse_col_names(hobbit_budget, col_name_rows = 1) %>%
  rename_all(tolower) %>%
  select(film, `release date u.s. release date`, `box office gross worldwide`, `budget u.s. and canada`) %>%
  filter(film != 'Total')

colnames(lotr_budget) <- colnames(hobbit_budget)

# combine the two tables
budgets <- bind_rows(lotr_budget, hobbit_budget)

budgets <- budgets %>%
  separate(`release date u.s. release date`, 
           into = c('release_day', 'release_month', 'release_year'), 
           sep = '[[:space:]]', 
           extra = 'drop',
           convert = TRUE) %>%
  mutate(release_date = ymd(sprintf('%s-%s-%s', release_year, release_month, release_day))) %>%
  mutate(box_office_gross_worldwide = gsub(`box office gross worldwide`, pattern = '\\$|,', replacement = '')) %>%
  mutate(box_office_gross_worldwide = as.numeric(box_office_gross_worldwide)) %>%
  select(film, release_year, release_date, box_office_gross_worldwide)

# adjust for inflation to 2021 USD
inflation <- tribble(
  # via https://www.officialdata.org/us/inflation/
  ~year, ~inflation_factor,
  2001, 1.54,
  2002, 1.52,
  2003, 1.48,
  2012, 1.19,
  2013, 1.17,
  2014, 1.15
)

budgets <- budgets %>%
  left_join(inflation, by = c('release_year' = 'year')) %>%
  mutate(box_office_gross_worldwide_2021usd = box_office_gross_worldwide*inflation_factor) %>%
  mutate(box_office_gross_worldwide_2021usd_billions = box_office_gross_worldwide_2021usd/1000000000) %>%
  mutate(film = gsub(film, pattern = 'The Hobbit: ', replacement = ''))


# join the awards and ratings data ----
awards_budgets <- awards %>%
  left_join(budgets %>% select(film, box_office_gross_worldwide_2021usd_billions), by = 'film')

# create the plot ----
# color palette created with https://mycolor.space/
colors <- c('#192029', '#f4eff9', '#897892', '#383C4B')

# plot
quartz()
ggplot() +
  geom_segment(data = awards_budgets,
               aes(x = film, xend = film, y = min(box_office_gross_worldwide_2021usd_billions)-1, yend = box_office_gross_worldwide_2021usd_billions),
               color = colors[2],
               size = 0.5) +
  geom_moon(data = awards_budgets,
            aes(x = film, y = box_office_gross_worldwide_2021usd_billions, ratio = won/nominations, size = nominations),
            right = TRUE,
            fill = colors[2],
            color = colors[2],
            stroke = 0.25) + 
  geom_moon(data = awards_budgets,
            aes(x = film, y = box_office_gross_worldwide_2021usd_billions, ratio = lost/nominations, size = nominations),
            right = FALSE,
            fill = colors[3],
            color = colors[2],
            stroke = 0.25) +
  scale_size(range = c(10, 30)) +
  scale_y_continuous(limits = c(0, 2)) +
  # replace second occurrence of a space in each string with a line break to 'wrap' the long x axis labels
  scale_x_discrete(labels = gsub(awards_ratings$film, pattern = '^([^ ]+[ ]+[^ ]+)[ ]', replacement = '\\1\n\\2')) +
  labs(x = '\n\nFilm',
       y = 'Box office gross, billions (2021 USD)\n',
       title = 'Critical acclaim and commercial success for the "Lord of the Rings" and "Hobbit" trilogies',
       subtitle = 
"<br>Each moon on the plot below displays the <span style = 'color:#f4eff9;'>proportion of Academy Award nominations won</span> per film, with the size of the moon
<br>reflecting the number of Academy Awards for which the film was nominated. The position of the moon on the y-axis reflects
<br>the film's box office gross worldwide, adjusted for inflation to 2021 US dollars",
       caption = 'Data: Wikipedia') +
  theme(legend.position = 'none',
        plot.title = element_text(family = 'macondo', size = 14, color = colors[2]),
        plot.subtitle = element_markdown(family = 'lato', size = 9.5, color = colors[3]),
        plot.caption = element_text(family = 'lato', size = 8, color = colors[3]),
        axis.text.y = element_text(family = 'lato', size = 9, color = colors[4]),
        axis.text.x = element_text(family = 'lato', size = 9, color = colors[2]),
        axis.title = element_text(family = 'lato', size = 10, color = colors[2]),
        plot.background = element_rect(fill = colors[1], color = colors[1]),
        panel.background = element_rect(fill = colors[1], color = colors[1]),
        panel.grid.major.y = element_line(color = colors[4], linetype = 'longdash'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank())

ggsave(here::here('04_magical', 'magical.png'), width = 8, height = 5, units = 'in', dpi = 500)
  