# 30 Day Chart Challenge
# Day 4: Magical
# Dialogue in Harry Potter films

library(here)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gggibbous)
library(wesanderson)
library(showtext)

font_add_google('Alice', family = 'alice', db_cache = FALSE)
showtext_auto()

# list of csv files, one per movie
files <- list.files(path = here::here('04_magical', 'data'), pattern = '.csv$')
files <- paste0('04_magical/data/', files)

# read in and merge the files
dialogue <- files %>%
  lapply(., read_delim, delim = ';', skip = 1, col_names = c('character', 'sentence')) %>%
  bind_rows(., .id = 'film') %>%
  mutate(character = str_to_title(character),
         character = str_trim(character),
         film_name = case_when(film == 1 ~ "Sorcerer's Stone",
                               film == 2 ~ "Chamber of Secrets",
                               film == 3 ~ "Prisoner of Azkaban"))

sort(unique(dialogue$character))

# correct misspellings in the character names
dialogue <- dialogue %>%
  mutate(character = 
           gsub(character, pattern = 'Hermoine', replacement = 'Hermione') %>%
           gsub(., pattern = 'Oiiver', replacement = 'Oliver') %>%
           gsub(., pattern = 'Mcgonagall', replacement = 'McGonagall'))

# list of top 10 characters with most dialogue across all three films
top_5_characters <- dialogue %>%
  group_by(character) %>%
  summarize(lines = n()) %>%
  arrange(-lines) %>%
  head(5) %>%
  pull(character)

# lines of dialogue per film
lines_per_film <- dialogue %>%
  mutate(film = as.factor(film)) %>%
  group_by(film) %>%
  summarize(total_lines = n())

dialogue_per_top_10 <- dialogue %>% 
  filter(character %in% top_5_characters) %>%
  mutate(character = factor(character, levels = rev(top_5_characters), ordered = TRUE),
         film = factor(film)) %>%
  group_by(character, film, .drop = FALSE) %>%
  summarize(lines = n()) %>%
  left_join(lines_per_film, by = 'film') %>%
  mutate(prop = lines/total_lines)

# color palette
dark_moon_col <- wes_palette('IsleofDogs1')[1]
light_moon_col <- wes_palette('IsleofDogs1')[3]
background_col <- wes_palette('IsleofDogs1')[4]
text_col <- 'white' #wes_palette('IsleofDogs1')[6]

# plot
ggplot(data = dialogue_per_top_10,
       aes(x = film,
           y = character)) +
  geom_moon(aes(ratio = prop),
            size = 30,
            fill = light_moon_col,
            color = light_moon_col,
            right = TRUE) +
  geom_moon(aes(ratio = 1-prop),
            size = 30,
            fill = dark_moon_col,
            color = dark_moon_col,
            right = FALSE) +
  scale_x_discrete(position = 'top', labels = c("Sorcerer's Stone\n", "Chamber of Secrets\n", "Prisoner of Azkaban\n")) +
  labs(title = 'Dialogue in Harry Potter',
       subtitle = 'Proportion of lines spoken by characters with the most dialogue in each of the first three films\n\n',
       caption = '\n\nData: Gulsah Demiryurek via Kaggle') +
  theme(panel.background = element_rect(color = background_col, fill = background_col),
        panel.grid = element_blank(),
        plot.background = element_rect(color = background_col, fill = background_col),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family = 'alice', color = text_col, size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = 'alice', color = text_col, size = 14, vjust = 2),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'alice', color = dark_moon_col, size = 20),
        plot.subtitle = element_text(hjust = 0.5, family = 'alice', color = text_col, size = 10),
        plot.caption = element_text(hjust = 0.5, family = 'alice', color = text_col, size = 9))

