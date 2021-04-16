# 30 Day Chart Challenge
# Day 4: Magical
# Word clouds of most common spells in each HP book

library(here)
library(rvest)
library(dplyr)
library(ggplot2)
library(harrypotter)

# list of spells
url <- read_html('https://www.pojo.com/harry-potter-spell-list/')

# scrape the list from the url and clean up
spells <- url %>%
  html_nodes('table') %>%
  html_table(header = TRUE) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  filter(Type == 'Spell') %>%
  mutate(Incantation = tolower(Incantation)) %>%
  select(Incantation) %>%
  as.list() %>%
  .[[1]]

# some of the spells are multi-word and thus won't be detected
# we can match any words that appear in a spell instead, although we will get some false hits
spellwords <- spells %>%
  strsplit(., split = ' ') %>%
  unlist()

# remove super common words
spellwords <- spellwords[!spellwords %in% c('to', 'ears', 'duo', 'pack', 'spell', 'flames', 'bolt')]

# list of the books
books <- c('philosophers_stone', 'chamber_of_secrets', 'prisoner_of_azkaban', 'goblet_of_fire',
           'order_of_the_phoenix', 'half_blood_prince', 'deathly_hallows')

# initialize an empty dataframe
hpwords <- data.frame(word = character(), book = integer())

# for loop
for (i in 1:length(books)){
  
  temp <- get(books[i]) %>%
    toString(.) %>%
    tolower(.) %>%
    gsub(., pattern = '[[:punct:]]', replacement = '') %>%
    gsub(., pattern = '[[:space:]]', replacement = ' ') %>%
    strsplit(., split = '[[:space:]]') %>%
    .[[1]] %>%
    as.data.frame(stringsAsFactors = FALSE, header = FALSE) %>%
    rename(., 'word' = '.') %>%
    mutate(., book = i) %>%
    filter(., word %in% spellwords)
  
  hpwords <- rbind(hpwords, temp)
  
}


