# 30 Day Chart Challenge
# Day 8: animals
# Distribution of median lifespan of purebred dog breeds

library(here)
library(rvest) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
library(showtext)

# import the data ---------------------------------------------------------------------------------------------------
# data: adams, evans, sampson & wood (jsap, 2010)
adams_et_al <- read_html('https://onlinelibrary.wiley.com/doi/full/10.1111/j.1748-5827.2010.00974.x')

# scrape the table containing the lifespan data per breed
lifespans <- adams_et_al %>%
  html_nodes('table') %>%
  html_table(fill = TRUE, header = FALSE) %>%
  .[[1]] 

# scrape the table containing the common causes of death across breeds
cods <- adams_et_al %>%
  html_nodes('table') %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[[4]] 

# scrape the table containing causes of death per breed
breed_cods <- adams_et_al %>%
  html_nodes('table') %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[[5]]

# tidying -----------------------------------------------------------------------------------------------------------
# there is a separate row for grand totals in the lifespan data, so we'll save this elsewhere
totals <- lifespans[nrow(lifespans), ]
colnames(totals) <- lifespans[2, ]

# there are two "header" rows in the lifespan data, so we'll fix this
colnames(lifespans) <- lifespans[2, ]
lifespans <- lifespans[4:nrow(lifespans)-1, ]

# clean up weird unicode characters and convert min/median/max columns to numeric    
lifespans <- lifespans %>%
  rename_all(tolower) %>%
  mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  # NA's introduced by coercion below, which is okay
  mutate(minimum = as.numeric(minimum),
         median = as.numeric(median),
         maximum = as.numeric(maximum)) %>%
  filter(!is.na(median)) %>%
  select(`breed name`, minimum, median, maximum)

# tidy cause of death data
cods <- cods %>%
  rename_all(tolower) %>%
  rename('percent' = '%') %>%
  mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  mutate(percent = as.numeric(percent))

# common (at least >4% of deaths), known, single CODs
single_cods <- cods %>%
  filter(!`cause of death` %in% (c('Unknown', 'Combinations', 'Subtotal', 'Total'))) %>%
  filter(percent > 4) %>%
  mutate(`cause of death` = gsub(`cause of death`, pattern = '\\*|"|+', replacement = ''),
         `cause of death` = gsub(`cause of death`, pattern = 'Cardiac.*', replacement = 'Cardiac'))

# removing the unicode characters replaced this with NA, so add it back
single_cods[6, 1] <- 'Gastrointestinal'
single_cods[2, 1] <- 'Age'

# tidy most common COD per breed data
colnames(breed_cods) <- c('Breed', paste(breed_cods[1, 2:ncol(breed_cods)], breed_cods[2, 2:ncol(breed_cods)]))

breed_cods <- breed_cods[3:nrow(breed_cods), ] %>%
  select(Breed, ends_with('%')) %>%
  mutate_all(~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>%
  # convert percent of deaths per cause columns from character to numeric
  mutate_at(vars(matches('%')), as.numeric) %>%
  rename_all(tolower)

breed_common_cod <- breed_cods %>%
  pivot_longer(!breed, 
               names_to = 'cause',
               values_to = 'percent') %>%
  mutate(cause = gsub(cause, pattern = ' %', replacement = '')) %>%
  group_by(breed) %>%
  # select the most common COD per breed
  filter(percent == max(percent)) %>%
  mutate(cause = case_when(cause == 'old age' ~ 'Age',
                           cause == 'cancer' ~ 'Cancer',
                           cause == 'cardiac' ~ 'Cardiac')) %>%
  select(-percent)

# join the lifespan and most common COD per breed data
lifespans_cods <- lifespans %>%
  left_join(breed_common_cod, by = c('breed name' = 'breed')) 
  

# create labels for interesting data points -------------------------------------------------------------------------
# which breeds have the longest and shortest median life expectancies?
longest <- lifespans %>% 
  select(`breed name`, median) %>% 
  arrange(-median) %>% 
  head(1)

shortest <- lifespans %>% 
  select(`breed name`, median) %>% 
  arrange(-median) %>% 
  tail(2)

labels <- bind_rows(longest, shortest)
labels$x <- c(0.25, 0.3, 0.3)
labels$offset <- c(1, 1, -1)

# plot --------------------------------------------------------------------------------------------------------------
# create the plot
ggplot() +
  geom_beeswarm(data = lifespans,
                aes(x = 0, y = median),
                cex = 4,
                size = 3) +
  geom_text(data = labels,
            aes(x = x,
                y = median + offset,
                label = sprintf('%s (%s years)', `breed name`, median)),
            size = 3) +
  scale_x_continuous(limits = c(-0.5, 0.5)) + 
  scale_y_continuous(limits = c(-2, 18)) +
  labs(title = 'Life expectancy and most common cause of death among a sample of purebred dogs in the UK',
       y = 'Median lifespan (years)',
       x = '',
       caption = 'Data: Adams, Evans, Sampson & Wood (2010)') +
  theme_bw()

ggplot() +
  geom_bar(data = single_cods,
           aes(x = reorder(`cause of death`, percent),
               y = percent,
               fill = `cause of death`),
           stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  labs(title = 'Common single, known causes of death among purebred dog breeds in the UK',
       y = 'Percent of all deaths',
       x = '') +
  theme_bw() +
  theme(legend.position = 'none')






