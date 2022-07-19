# 30 Day Chart Challenge
# Day 14: space
# Spatial distribution of voters in the 2020 US presidential election

#### packages ----

library(tidyverse)
library(tigris)
library(sf)
library(patchwork)
library(showtext)
library(ggtext)

#### fonts ----

font_add_google('Orelega One', family = 'orelega')
font_add_google('Roboto Serif', family = 'roboto-serif')
showtext_auto()

#### import data ----

# state boundaries from the US Census
us_states <- tigris::states(cb = TRUE)

# county boundaries from the US Census
us_counties <- tigris::counties(cb = TRUE)

# 2020 county population estimates from the US Census
county_pops <- read.csv(
  url('https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020.csv'),
  stringsAsFactors = FALSE
)

# 2020 US presedential election results by county via Tony McGovern
election_results <- read.csv(
  url('https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv'),
  stringsAsFactors = FALSE)


#### prep data ----

# state geometries
us_states <- us_states %>%
  rename_all(tolower) %>%
  filter(as.numeric(statefp) <= 56 & !(as.numeric(statefp) %in% c(2, 15)))

# county geometries
us_counties <- us_counties %>%
  rename_all(tolower) %>%
  select(statefp, countyfp, geoid, geometry)

# county centroids
us_county_centroids <- us_counties %>%
  st_centroid() %>%
  rename(centroid = geometry)

# 2020 county population estimates
county_pops <- county_pops %>%
  rename_all(tolower) %>%
  select(state, county, stname, ctyname, popestimate2020) %>%
  # county = 0 are statewide totals
  filter(county != 0) %>%
  # reconstruct state-county ID variable
  mutate(
    state = case_when(
      nchar(state) == 1 ~ paste0('0', state),
      TRUE ~ as.character(state)
    ),
    county = case_when(
      nchar(county) == 1 ~ paste0('00', county),
      nchar(county) == 2 ~ paste0('0', county),
      TRUE ~ as.character(county)
    ),
    county_fips = paste0(state, county)
  )

# 2020 county election results
election_results <- election_results %>%
  select(state_name, county_fips, county_name, votes_gop, votes_dem) %>%
  # create variable for winning candidate 
  mutate(
    winning_candidate = case_when(
      votes_gop > votes_dem ~ 'Republican',
      votes_gop < votes_dem ~ 'Democrat',
      votes_gop == votes_dem ~ 'Tie'
    )
  ) %>%
  # make state-county ID consistent with other datasets
  mutate(
    county_fips = case_when(
      nchar(county_fips) == 4 ~ paste0('0', county_fips),
      TRUE ~ as.character(county_fips)
    )
  )


#### join data ----

all_data <- us_counties %>%
  # for mapping, we are interested in the contiguous US only
  filter(as.numeric(statefp) <= 56 & !(as.numeric(statefp) %in% c(2, 15))) %>%
  rename(county_fips = geoid) %>%
  select(county_fips, geometry) %>%
  # join centroids
  left_join(
    us_county_centroids %>%
      as.data.frame() %>%
      rename(county_fips = geoid) %>%
      select(county_fips, centroid),
    by = 'county_fips'
  ) %>%
  # join election results
  left_join(
    election_results %>%
      select(county_fips, votes_gop, votes_dem, winning_candidate),
    by = 'county_fips'
  ) %>%
  # join pop estimates
  left_join(
    county_pops %>%
      select(county_fips, popestimate2020),
    by = 'county_fips'
  )

  
#### summary stats for the contiguous US ----

summ_stats <- all_data %>%
  as.data.frame() %>%
  select(county_fips, winning_candidate, popestimate2020) %>%
  group_by(winning_candidate) %>%
  summarize(
    n_counties = n(),
    total_pop = sum(popestimate2020)
  ) %>%
  mutate(
    county_diff = abs(diff(n_counties, lag = 1)),
    pop_diff_millions = abs(diff(total_pop, lag = 1))/1000000
  )

summ_stats

#### create the plot ----

# map 1: choropleth
map1 <- ggplot() +
  geom_sf(
    data = all_data,
    aes(
      geometry = geometry,
      fill = winning_candidate
    ),
    alpha = 0.7,
    color = 'black'
  ) +
  scale_fill_manual(values = c('blue', 'red')) +
  theme_void() +
  theme(
    legend.position = 'none'
  )


# map 2: dot plot
map2 <- ggplot() +
  geom_sf(
    data = us_states,
    aes(geometry = geometry),
    fill = 'white',
    color = 'black'
  ) +
  geom_sf(
    data = all_data,
    aes(
      geometry = centroid,
      color = winning_candidate,
      size = popestimate2020
    ),
    alpha = 0.4
  ) +
  scale_color_manual(values = c('blue', 'red')) +
  scale_size_continuous(range = c(1, 20)) +
  theme_void() +
  theme(
    legend.position = 'none'
  )

maps <- map1 / map2 +
  plot_annotation(
    title = 'Land does not vote.',
    caption = '\nCounty-level election results via Tony McGovern  |  Population estimates via US Census Bureau',
    subtitle = sprintf(
      'Spatial distribution of county-level 2020 presidential election results in the contiguous United States.<br>
      Although the <span style = "color:#FF0000;">Republican</span> candidate won the majority of votes in %s more counties than did the<br>
      <span style = "color:#0000FF;">Democrat</span> candidate (top), the counties in which the Democrat candidate won represent about %s million<br> 
      more people than do those in which the Republican candidate won (bottom).<br>', 
      scales::comma(summ_stats %>% pull(county_diff) %>% .[1]),
      round(summ_stats %>% pull(pop_diff_millions) %>% .[1], 1)
    ),
    theme = theme(
      plot.title = element_text(family = 'orelega', size = 40),
      plot.subtitle = element_markdown(family = 'roboto-serif'),
      plot.caption = element_text(family = 'roboto-serif', hjust = 0.5)
    )
  )
  
ggsave(plot = maps, filename = here::here('14_space/14_space.png'), dpi = 2000)




ggplot() +
  geom_histogram(aes(x = rnorm(100))) +
  labs(
    title = 'Land does not vote.',
    caption = '\nCounty-level election results via Tony McGovern  |  Population estimates via US Census Bureau',
    subtitle = sprintf(
      'Spatial distribution of county-level 2020 presidential election results in the contiguous United States.<br>
      Although the <span style = "color:#FF0000;">Republican</span> candidate won the majority of votes in %s more counties than did the<br>
      <span style = "color:#0000FF;">Democrat</span> candidate (top), the counties in which the Democrat candidate won represent about %s million<br> 
      more people than do those in which the Republican candidate won (bottom).<br>', 
      scales::comma(summ_stats %>% pull(county_diff) %>% .[1]),
      round(summ_stats %>% pull(pop_diff_millions) %>% .[1], 1)
    )
  ) +
  theme_void() +
  theme(
    plot.title = element_text(family = 'orelega', size = 40),
    plot.subtitle = element_markdown(family = 'roboto-serif'),
    plot.caption = element_text(family = 'roboto-serif', hjust = 0.5)
  )



