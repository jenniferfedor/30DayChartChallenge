# 30 Day Chart Challenge
# Day 7: physical 
# Density plots of mountain elevations per continent

library(here)
library(rvest)
library(dplyr)
library(ggplot2)
library(ggridges)
library(showtext)
library(PNWColors)

showtext_auto()
font_add_google('Signika', family = 'signika')

# country to continent crosswalk via John Snow Labs (https://datahub.io/JohnSnowLabs/country-and-continent-codes-list)
crosswalk <- read.delim(here::here('07_physical', 'data', 'country-and-continent-codes-list-csv_csv.txt'),
                        header = TRUE, 
                        sep = ',')

# clean up the crosswalk data 
crosswalk <- crosswalk %>%
  rename_all(tolower) %>%
  mutate(country_name = gsub(country_name, pattern = ',.*', replacement = '') %>%
           gsub(., pattern = '\\(.*\\)', replacement = '') %>%
           gsub(., pattern = ' of .*', replacement = '') %>%
           gsub(., pattern = '[[:space:]]$|^[[:space:]]', replacement = '')) %>%
  select(continent_name, country_name)

# list of mountains and elevations via Wikipedia
wiki <- read_html('https://en.wikipedia.org/wiki/List_of_mountains_by_elevation')

# list of the tables containing the data
wikitables <- wiki %>%
  html_nodes('table') %>%
  html_table()

# initialize an empty dataframe
mountains <- data.frame(mountain = character(),
                        metres = double(),
                        location = character(),
                        stringsAsFactors = FALSE)

# combine the separate tables containing the data into a single dataframe
for (i in 1:length(wikitables)){
  temp <- wikitables[[i]] %>%
    rename_all(tolower) %>%
    mutate(metres = gsub(metres, pattern = ',', replacement = ''),
           metres = as.numeric(metres)) %>%
    rename('location' = 'location and notes') %>%
    select(mountain, metres, location)
  
  # append the above to the exisiting mountain data
  mountains <- bind_rows(mountains, temp)
}

# clean up the messy location column
mountains <- mountains %>%
  mutate(
    # replace any whitespace character with space, remove parentheticals and notes following the country name(s)
    location = gsub(location, pattern = '[[:space:]]', replacement = ' ') %>%
      gsub(., pattern = ' –.*| –.*|\\..*', replacement = '') %>%
      gsub(., pattern = '\\(.*\\)', replacement = '') %>%
      gsub(., pattern = '.* in ', replacement = '') %>%
         
      # if there is a list of locations(sep by comma, period, dashes, slashes), just take the first one
      gsub(., pattern = '^.*, ', replacement = '') %>%
      gsub(., pattern = '/.*', replacement = '') %>%
      gsub(., pattern = '-.*', replacement = '') %>%
      gsub(., pattern = ' and .*| \\& .*', replacement = '') %>%
         
      # rename for consistency within the dataset as well as with crosswalk below
      gsub(., pattern = '[Ww]estern|[Ee]astern|[Nn]orthern|[Ss]outhern', replacement = '') %>%
      gsub(., pattern = 'US|.*United States.*', replacement = 'United States') %>%
      gsub(., pattern = '.*Italy$|^Italy.*|.*Italian.*|.*Sicily|Marche|Emilia|Sardinia|liguria|Lombardy|Italy', replacement = 'Italy') %>%
      gsub(., pattern = 'Canadian.*|^B$|Yukon|Waddington Range', replacement = 'Canada') %>%
      gsub(., pattern = 'DRC|Democratic Republic of the Congo', replacement = 'Congo') %>%
      gsub(., pattern = 'Russia.*|Chukotka', replacement = 'Russian Federation') %>%
      gsub(., pattern = 'UK|England|British.*|Wales|Scotland', replacement = 'United Kingdom') %>%
      gsub(., pattern = '.*Korea', replacement = 'Korea') %>%
      gsub(., pattern = 'Bosnia|BiH|Mountain Dinara', replacement = 'Bosnia and Herzegovina') %>%
      gsub(., pattern = 'Trinidad', replacement = 'Trinidad and Tobago') %>%
      gsub(., pattern = '.*India$|^India.*|Nagaland|Uttarakhand', replacement = 'India') %>%
      gsub(., pattern = '.*Australia', replacement = 'Australia') %>%
      gsub(., pattern = '.*Swiss.*', replacement = 'Switzerland') %>%
      gsub(., pattern = 'Kyrgyzstan', replacement = 'Kyrgyz Republic') %>%
      gsub(., pattern = 'Afghanistan–Tajikistan', replacement = 'Afghanistan') %>%
      gsub(., pattern = 'Laos', replacement = "Lao People's Democratic Republic") %>%
      gsub(., pattern = 'Columbia', replacement = 'Colombia') %>%
      gsub(., pattern = '.*Chile', replacement = 'Chile') %>%
      gsub(., pattern = '.*China.*|Tibet|Karakoram|Kangri Garpo', replacement = 'China') %>%
      gsub(., pattern = 'Crna Gora', replacement = 'Montenegro') %>%
      gsub(., pattern = 'Libya', replacement = 'Libyan Arab Jamahiriya') %>%
      gsub(., pattern = 'North Macedonia', replacement = 'Macedonia') %>%
      gsub(., pattern = '.*Iran', replacement = 'Iran') %>%
      gsub(., pattern = 'South Ossetia or Georgia', replacement = 'Georgia') %>%
      gsub(., pattern = 'Pyrenees|Réunion', replacement = 'France') %>%
      gsub(., pattern = 'Muzaffarabad Azad Kashmir', replacement = 'Pakistan') %>%
      gsub(., pattern = 'East Timor', replacement = 'Timor-Leste') %>%
      gsub(., pattern = 'Syria', replacement = 'Syrian Arab Republic') %>%
      gsub(., pattern = 'Kosovo', replacement = 'Serbia') %>%
      gsub(., pattern = 'Victoria Land|Kerguelen Islands|South Shetland Islands|Australian Antarctic Territory', replacement = 'Antarctica') %>%
      gsub(., pattern = 'Galunggung', replacement = 'Indonesia') %>%
      gsub(., pattern = 'Roosevelt Range', replacement = 'Greenland') %>%
      gsub(., pattern = 'Saint Vincent', replacement = 'Saint Vincent and the Grenadines') %>%
      gsub(., pattern = 'West Bank', replacement = 'Israel') %>%
      gsub(., pattern = 'Saint Barthélemy', replacement = 'Saint Barthelemy') %>%
         
      # replace US state and location names with 'United States'
      gsub(., pattern = 'Alaska|California|Hawaii|Montana|Nevada|New Mexico|Texas|Utah|Wyoming', replacement = 'United States') %>%
      gsub(., pattern = 'Death Valley|North Cascades|Wrangell Mtns|Saint Elias Mountains|the Tushar Mountains|White Mtns', replacement = 'United States') %>%
         
      # finally, remove any remaining leading and trailing spaces
      gsub(., pattern = '[[:space:]]$|^[[:space:]]', replacement = '')
  )

# join mountain elevation data and crosswalk country to continent data
mountains <- mountains %>%
  left_join(crosswalk, by = c('location' = 'country_name')) %>%
  rename(c('continent' = 'continent_name', 'elevation_m' = 'metres')) %>%
  distinct()

# check which mountain locations didn't have a match in the crosswalk
mountains %>% filter(is.na(continent))

# manually assign the continent to the few mountains for which the country did not match any entry in the crosswalk
mountains[mountains$mountain == 'Ulugh Muztagh', 'continent'] <- 'Asia'
mountains[mountains$mountain == 'Mount Robson', 'continent'] <- 'North America'
mountains[mountains$mountain == 'Teide', 'continent'] <- 'Europe'
mountains[mountains$mountain == 'Thabana Ntlenyana', 'continent'] <- 'Africa'
mountains[mountains$mountain == 'Kendrick Peak', 'continent'] <- 'North America'
mountains[mountains$mountain == 'Galunggung', 'continent'] <- 'Asia'
mountains[mountains$mountain == 'Mount Mitchell', 'continent'] <- 'North America'
mountains[mountains$mountain == 'Monte Penice', 'continent'] <- 'Europe'
mountains[mountains$mountain == 'Mount Bates', 'continent'] <- 'Oceania'
mountains[mountains$mountain == 'Alto de Coloane', 'continent'] <- 'Asia'

save(mountains, file = here::here('07_physical', 'data', 'mountains.RData'))

# create a color palette for the plot
colors <- pnw_palette('Moth', 9)

pal <- colors[3:9]
background <- colors[1]
gridlines <- colors[2]
text <- colors[9]
title <- colors[4]

# create the plot
ggplot(data = mountains,
       aes(x = elevation_m, y = continent, fill = continent, color = continent)) +
  geom_density_ridges(alpha = 0.7,
                      quantile_lines = TRUE,
                      quantile_fun = median) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = c(0, 5000, 10000), 
                     labels = c('0 meters', '5,000 meters', '10,000 meters')) +
  coord_cartesian(clip = 'off') +
  labs(x = 'Elevation',
       title = 'Mountains of the world',
       subtitle = 'Distribution of mountain elevations (in meters) across continents. The median elevation is\nindicated by the solid vertical line on each density plot\n',
       caption = '\nData: Wikipedia & John Snow Labs') +
  theme(legend.position = 'none',
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = background, color = background),
        panel.background = element_rect(fill = background, color = background),
        #panel.grid.minor = element_line(color = '#615151', linetype = 'dotted'),
        #panel.grid.major = element_line(color = '#615151', linetype = 'dotted'),
        panel.grid = element_blank(),
        plot.title = element_text(color = title, size = 14, family = 'signika', face = 'bold'),
        plot.subtitle = element_text(color = text, size = 10, family = 'signika'),
        plot.caption = element_text(color = text, size = 7, family = 'signika'),
        axis.title.x = element_text(color = text, size = 10, family = 'signika', face = 'bold'),
        axis.text.x = element_text(color = text, size = 8, family = 'signika', vjust = 7),
        axis.text.y = element_text(color = pal, size = 10, family = 'signika', face = 'bold', vjust = 0))

ggsave(here::here('07_physical', 'physical.png'), width = 7, height = 5, units = 'in', dpi = 500)
