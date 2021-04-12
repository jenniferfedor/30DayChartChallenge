# 30 Day Chart Challenge
# Day 6: Experimental
# Sample size in human fMRI studies (n = 944)

library(here)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggtext)
library(ghibli)
library(showtext)
library(patchwork)

font_add_google('Roboto', family = 'roboto')
showtext_auto()

# data from Denes Szucs & John Ioannidis (Neuroimage, 2020; https://osf.io/g7zw3/)
# notes about the data: 
# 'tp' is sample size, 'ex' is number excluded, 'tpf' is sample for analysis (tp - ex)
# per the legend in the 2017 database, codes for fMRI studies end in 1

# read in data
studies_12 <- read_xlsx(here::here('06_experimental', 'data', 'FINAL database 29 Jul 2017.xlsx'), 
                        sheet = 'FINAL database 29 Jul 2017')

studies_17 <- read_xlsx(here::here('06_experimental', 'data', 'FINAL database 29 Jul 2017.xlsx'), 
                        sheet = '2017DATA')

studies_18 <- read_xlsx(here::here('06_experimental', 'data', 'Final 2018 DATABASE.xlsx'), 
                        sheet = 'Data2018')

# clean up 1990-2012 data
studies_12 <- studies_12 %>%
  rename_all(tolower) %>%
  mutate(first_author = gsub(authors, pattern = '\\s(.*)', replacement = ''),
         type1 = as.numeric(type1),
         tp = as.numeric(tp),
         ex = as.numeric(ex),
         tpf = as.numeric(tpf)) %>%
  select(first_author, sourcetitle, year, type1, tp, ex, tpf) %>%
  rename('journal' = 'sourcetitle') 

# clean up 2017 data
studies_17 <- studies_17 %>%
  rename_all(tolower) %>%
  mutate(year = as.character(fmri)) %>%
  select(fa, journal, year, type1, tp, ex, tpf) %>%
  rename('first_author' = 'fa') 

# clean up 2018 data
studies_18 <- studies_18 %>%
  rename_all(tolower) %>%
  mutate(year = gsub(as.character(issue), pattern = '-..-..', replacement = ''),
         tp = gsub(tp, pattern = ' \\(.*\\)', replacement = ''),
         tp = as.numeric(tp)) %>%
  select(first_author, journal, year, type1, tp, ex, tpf) 

# combine the 1990-2012, 2017, and 2018 dataframes and select fMRI studies
studies <- bind_rows(studies_12, studies_17, studies_18) %>%
  filter(year != 'NaN') %>%
  # just fMRI studies
  filter(grepl(type1, pattern = '.1'))
  
# get mean and median sample size per publication year
yearly_medians <- studies %>%
  group_by(year) %>%
  summarize(tp_med = median(tp, na.rm = TRUE),
            ex_med = median(ex, na.rm = TRUE),
            tpf_med = median(tpf, na.rm = TRUE),
            tp_mean = mean(tp, na.rm = TRUE),
            ex_mean = mean(ex, na.rm = TRUE),
            tpf_mean = mean(tpf, na.rm = TRUE)) %>%
  ungroup()

# color palette
textcol <- '#1d2f30'
jittercol <- '#6b8e65'
mediancol <- '#1d2f30'
gridcol <- '#d8d8d8'

# plot excluding 2017-2018
plot1 <- ggplot() +
  geom_jitter(data = studies %>% filter(!year %in% c('2017', '2018')),
              aes(x = year, y = tpf),
              color = jittercol,
              alpha = 0.6) +
  geom_point(data = yearly_medians %>% filter(!year %in% c('2017', '2018')),
             aes(x = year, y = tpf_med),
             color = mediancol,
             alpha = 0.8,
             size = 3) +
  geom_line(data = yearly_medians %>% filter(!year %in% c('2017', '2018')),
            aes(x = year, y = tpf_med, group = 1),
            color = mediancol) +
  scale_y_log10(limits = c(2, 10000)) +
  annotation_logticks(sides = 'l', 
                      outside = FALSE, 
                      short = unit(0.1, 'cm'),
                      mid = unit(0.1, 'cm'),
                      long = unit(0.2, 'cm'),
                      color = textcol) +
  labs(y = 'Sample size for analysis',
       x = '\nYear of publication') +
  theme(legend.position = 'none',
        axis.title.y = element_text(family = 'roboto', color = textcol),
        axis.text.y = element_text(family = 'roboto', color = textcol),
        axis.title.x = element_text(family = 'roboto', color = textcol),
        axis.text.x = element_text(family = 'roboto', color = textcol),
        axis.line = element_line(color = textcol),
        panel.grid.major = element_line(linetype = 'dashed', color = gridcol),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# plot for 2017 and 2018
plot2 <- ggplot() +
  geom_jitter(data = studies %>% filter(year %in% c('2017', '2018')),
              aes(x = year, y = tpf),
              color = jittercol,
              alpha = 0.6) +
  geom_point(data = yearly_medians %>% filter(year %in% c('2017', '2018')),
             aes(x = year, y = tpf_med),
             color = mediancol,
             alpha = 0.8,
             size = 3) +
  geom_line(data = yearly_medians %>% filter(year %in% c('2017', '2018')),
            aes(x = year, y = tpf_med, group = 1),
            color = mediancol) +
  scale_y_log10(limits = c(2, 10000)) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = 'roboto', color = textcol),
        axis.line.x = element_line(color = textcol),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(linetype = 'dashed', color = gridcol),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# combine the plots with patchwork
plot1 + plot2 + 
  plot_layout(widths = c(7, 1)) +
  plot_annotation(
    title = ' Sample size in fMRI studies',
    caption = '\nData: Szucs & Ioannidis (NeuroImage, 2020)',
    subtitle = 
    '  Science is facing a reproducibility crisis. This is particularly apparent in functional magnetic resonance imaging (fMRI) and other human neuroimaging
  studies, in which sample sizes are often constrained by the high cost and effort associated with acquiring these data; insufficient sample size reduces
  statistical power and may lead to overestimates of effect size.\n
  These data were assembled by Denes Szucs and John Ioannidis, who abstracted sample size from a total of 1308 highly-cited neuroimaging papers,
  including 861 fMRI papers, published between 1990-2012 and 2017-2018. The plot below visualizes historic trends in fMRI study sample size and
  compares them to that of more recent work. Although there are more studies in recent years with relatively large sample sizes, in part due to the rise
  of open data and data-sharing initiatives, the median study sample size has remained low over time.\n'
  ) &
  theme(plot.title = element_text(size = 14, face = 'bold', family = 'roboto', color = textcol),
        plot.subtitle = element_text(size = 10, family = 'roboto', color = textcol),
        plot.caption = element_text(family = 'roboto', color = textcol))

# Szucs, D., & Ioannidis, J. PA. (2020). Sample size evolution in neuroimaging research: An evaluation of highly-cited studies (1990–2012) and of latest practices (2017–2018) in high-impact journals. NeuroImage, 221, 117164. https://doi.org/10.1016/j.neuroimage.2020.117164



