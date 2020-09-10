# SET UP  #####################################################################
library(dplyr)
library(tidyr)
library(highcharter)
library(ggplot2)
library(tidyr)
library(dplyr)

# DATA  #######################################################################
# get some data about countries
countries_data <- readr::read_csv('f:/country_data.csv')

# normalise and shape it for the chart
countries_norm <- countries_data %>% 
  gather(metric, val, -c(Country, Region)) %>% 
  group_by(metric) %>% 
  mutate(norm_val = (val-min(val, na.rm = T)) / (max(val, na.rm = T) - min(val, na.rm = T)),
         yval = match(metric, names(countries_data)[c(-1, -2)])) %>% 
  arrange(metric, desc(norm_val)) %>% 
  ungroup()

# GGPLOT  #####################################################################
ggplot() +
  geom_point(aes(norm_val, yval), countries_norm,
             alpha = 0.3) + 
  scale_y_continuous(labels = unique(countries_norm$metric),
                     breaks = 1:length(unique(countries_norm$metric))) + 
  geom_hline(yintercept = seq(1.5, 
                              length(unique(countries_norm$metric)) - 0.5, 
                              1), colour = 'white') + 
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  geom_path(aes(norm_val, yval, colour = Country),
            subset(countries_norm, Country %in% c('Liberia', 'Somalia')) %>%
              arrange(yval),
            lwd = 1)

# HIGHCHARTER  ################################################################
highchart() %>% 
  hc_add_series(data = countries_norm, hcaes(norm_val, y))

hchart(countries_norm, 'scatter', hcaes(norm_val, y, group = Region))