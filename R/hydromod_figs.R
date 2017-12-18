library(tidyverse)
library(lubridate)
library(forcats)
library(broom)

load(file = 'data/sheds.RData')
hydro <- readxl::read_excel('ignore/Hydromod 2015 2016_Susceptibility by land use.xlsx')

##
# hydromod sum

hydro_all <- hydro %>% 
  rename(
    Vertical = VertSuscept,
    Lateral = AverageLatSuscept
  ) %>% 
  gather('var', 'val', Vertical, Lateral) %>% 
  dplyr::select(StationCode, SampleDate, var, val) %>% 
  group_by(var, val) %>% 
  summarise(
    n = length(val)
  ) %>% 
  ungroup %>% 
  mutate(
    val = tolower(val),
    val = factor(val, levels = c('low', 'medium', 'high', 'very high')), 
    var = factor(var, levels = c('Vertical', 'Lateral'))
  ) %>% 
  rename(
    Susceptibility = val
  ) %>% 
  mutate(lu = 'all') %>% 
  select(var, Susceptibility, lu, n)

hydro_lu <- hydro %>% 
  rename(
    Vertical = VertSuscept,
    Lateral = AverageLatSuscept
  ) %>% 
  gather('var', 'val', Vertical, Lateral) %>% 
  dplyr::select(StationCode, SampleDate, SMC_LU, var, val) %>%
  mutate(
    lu = fct_recode(SMC_LU, ag = 'Agricultural', urban = 'Urban', other = 'NR', other = 'Open', other = 'SMC_out')
  ) %>% 
  group_by(var, val, lu) %>% 
  summarise(
    n = length(val)
  ) %>% 
  ungroup %>% 
  mutate(
    val = tolower(val),
    val = factor(val, levels = c('low', 'medium', 'high', 'very high')), 
    var = factor(var, levels = c('Vertical', 'Lateral'))
  ) %>% 
  rename(
    Susceptibility = val
  )

toplo <- rbind(hydro_all, hydro_lu) %>% 
  mutate(
    lu = factor(lu, levels = c('all', 'ag', 'urban', 'other'), labels = c('All', 'Ag', 'Urban', 'Other'))
  )

cols <- RColorBrewer::brewer.pal(4, 'RdYlGn') %>% rev
p <- ggplot(toplo, aes(x = lu, y = n, group = Susceptibility, fill = Susceptibility)) + 
  geom_bar(stat = 'identity', position = 'fill', colour = 'black', alpha = 0.8, width = 0.75) + 
  scale_y_continuous('Proportion of sites', expand = c(0, 0)) +
  scale_fill_manual(values = cols) +
  facet_wrap(~ var, ncol = 2) +
  scale_x_discrete('Land use') +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.border = element_rect(colour = 'black', fill = NA, size = 0.5), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.position = 'top'
  ) 

tiff('figs/hydro_sum.tif', height = 4, width = 7, units = 'in', compression = 'lzw', res = 300, family = 'serif')
p
dev.off()

##
# map

key <- sheds@data %>% 
  rownames_to_column('id') %>% 
  dplyr::select(id, SMC_Name)
shd <- tidy(sheds) %>% 
  left_join(key, by = 'id')

stat <- read.csv('ignore/luStation.txt') %>% 
  select(StationID, Latitude, Longitude) %>% 
  rename(StationCode = StationID) %>% 
  mutate(StationCode = as.character(StationCode))

toplo <- hydro %>% 
  rename(
    Vertical = VertSuscept,
    Lateral = AverageLatSuscept
  ) %>% 
  gather('var', 'val', Vertical, Lateral) %>% 
  dplyr::select(StationCode, SampleDate, var, val) %>% 
  mutate(
    val = tolower(val),
    val = factor(val, levels = c('low', 'medium', 'high', 'very high')), 
    var = factor(var, levels = c('Vertical', 'Lateral'))
  ) %>% 
  rename(
    Susceptibility = val
  ) %>% 
  mutate(yr = year(SampleDate)) %>% 
  left_join(stat, by = 'StationCode')

p <- ggplot(toplo) + 
  geom_polygon(data = shd, 
               aes(x = long, y = lat, group = group), 
               colour = 'gray', fill = 'lightgrey', alpha = 0.8
  ) + 
  geom_point(aes(x = Longitude, y = Latitude, fill = Susceptibility), colour = 'black', pch = 21, alpha = 0.7, size = 2) +
  facet_grid(yr ~ var) + 
  coord_map() + 
  scale_fill_manual(values = cols) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_blank(), 
    axis.title.x = element_blank(),
    panel.border = element_rect(colour = 'black', fill = NA, size = 0.5),  
    legend.position = 'top'
  )

tiff('figs/hydro_map.tif', height = 5, width = 5, units = 'in', compression = 'lzw', res = 300, family = 'serif')
p
dev.off()
