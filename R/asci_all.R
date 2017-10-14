# libraries
library(tidyverse)
library(broom)
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gridExtra)

##
# PSA boundaries
prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
psa <- readOGR('S:/Spatial_Data/RCMP_needs editting/Inputs/PSA6_090111/PSA6_2011.shp') %>% 
  spTransform(CRS(prstr))
save(psa, file = 'data/psa.RData', compress = 'xz')

##
# import scores for OE, MMI

# all data
fls <- list.files('ignore', pattern = '\\.csv$', full.names = TRUE) %>% 
  tibble(fl = .) %>% 
  mutate(
    data = map(fl, read.csv, stringsAsFactors = FALSE)
  )

# environmental data
evdat <- fls %>% 
  filter(grepl('algSiteData', fl)) %>% 
  unnest %>% 
  rename(id = SampleID_old) %>% 
  mutate(site = gsub('_[0-9]+\\..*$', '', id))

# latlon, clip by sheds
latlon <- evdat %>% 
  dplyr::select(site, New_Lat, New_Long) %>% 
  unique %>% 
  na.omit
coordinates(latlon) <- latlon[, c('New_Long', 'New_Lat')]
crs(latlon) <- CRS(prstr)
latlon <- raster::intersect(latlon, psa) %>%
  data.frame %>%
  dplyr::select(site, New_Lat, New_Long)

# OE data, filter by sites in latlon
oedat <- fls %>% 
  filter(grepl('Scores', fl)) %>% 
  unnest %>% 
  filter(!Type %in% 'notrecent') %>%
  rename(
    id = X,
    scr = OoverE
  ) %>% 
  mutate(
    fl = gsub('^ignore/|\\.Scores.*$', '', fl),
    type = factor(Type),
    type = fct_collapse(type, ref = c('rc', 'rv')),
    ind = 'oe', 
    site = gsub('_[0-9]+\\..*$', '', id)
  ) %>% 
  dplyr::select(fl, site, id, type, ind, scr) %>% 
  inner_join(latlon, ., by = 'site') 

# MMI data
mmdat <- fls %>% 
  filter(grepl('metrics', fl)) %>% 
  unnest %>% 
  rename(
    id = X,
    scr = Means
  ) %>% 
  mutate(
    fl = gsub('^ignore/|\\.combined.*$', '', fl), 
    type = factor(Type, levels = c('ref', 'int', 'str')), 
    ind = 'mmi', 
    site = gsub('_[0-9]+\\..*$', '', id)
  ) %>% 
  dplyr::select(fl, site, id, type, ind, scr) %>% 
  inner_join(latlon, ., by = 'site') 

# combine mmdat, oedat
indat <- rbind(mmdat, oedat)

##
# add tenth percentile thresholds
# from https://drive.google.com/open?id=0B60qUcYQTz_dVy11RGM2V21rV1E
# IndexPerformance tab

# thresholds
thrsh <- tibble(
  fl = c('diatoms', 'sba', 'hybrid', 'diatoms', 'sba', 'hybrid'),
  ind = c('oe', 'oe', 'oe', 'mmi', 'mmi', 'mmi'),
  thr = c(0.77, 0.51, 0.69, 0.75, 0.80, 0.78)
)

# join w/ indat, create categories
indat <- indat %>% 
  left_join(thrsh, by = c('ind', 'fl')) %>% 
  mutate(
    scrcat = ifelse(scr < thr, 'blw', 'abv')
  )

# add chdat to indat, final formatting
indat <- indat %>% 
  rename(
    lat = New_Lat, 
    lon = New_Long, 
    tax = fl
  ) %>% 
  mutate(
    tax = factor(tax, 
                 levels = c('diatoms', 'sba', 'hybrid'), 
                 labels = c('Diatom', 'Soft-bodied', 'Hybrid')
    ), 
    ind = factor(ind,
                 levels = c('oe', 'mmi'), 
                 labels = c('O/E', 'pMMI')
    ), 
    type = factor(type, 
                  levels = c('ref', 'int', 'str'),
                  labels = c('Reference', 'Intermediate', 'Stressed')
    ), 
    scrcat = factor(scrcat, 
                    levels = c('abv', 'blw'), 
                    labels = c('Hi', 'Lo')
    )
  ) %>% 
  dplyr::select(id, site, lon, lat, type, ind, tax, scr, thr, scrcat)

# add psa region names to indat
coordinates(indat) <- indat[, c('lon', 'lat')]
crs(indat) <- CRS(prstr)
indat <- raster::intersect(indat, psa) %>%
  data.frame %>%
  dplyr::select(id, site, lon, lat, type, ind, tax, scr, thr, scrcat, PSA6)



toplo <- indat %>% 
  filter(type %in% 'Reference')

p1 <- ggplot(toplo, aes(x = PSA6, y = scr)) + 
  geom_boxplot(outlier.size = 1, alpha = 0.7, fill = 'lightblue') + 
  geom_hline(aes(yintercept = thr), linetype = 'dashed') + 
  facet_grid(ind ~ tax) +
  scale_y_continuous('Index Score') + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_blank(), 
    panel.border = element_rect(colour = 'black', fill = NA, size = 0.5), 
    legend.position = 'none'
  ) 

p2 <- ggplot(toplo, aes(x = PSA6, y = scr)) + 
  geom_boxplot(outlier.size = 1, alpha = 0.7, fill = 'lightblue') + 
  facet_grid(ind ~ tax) +
  scale_y_continuous('Index Score') + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.title.x = element_blank(), 
    panel.border = element_rect(colour = 'black', fill = NA, size = 0.5), 
    legend.position = 'none'
  ) 

png('figs/ref1.png', height = 5, width = 9, units = 'in', res = 300, family = 'serif')
p1
dev.off()


png('figs/ref2.png', height = 5, width = 9, units = 'in',  res = 300, family = 'serif')
p2
dev.off()

