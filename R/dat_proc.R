library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)
library(raster)

##
# SMC boundaries
prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
sheds <- readOGR('ignore/SMCSheds2009.shp') %>% 
  spTransform(CRS(prstr))

##
# import environmental data and scores for OE, MMI, clip all by SMC boundaries

# all data
fls <- list.files('ignore', pattern = '\\.csv$', full.names = TRUE) %>% 
  tibble(fl = .) %>% 
  mutate(
    data = map(fl, read.csv, stringsAsFactors = FALSE)
  )

# environmental data
envdat <- fls %>% 
  filter(grepl('algSiteData', fl)) %>% 
  unnest %>% 
  rename(site = SampleID_old)

# latlon, clip by sheds
latlon <- envdat %>% 
  dplyr::select(site, New_Lat, New_Long) %>% 
  unique %>% 
  na.omit
coordinates(latlon) <- latlon[, c('New_Long', 'New_Lat')]
crs(latlon) <- CRS(prstr)
latlon <- raster::intersect(latlon, sheds) %>% 
  data.frame %>% 
  dplyr::select(site, New_Lat, New_Long)

# filter envdat by latlon
envdat <- envdat %>%  
  filter(site %in% latlon$site)

# OE data, filter by sites in latlon
oedat <- fls %>% 
  filter(grepl('Scores', fl)) %>% 
  unnest %>% 
  mutate(
    fl = gsub('^ignore/|\\.Scores.*$', '', fl),
    Type = factor(Type, levels = c('ref', 'int', 'str'))
  ) %>% 
  rename(site = X) %>% 
  dplyr::select(fl, site, O, E, OoverE) %>% 
  left_join(latlon, ., by = 'site')

# MMI data
mmdat <- fls %>% 
  filter(grepl('metrics', fl)) %>% 
  unnest %>% 
  mutate(
    fl = gsub('^ignore/|\\.combined.*$', '', fl), 
    Type = factor(Type, levels = c('ref', 'int', 'str'))
  ) %>% 
  rename(site = X) %>% 
  left_join(latlon, ., by = 'site') %>% 
  na.omit

save(mmdat, file = 'data/mmdat.RData', compress = 'xz')
save(envdat, file = 'data/envdat.RData', compress = 'xz')
save(oedat, file = 'data/oedat.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')

