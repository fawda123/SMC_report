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
evdat <- fls %>% 
  filter(grepl('algSiteData', fl)) %>% 
  unnest %>% 
  rename(site = SampleID_old)

# latlon, clip by sheds
latlon <- evdat %>% 
  dplyr::select(site, New_Lat, New_Long) %>% 
  unique %>% 
  na.omit
coordinates(latlon) <- latlon[, c('New_Long', 'New_Lat')]
crs(latlon) <- CRS(prstr)
latlon <- raster::intersect(latlon, sheds) %>% 
  data.frame %>% 
  dplyr::select(site, New_Lat, New_Long)

# filter evdat by latlon
evdat <- evdat %>%  
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

##
# get channel type

# site/key lookup
kydat <- read.csv('ignore/luStation.txt', stringsAsFactors = FALSE) %>% 
  dplyr::select(StationID, MasterID) %>% 
  rename(
    sitetmp = MasterID, 
    stationcode = StationID
    )

# rafi's channel data
chdat <- read.csv('ignore/csci.alg.df.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(StationCode, ChannelClass_3hf) %>%
  rename(
    chcls = ChannelClass_3hf, 
    stationcode = StationCode
    ) %>%
  left_join(kydat, by = 'stationcode') %>%
  dplyr::select(sitetmp, chcls)

# # channel data
# chdat <- read.csv('ignore/tblChannelEngineering.txt', stringsAsFactors = FALSE) %>%
#   dplyr::select(stationcode, channeltype) %>%
#   rename(chcls = channeltype) %>%
#   left_join(kydat, by = 'stationcode') %>%
#   dplyr::select(sitetmp, chcls)

# add chdat to mmdat
mmdat <- mmdat %>% 
  mutate(
    sitetmp = gsub('_[0-9]+\\..*$', '', site)
  ) %>% 
  left_join(chdat, by = 'sitetmp') %>% 
  dplyr::select(-sitetmp)

# add chdat to oedat
oedat <- oedat %>% 
  mutate(
    sitetmp = gsub('_[0-9]+\\..*$', '', site)
  ) %>% 
  left_join(chdat, by = 'sitetmp') %>% 
  dplyr::select(-sitetmp)

##
# save 

save(mmdat, file = 'data/mmdat.RData', compress = 'xz')
save(evdat, file = 'data/evdat.RData', compress = 'xz')
save(oedat, file = 'data/oedat.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')

