library(tidyverse)
library(forcats)
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
  rename(id = SampleID_old) %>% 
  mutate(site = gsub('_[0-9]+\\..*$', '', id))

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

##
# get channel type

# rafi's channel data
chdat <- read.csv('ignore/csci.alg.df.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(StationCode, ChannelClass_3ef) %>%
  rename(
    chcls = ChannelClass_3ef, 
    site = StationCode
    )

# add chdat to indat, final formatting
indat <- indat %>% 
  left_join(chdat, by = 'site') %>% 
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
  dplyr::select(id, site, lon, lat, type, ind, chcls, tax, scr, thr, scrcat)

# add smc shed names to indat
coordinates(indat) <- indat[, c('lon', 'lat')]
crs(indat) <- CRS(prstr)
indat <- raster::intersect(indat, sheds) %>%
  data.frame %>%
  dplyr::select(id, site, lon, lat, type, ind, chcls, tax, scr, thr, scrcat, SMC_Name)

##
# addl algal IBI data

# old algal ibi divided by median of ref calibration scores to standardize
thrsh <- tibble(
  ind = c('H20','D18','S2'),
  thrsh = c(75, 79, 69)
  )
aldat <- read.csv('ignore/tblAlgaeIBI.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(SampleID, S2, D18, H20) %>% 
  gather('ind', 'scr', S2:H20) %>% 
  left_join(thrsh, by = 'ind') %>% 
  mutate(scr = scr / thrsh) %>% 
  dplyr::select(-thrsh)


##
# save 

save(indat, file = 'data/indat.RData', compress = 'xz')
save(evdat, file = 'data/evdat.RData', compress = 'xz')
save(aldat, file = 'data/aldat.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')

