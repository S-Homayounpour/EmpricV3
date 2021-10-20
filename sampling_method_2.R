library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(leaflet)
library(mapview)




##### sampling method version 2

nsw_census <- readOGR("data/ignoredata/Census_NSW/MB_2016_NSW.shp")


## find center of the fires which is already done
## find points that fires have intersected with urban area
nsw_census<- st_as_sf(nsw_census)

nsw_census <- nsw_census %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

#finding burned part of blocks





selected_regions<- c("Residential","Commercial","Education","Industrial","Hospital/Medical")


smaller_urban <- nsw_census %>% 
  filter(MB_CAT16 %in% selected_regions)




donuts <- st_read("data/donuts.shp")





selected_donuts <- donuts %>%
  slice_sample(.,n=3000)


mapview(smaller_urban)+
  mapview(selected_donuts , color = "red")
