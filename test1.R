library(tigris)
library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(leaflet)


geo <- tigris::zctas(cb = TRUE, starts_with = "75254")
geo <- st_as_sf(geo)


geo <- geo %>% 
  st_as_sf() %>% 
  slice(1:5) %>%
  st_transform(3857) %>%
  arrange(GEOID10)


hull <- geo %>% st_convex_hull()
