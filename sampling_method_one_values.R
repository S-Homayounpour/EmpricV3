library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(leaflet)
library(mapview)


fire_hist_1 <- st_read("data/ignoredata/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRs.gdb")


sampled_lines_mv1 <- st_read("data/sampledlines.shp")

fire_hist_1<- fire_hist_1 %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))%>%
  mutate(Shape = st_make_valid(Shape)) 



centroids_inside_the_polygs<- fire_hist_1 %>%
  mutate(ID = row_number()) %>%
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>%
  filter(YearOfFire > 2017) %>%
  mutate(Shape = st_make_valid(Shape)) %>%
  mutate( cntr = st_centroid(Shape)) %>%
  mutate( s = map2(.$cntr,.$Shape,st_intersection)) %>%
  rowwise() %>%
  filter( length(s) != 0) %>% ## around 144 fires from the data was removed because centroid was not inside polygon
  ungroup() %>%
  dplyr::select(-c(cntr,s))



sampled_lines_mv1 <- sampled_lines_mv1 %>% 
  st_set_crs(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  mutate(lineid = row_number())

lines_test <- sampled_lines_mv1 %>%
  slice_sample(.,n=3000)


# 
# mapview(centroids_inside_the_polygs)+
#   mapview(lines_test,color = "red")



### Now I should find values for sampled lines
## Whether it is burnt or not
## forest coverage across the lines
## weather data
## how many streets or water resources are there
## Topological data
## 


# lines_test <- lines_test %>% 
#   st_set_crs(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
#   mutate(lineid = row_number())

##Finding burnt segment in fire history




shamel2 <- lapply(1:dim(centroids_inside_the_polygs)[1], function(i){
  contained <- st_contains(st_geometry(centroids_inside_the_polygs)[[i]],st_geometry(sampled_lines_mv1))
  cond1<- length(unlist(contained)) == 0L
  if(cond1==FALSE){
  list(FireId = centroids_inside_the_polygs[i,]$ID, khatha = sampled_lines_mv1[unlist(contained),]$lineid)
  }
    })
  
## run from here
shamel2_unlist1 <- do.call("rbind",shamel2) 

shamel2_unlist2 <- do.call("rbind",shamel2) 

shamel2 %>% 
  unnest()


