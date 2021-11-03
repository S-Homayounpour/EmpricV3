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
  slice_sample(.,n=3000) %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))



# mapview(centroids_inside_the_polygs)+
#   mapview(lines_test,color = "red")


# 
# hulls <- st_read("data/hull_blocks_max.shp")
# geom_circumscribed <- st_read("data/geom ciricum/geom_circumscribed_sampling1.shp")
# 
#  mapview(centroids_inside_the_polygs,color = "yellow",zcol = NULL,legend = FALSE)+
#   mapview(hulls,color = "blue",alpha.regions = 0.1, col.regions = "blue" ,legend = FALSE)+
#    mapview(geom_circumscribed, color = "red",alpha.regions = 0.01,legend = FALSE)



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



## This function finds the list of all lines in the sampled lines df that have been fully burnt in the
## corresponding fire
shamel2 <- lapply(1:dim(centroids_inside_the_polygs)[1], function(i){
  contained <- st_contains(st_geometry(centroids_inside_the_polygs)[[i]],st_geometry(sampled_lines_mv1))
  cond1<- length(unlist(contained)) == 0L
  if(cond1==FALSE){
  list(FireId = centroids_inside_the_polygs[i,]$ID, khatha = sampled_lines_mv1[unlist(contained),]$lineid)
  }
    })
  
## 
shamel2_unlist1 <- do.call("rbind",shamel2) 

## this df includes all burnt fires
shamel2_unlist2 <- as_tibble(shamel2_unlist1) %>% 
  group_by(FireId) %>% 
  unnest("khatha") %>% 
  ungroup() %>% 
  mutate(line_type = "burnt") %>% 
  unnest('FireId')




# adding burnt and unburnt to line data aka sampled lines df
smpdlines_infoadded <- sampled_lines_mv1 %>% 
  left_join(shamel2_unlist2, by = c("FireId" = "FireId", "lineid" = "khatha" )) %>% 
  mutate(line_type = replace_na(line_type,"Partially_burnt"))


## next step is adding mean of slope across the line
dem_dt <- raster('data/slope_nsw')





#plot(dem_dt)

## sampling values from raster using Michael code

cellres <- min(raster::res(dem_dt))
 
# e <- extent(144.8,146.6,-35.7,-33.04)
# 
# 
# cropped_ras<- crop(dem_dt,e)
# sapply(cropped_ras,raster::res)

spacing <- min(cellres)

#smpdlines_infoadded <- st_as_sf(smpdlines_infoadded)
# Generate sample points. This will give an sfc object
# containing MULTIPOINTS, one for each scan line
# smpdlines_infoadded
#mpts <- st_segmentize(lines_test, dfMaxLength = 1 / spacing)

mpts_linesamp <-  sf::st_line_sample(lines_test, n = 30)


# Create a data frame of sample points
dat <- lapply(1:nrow(lines_test),
              function(i) {
                xy <- st_coordinates(mpts[[i]])
                
                data.frame(FireId = lines_test$FireId[i],
                           lineid = lines_test$lineid[i],
                           sampleid = 1:nrow(xy),
                           x = xy[, 1],
                           y = xy[, 2])
              })

dat <- do.call(rbind, dat)


# Extract values from rasters
vals <- lapply(dem_dt, function(r) {
  raster::extract(r, as.matrix(dat[, c("x", "y")]))
})
names(vals) <- names(dem_dt)

# Return result
data.frame(dat, vals)
