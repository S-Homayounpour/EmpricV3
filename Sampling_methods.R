library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(leaflet)
library(mapview)

fire_hist_1 <- st_read("data/ignoredata/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRs.gdb")




# sampling method
# I want to develop two sampling methods 
# the first model consider angle and distnce from ignition points as part of the method
#i.e. it assumes that the question is what the probabilty of fire burnig from point A to point B
# if the point A is located at c meters at y angle.
## The second sampling method assumes that  we have burnt lines and unburnut lines and then 
## sample from lines and see if they are burnt or not. 
# it means that this method does not care about angles and is basically drawing samples from every two points
# in the map.



# Sampling method 1
# step 1 finding the centroids of fires
# step 2 finding the longest distance from each fire centeroid which will be used for sampling
# step 3 building the lines for each fire using random distance and angle.

# step 1
 

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


# #
# # Unsuccessful
# ## a function that find the convex hull around the polygon and give back its radius
# # 
# # circle <- function(Shape){
# #   cx_hull <- st_convex_hull(Shape)
# #   dayereh <- st_inscribed_circle(cx_hull,dTolerance = 1)
# #   dayereh
# # }
# # 
# # 
# # center <-  function(Shape){
# #   cx_hull <- st_convex_hull(Shape)
# #   dayereh <- st_inscribed_circle(cx_hull,dTolerance = 1)
# #   markaz <- st_centroid(dayereh)
# #   markaz
# # }
# # 
# # 
# # radius <-  function(Shape){
# #   cx_hull <- st_convex_hull(Shape)
# #   dayereh <- st_inscribed_circle(cx_hull,dTolerance = 1)
# #   markaz <- st_centroid(dayereh)
# #   rad <- st_distance(markaz,st_boundary(dayereh),by_element = TRUE)
# #   rad  
# # }
# #   #filter(is.list(s))
# # # step 2 finding the longest distance
# # test_cir_in_polg <- centroids_inside_the_polygs %>%
# #   select(WildFireId,Shape) %>% 
# #   filter(row_number() == 300 ) %>% 
# #   mutate(Shape = st_make_valid(Shape)) %>% 
# #   rowwise() %>% 
# #   mutate( mohat = map(.$Shape, circle)) %>%
# #   mutate( center = map(.$Shape, center)) %>%
# #   mutate( shoae = map(.$Shape, radius)) %>% 
# #   ungroup()
# #   
# # 
# # 
# # 
# # test_cir_in_polg %>% 
# #   st_drop_geometry()%>% 
# #   mutate( mohat = st_geometry(st_sfc(mohat)))%>% 
# #   st_sf() %>%  
# #   leaflet() %>% 
# #   addPolygons(fillColor = "red") %>%  
# #   addPolygons(data = test_cir_in_polg ,fillColor = "blue")
# # 
# 
# 
# 
# # cx_hull_f <- function(Shape){
# #   cx_hull_dt <- st_convex_hull(Shape)
# #   cx_hull_dt
# # }
# # 
# # radius_circ <- function(Shape,cntr){
# #   cx_hull_dt <- st_convex_hull(Shape)
# #   # Add centroid, then cast hull to points
# #   hull_points <- st_sfc(cx_hull_dt) %>%
# #       mutate(centroid_geometry = st_centroid(Shape)) 
# #   # %>%
# #   #     st_cast("POINT")
# #   # # Compute distance from centroid to all points in hull
# #   hull_points$dist_to_centroid <- as.numeric(hull_points %>%
# #                                                st_distance(hull_points$centroid_geometry, by_element = TRUE))
# #   
# #   hull_points
# #   
# # }
# # Unsuccessful
##

## drawing a chull convex around fire area for fires after 2017
hulls <- centroids_inside_the_polygs %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  dplyr::select(ID,Shape) %>% 
  mutate(Shape = st_make_valid(Shape)) %>%
  st_convex_hull()


## finding the centroirds of the fire data
geom_centroids <- centroids_inside_the_polygs %>% 
  dplyr::select(ID,Shape) %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  mutate(Shape = st_make_valid(Shape)) %>% 
  st_centroid(.)


## assigning centroids to hulls
hull_points <- hulls %>% 
  dplyr::select(ID,Shape) %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  mutate(centroid_geometry = geom_centroids$Shape) %>%
  st_cast("POINT")

## calculatin distance from centroids to all poins in hull 
hull_points$dist_to_centroid <- as.numeric(hull_points %>% 
                                             st_distance(hull_points$centroid_geometry, by_element = TRUE))  

## finding the maximum distance from centroids to hull pints
hull_max<- hull_points %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  group_by(ID) %>%
  summarize(max_dist = max(dist_to_centroid)) %>% 
  ungroup()

## drawing an incirumscribed cicle around centroids
geom_circumscribed <- geom_centroids %>%
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  st_buffer(hull_max$max_dist)



#st_write(geom_circumscribed,"geom_circumscribed_sampling1.shp")

#####plots

# tm_shape(centroids_inside_the_polygs) +
#   tm_borders(col = "red") +
#   tm_shape(hulls)+
#   tm_borders(col = "orange") +
#   tm_shape(geom_circumscribed)+
#   tm_borders(col ="blue")
# 
# 
# 
# mapview(centroids_inside_the_polygs,color = "yellow",zcol = NULL,legend = FALSE)+
#   mapview(hulls,color = "blue",alpha.regions = 0.1, col.regions = "blue" ,legend = FALSE)+
#   mapview(geom_circumscribed, color = "red",alpha.regions = 0.01,legend = FALSE)



## selecting centroids and the max distance 
## like an index
sampling_1_index<- hull_points %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) %>% 
  group_by(ID) %>%
  summarize(max_dist = max(dist_to_centroid)) %>% 
  ungroup() %>% 
  st_drop_geometry() 


## extracting long and lat of centroids ading that to MAX distance
## ID belongs to fire
sampling_1_index<- sampling_1_index %>% 
  left_join(geom_centroids, by = "ID") %>%
  st_as_sf() %>% 
  st_set_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  mutate(long = unlist(map(.$Shape,1)),
         lat = unlist(map(.$Shape,2)))
  







##The next step is writing a function that gets the radius and multiply it with 1.5 and generates a series of angles 
## and length

random_ang_len <-  function(max_dist = 100 , nlines = 180){
  
  zaviehs <- seq(0,359,1)
  step_unit = (max_dist * 1.5)/nlines
  faseles <-  seq(0,max_dist * 1.5,step_unit)
  angs_dists_set <- data.frame( angles = sample(zaviehs,nlines),distances = sample(faseles,nlines))
  as.data.frame(angs_dists_set)
}

# ## running sampled lines commented out 
sampled_lines<- sapply(1:nrow(sampling_1_index),
  function(i){
    long0 <-  sampling_1_index[[i,4]]
    lat0 <- sampling_1_index[[i,5]]
    ID_ind <- sampling_1_index[[i,1]]
    max_distance <- sampling_1_index[[i,2]]
    max_distance<- max_distance/100000## find out why

    generated_s <- random_ang_len(max_dist = max_distance)
    # print(paste("row number in outer data is",i))
    # print(paste("The lat for row number",i," is ",lat0))
    # print(paste("The long for row number",i," is ",long0))

    ## next step is runnig the st_linestring function
    lapply(1:nrow(generated_s), function(k,y0 = lat0,x0 = long0, fid = ID_ind ,tool = max_distance){
      x1 <- x0 + generated_s[k,2] * cos(generated_s[k,1])
      y1 <- y0 + generated_s[k,2] * sin(generated_s[k,1])
      dists1 <- generated_s[k,2]
      # print(paste("The row number in inner data is",k))
      # print(paste("x0 for row",k," in inner data is",x0))
      # print(paste("x0 for row",k," in inner data is",x0))
      # print(paste("y0 for row",k," in inner data is",y0))
      # print(paste("x1 for row",k," in inner data is",x1))
      # print(paste("y1 for row",k," in inner data is",y1))
      list(FireId = ID_ind ,dists = dists1  ,lineeq= st_sfc(st_linestring(matrix(c(x0,x1,y0,y1),ncol = 2))))
    })
    })
#   
## sampled lines turned into dataframe
 slines<- do.call(bind_rows,sampled_lines)




## writing sampled lines into file
  st_write(slines,"data/sampledlines.shp")







##### sampling method version 2

nsw_census <- readOGR("data/Census_NSW/MB_2016_NSW.shp")


## find center of the fires which is already done
## find points that fires have intersected with urban area
nsw_census<- st_as_sf(nsw_census)

nsw_census <- nsw_census %>% 
  st_transform(st_crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))

#finding burned part of blocks





selected_regions<- c("Residential","Commercial","Education","Industrial","Hospital/Medical")


smaller_urban <- nsw_census %>% 
  filter(MB_CAT16 %in% selected_regions)


## intersected polygons commented out
tooham<- st_intersects(st_geometry(centroids_inside_the_polygs),st_geometry(smaller_urban))
tooham <- transform(tooham)


tooham_geoms<- st_intersection(centroids_inside_the_polygs %>%
                filter(row_number() %in% tooham$row.id) %>%
                  st_geometry(),
                smaller_urban %>%
                filter(row_number() %in% tooham$col.id) %>%
                  st_geometry())

# transform(tooham_geoms) %>%
#   st_write(.,"data/tooham_geoms.shp")


a <- centroids_inside_the_polygs %>% 
  filter(row_number() %in% tooham$row.id)


b <- smaller_urban %>% 
  filter(row_number() %in% tooham$col.id)



tooham_geoms <- st_sf(tooham_geoms) %>% 
  mutate(commonShapeId = row_number())

mapview(a,col.regions = "red") +
 mapview(b,col.regions = "blue") +
  mapview(tooham_geoms,col.regions = "black")





## cartesian product with centroids and points from blocks and points from 
## burned blocks

# 
# mutate(lat = unlist(map(.$Shape,1)),
#        long = unlist(map(.$Shape,2)))



## sampling points from intersected polygons - fires and census blocks
samp_points<- tooham_geoms %>% 
  rowwise() %>% 
  summarise(points = st_sample(., size = 1)) %>% 
  ungroup() 


samp_points<- transpose(samp_points)

samp_points <- do.call(cbind,samp_points)

samp_points<-  do.call(rbind,samp_points)

samp_points<- data.frame(samp_points)

## figure a way out to sample one point from polygons
samp_points <- st_as_sf(samp_points,coords = c("lon","lat"))

mapview(tooham_geoms, color = "blue")+
  mapview(samp_points,color = "red")

#st_write(samp_points,"data/samp_points.shp")


## that is the final result of sampled points
## now select points from 2 kms edge of the blocks 
# first select centroids of the blocks
# then draw a circle around it 
# then draw a circle around it using bigger radius
# then use st_difference to find the donuts
## thensample points from that donuts


hulls_block <- smaller_urban %>% 
  dplyr::select(MB_CODE16,geometry) %>% 
  st_convex_hull()



blocks_centroid <- smaller_urban %>% 
  dplyr::select(MB_CODE16,geometry) %>%
  st_centroid(.)



hulls_block_points <- hulls_block %>% 
  dplyr::select(MB_CODE16,geometry) %>% 
  mutate(centroid_geometry = blocks_centroid$geometry) %>%
  st_cast("POINT")


hulls_block_points$dist_to_centroid <- as.numeric(hulls_block_points %>% 
                                             st_distance(hulls_block_points$centroid_geometry, by_element = TRUE)) 




hull_blocks_max<- hulls_block_points %>% 
  group_by(MB_CODE16) %>%
  summarize(max_dist = max(dist_to_centroid)) %>% 
  ungroup()


 #st_write(hull_blocks_max,"data/hull_blocks_max.shp")


## now that I have the distance I will make two cricles and sample points from inside using st_difference


circle_inner <- blocks_centroid %>%
  st_buffer(hull_blocks_max$max_dist)


circle_outer <- blocks_centroid %>%
  st_buffer((hull_blocks_max$max_dist)*2)

# 
# st_write(circle_inner,"data/circle_inner.shp")
#  st_write(circle_outer,"data/circle_outer.shp")

circle_inner <- circle_inner %>% 
  arrange(MB_CODE16)

circle_outer <- circle_outer %>% 
  arrange(MB_CODE16)


# test_inner<- circle_inner %>%
#   filter(row_number() %in% 1:20) 
# 
# test_outer <- circle_outer %>% 
#                   filter(row_number() %in% 1:20)
# 
# 
# mapview(test_inner,col.regions = "red")+
#   mapview(test_outer,col.regions = "blue")


# ## donuts_test
# donuts_test_list <- purrr::map(1:nrow(test_inner), ~st_difference(test_outer[.x,], test_inner[.x,]))
# 
# donuts_test <- do.call(rbind,donuts_test_list)
# 
# donuts_test <- st_cast(donuts_test)
# 
# 
# mapview(donuts_test)



# donuts real


# donuts_list <- purrr::map(1:nrow(circle_inner), ~st_difference(circle_outer[.x,], circle_inner[.x,]))
# 
# donuts <- do.call(rbind,donuts_list)
# 
# donuts <- st_cast(donuts)
# 
# 
donuts <- st_read("data/donuts.shp")

donuts_plot_test <-  donuts %>% 
  filter(row_number() %in% 1:1100)


mapview(donuts_plot_test)

## Now that donuts are working fine  it is time to sample points from them. Although I noticed that I did not need
## to draw donuts for all selected census blocks


