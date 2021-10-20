library(tidyverse)
library(rgdal)
library(sf)
library(raster)
library(ggmap)
library(tmap)
library(leaflet)

nsw_census <- readOGR("data/Census_NSW/MB_2016_NSW.shp")

forest_cover <- raster("data/NSW_Forest/NSW_Forest/nsw_for_56m/dblbnd.adf")

fire_hist_1 <- st_read("data/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRsgdb/NSW_FireHistory20200413_HRs.gdb")


qpal <- colorFactor("Blues", nsw_census$MB_CAT16, n = 7)

# leaflet(nsw_census) %>% 
#   addPolygons(fillColor = ~qpal(MB_CAT16))

# 

ggplot(nsw_census@data ) +
  geom_boxplot(aes(x = "Area",y = AREASQKM16))

ggplot(nsw_census@data %>% filter(AREASQKM16 <= 0.1)) +
  geom_boxplot(aes(x = "Area",y = AREASQKM16))



ggplot(nsw_census@data ) +
  geom_boxplot(aes(x = "Pop",y = POP16))


ggplot(nsw_census@data %>% filter(POP16 > 250)) +
  geom_boxplot(aes(x = "Pop",y = POP16))



ggplot(nsw_census@data ) +
  geom_boxplot(aes(x = "Pop",y = DWELL16))



ggplot(nsw_census@data %>%  filter(DWELL16 < 250) ) +
  geom_boxplot(aes(x = "Pop",y = DWELL16))




## Worth adding this to the model and see if it is a significant variable
ggplot(nsw_census@data) +
  geom_bar(aes( x = MB_CAT16))


#Residential, Commercial, Education 

### Finding polygons with larger area

cpal<- colorNumeric("magma", nsw_census$AREASQKM16)

fpal<- colorFactor("BuPu", nsw_census$MB_CAT16, n = 11)

npal_dwl <- colorNumeric("magma", nsw_census$DWELL16)



npal_pop <- colorNumeric("magma", nsw_census$POP16)

nsw_census[nsw_census$AREASQKM16>50,] %>% 
  leaflet() %>% 
  addPolygons(fillColor  = ~ cpal(AREASQKM16),fillOpacity = 1,color = "")



nsw_census[nsw_census$MB_CAT16 %in% c("Water","Primary Production"),]%>% 
  leaflet() %>% 
  addPolygons(fillColor  = ~ fpal(MB_CAT16),fillOpacity = 1,color = "") %>% 
  addLegend(pal = fpal, values = ~MB_CAT16, group = "circles", position = "bottomleft")






nsw_census[nsw_census$DWELL16 <=4500 & nsw_census$DWELL16 >= 50 , ] %>% 
  leaflet() %>% 
  addPolygons(fillColor  = ~ npal_dwl(DWELL16),fillOpacity = 1,color = "") %>% 
  addLegend(pal = npal_dwl, values = ~DWELL16, group = "circles", position = "bottomleft")



#nsw_census$POP16 <=4500 &
nsw_census[ nsw_census$POP16 >= 50 , ] %>% 
  leaflet() %>% 
  addPolygons(fillColor  = ~ npal_pop(POP16),fillOpacity = 1,color = "") %>% 
  addLegend(pal = npal_pop, values = ~POP16, group = "circles", position = "bottomleft")



####
## Forest cover
plot(forest_cover)






### write the sampling function
## The sampling function should get a dataframe that has xy-coords of ignition points
## The sampling function should get a raster and sample random points from borders of 
## the nsw urban area also get random points from forest raster



# random_sample_points <-function(){
#   
#   
# }


## Plot fires

which(st_is_valid(fire_hist_1[,39]) == FALSE)



plot(st_make_valid(fire_hist_1[,39]))


st_make_valid(fire_hist_1[7395,39])





fire_hist_1 %>% 
  mutate(Shape = st_make_valid(Shape)) %>% 
  filter(lubridate::year(StartDate) >= 2017 ) %>% 
  ggplot()+
  geom_sf(aes(fill = YearOfFire))






#Residential, Commercial, Education
fpal_fire_year<- colorFactor("magma", fire_hist_1$YearOfFire, n = 11)


res_nsw_dt<- rmapshaper::ms_simplify(nsw_census[nsw_census$MB_CAT16 %in% c("Residential", "Commercial", "Education","Transport"),])



fire_hist_1 %>% 
  mutate(Shape = st_make_valid(Shape)) %>% 
  filter(lubridate::year(StartDate) == 2020 ) %>% 
  leaflet() %>% 
  addPolygons(fillColor = "Red",fillOpacity = 1,color = "") %>% 
  addPolygons(data = res_nsw_dt,color = "Black",fillColor = NULL,opacity = 0.5,fillOpacity = 0,fill =FALSE)






### Selecting points from fire shapes and points from nsw region


## first step selecting points from fire shapes




fire_hist_1 %>% 
  mutate(Shape = st_make_valid(Shape)) %>% 
  filter(lubridate::year(StartDate) %in% c(2020,2019) ) 


