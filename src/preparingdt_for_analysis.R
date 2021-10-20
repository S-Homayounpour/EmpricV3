## Preparing data for analysis

library(WildfireRisk)
library(sf)
library(raster)
library(tidyverse)
library(here)

## First step is creating random lines and deciding if the it is been burnt
## or not 
## Then I should find the variables for that line using sample_raster function 
## in wildfirerisk package

## I have noticed that there is a problem with sampling method.
## So I decided to implment a new sampling methd
## the new sampling method uses ignition point and 
##  the distnace from each sampling method
## So a range of distances from each ignition point is selected and then
## for each distance the 360 degree angle is divided to 8 equal region and from each region
## a random angle is selected.
## I believe this method of sampling represent a more accurate picture of the way that wildfires move.

ignitionprob <- raster(here("test_leaflet/data/act_rasters/ignition/act_igden_n2/hdr.adf"))

initial_risks_loc <- st_read(here("initial_risk.shp")) 

initial_risks_loc <- st_transform(initial_risks_loc,crs = 4326)

loc_coords <- as.data.frame(st_coordinates(initial_risks_loc))

loc_coords$id <-  1:nrow(initial_risks_loc)

loc_coords$pobs_mean <-  initial_risks_loc$pobs_mean


locations <- as_tibble(loc_coords)





### sampling lines in my own way using function designed by Michael Bedward 
### in wildfire risk package
### THe random sample method that I am developing is used for developing models
### I used the sample_lines function in wildfirerisk package to test accuracy of the model


# steps_unit: sample lines every steps_unit meters
# maximum_dist: Maximum distance to do the sampling
random_tool_angles <- function(steps_unit = 100,maximum_dist =20000){
  # number_of_lines: to sample per step unit 
  number_of_lines = 8
  #dividing_range <- rep(seq(0,maximum_dist,steps_unit),number_of_lines)
  
  nmb_angles_to_generate <- length(seq(steps_unit,maximum_dist,steps_unit))
  ## every 8 (number_of_lines) angles reset the process
  
  angles_range <- seq(0,8,1)*45
  
  angles_bb <- list()
  
  last_cell<- length(angles_range)-1
  
  for(i in 1: last_cell){
  
      angles_bb[[i]] <- c(angles_range[i],angles_range[i+1])
  }
  
  
  len_angs <- lapply(seq(steps_unit,maximum_dist,steps_unit), function(i){ 
    
    lapply(1:8, function(s) {
      
      c(sample(seq(angles_bb[[s]][1]+1,angles_bb[[s]][2],1),1),i)
    })
    })

 
  len_angs <- do.call(rbind,len_angs)
  len_angs <- do.call(rbind,len_angs)
  
  ot <- as_tibble(len_angs)
  colnames(ot) <- c("Zavieh","Tool")
  ot <- ot %>% 
    arrange(Tool)
  
  as.data.frame(ot)
  
  
}




tool_zav_dt <-  random_tool_angles()


lines <- lapply(
  1:nrow(locations),
  
  function(i) {
    x0 <- locations[[i,1]]
    y0 <- locations[[i,2]]
    
    # # lengths <- length_fun(nlines)
    # # angles <- angle_fun(nlines)
    
    lapply(1:dim(tool_zav_dt)[1],
           function(k) {
             x1 <- x0 + tool_zav_dt[k,2] * cos(tool_zav_dt[k,1])
             y1 <- y0 + tool_zav_dt[k,2] * sin(tool_zav_dt[k,1])
             # print(matrix(c(x0, x1, y0, y1), ncol = 2))
             # break()
             # print(typeof(x0))
             # print(typeof(y0))
             # print(typeof(x1))
             # print(typeof(y1))
             # print(is.numeric(matrix(c(x0, x1, y0, y1), ncol = 2)))
             # break()
             st_linestring(matrix(c(x0, x1, y0, y1), ncol = 2))
           })
  })
