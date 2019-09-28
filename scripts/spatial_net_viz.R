###-------------------------------------------###
### Core and Periphery with spatial networks  ###
###             J.L. Losada 2019              ###
###-------------------------------------------###

### ├── spatial_net_functions.R
###     └── spatial_net.R
###         └── spatial_net_viz.R     <--+ you are here
###         └── spatial_net_map.R    


library(tidyverse)
library(ggvoronoi)


datatable = datatable(novels, style = "bootstrap"
                      #, options = list(dom = 't')
                      )


## Delanauy and Voronboi

plot1_both = ggplot(novels,aes(lon,lat)) +
        geom_segment(
          aes(x = x1, y = y1, xend = x2, yend = y2),
          size = 0.5,
          data = vtess$delsgs,
          linetype = 1,
          color= "orange") +
        coord_quickmap() +
        stat_voronoi(geom="path", linetype = 2) +  # hace calculo y dibuja el path.
        geom_point(color="red",size=1) + 
        theme_void() +
        theme(legend.position = "none")
  
  
##  Only Voronoi

plot2_voro = ggplot(novels,aes(lon,lat)) +
  
  coord_quickmap() +
  stat_voronoi(geom="path", linetype = 2) +  # hace calculo y dibuja el path.
  geom_point(color="red",size=1) + 
  theme_void() +
  theme(legend.position = "none")

## Only Delaunay

plot3_dela =   ggplot(novels,aes(lon,lat)) +
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 0.5,
    data = vtess$delsgs,
    linetype = 1,
    color= "orange") +
  geom_point(aes(lon,lat), color="red",size=1, data = novels) +
  coord_quickmap() +
  theme_void() +
  theme(legend.position = "none")


## Add a map to the plot   
    
  
  library(maps)
  library(mapdata) 
  
  world_map_spain <- map_data('world', region = "Spain")
  world_map_italy <- map_data('world', region = "Italy")
  
  
  names(world_map_spain) <- c("lon","lat", "group","order","region","subregion")
  names(world_map_italy) <- c("lon","lat", "group","order","region","subregion")
  
plot4_map = ggplot(novels, aes(lon,lat)) +
    coord_quickmap() +
    geom_voronoi(aes(fill=vtess$summary$dir.area)) +  
    geom_polygon(data=world_map_italy, aes(group=group), fill=NA, color="white") +
    geom_polygon(data=world_map_spain, aes(group=group), fill=NA, color="white") +
    geom_point(color="red") +    
    theme_void() +
    theme(legend.position = "none")
  
  