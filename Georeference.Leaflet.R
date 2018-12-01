install.packages("devtools") # Unless you don't have it already installed.
library(devtools) # function to install from github

devtools::install_github("editio/georeference")  # Install package from editio/georeference
devtools::install_github("rstudio/leaflet")  # Install package from rstudio/leaflet

########
##### georeference(): Geolocation R package + leaflet(): Javascript library in R to plot interactive maps 
#######

library(georeference)
library(leaflet)

# run the georef function
places = georef(c("Roma", "Complutum", "Byzantium"))

# run the leaflet functions

leaflet() %>%
  addTiles(urlTemplate = "http://pelagios.org/tilesets/imperium/{z}/{x}/{y}.png",
           attribution = 'Barrington Roman Empire: (CC BY-SA) <a href="http://dare.ht.lu.se">DARE</a> & <a href="http://commons.pelagios.org">Pelagios</a> | Project by J.L.Losada',
           group="Roman Empire",
           option=list(continuousWorld=TRUE, tileSize="256")) %>%  # it adds the tiles of the Roman Empire
  addMarkers(places$lon, places$lat) %>%
  addProviderTiles ("CartoDB.Positron", group = "Modern") %>% 
  addPolylines(places$lon, places$lat,  color = "blue", weight = 3) %>%    
  
addLayersControl(
  baseGroups = c("Roman Empire", "Modern"),
  options = layersControlOptions(collapsed = F)) # it adds the switch controls for added maps.
  