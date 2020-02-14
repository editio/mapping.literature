###-------------------------------------------###
### Core and Periphery with spatial networks  ###
###             J.L. Losada 2019              ###
###-------------------------------------------###

### ├── spatial_net_functions.R
###     └── spatial_net.R
###         └── spatial_net_viz.R
###         └── spatial_net_map.R    <--+ you are here


### Visualizations---- 

library(RColorBrewer)

# markers color

cols <- brewer.pal(3, "Set1") 
com.color.ec = V(grafo)$quant_ec[match(novels$lugar, V(grafo)$label)] 

# areas color

qpal1 <- colorFactor("Greys", novels$freq, reverse = F)
qpal2 <- colorFactor("Greys", vtess$summary$dir.area, reverse = T)


library(leaflet)
library(leaflet.extras)


# MAP---- 

m1 = leaflet() %>% 

  addFullscreenControl(pseudoFullscreen = T) %>%
  
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/tile/{z}/{y}/{x}", attribution = 'Base Map &copy; Esri | Project by J.L. Losada Palenzuela', group="Base (no labels)", option=list(continuousWorld=TRUE, tileSize="256", minZoom="0", maxZoom="11")) %>%
  
  addTiles(urlTemplate = "https://dh.gu.se/tiles/imperium/{z}/{x}/{y}.png", attribution = 'Barrington Roman Empire: (CC BY-SA) <a href="https://dh.gu.se/dare/">DARE</a> | Project by J.L.Losada', group="Roman Empire", option=list(continuousWorld=TRUE, tileSize="256", minZoom="0", maxZoom="11")) %>% 
  
  addPolygons(data=TIN, weight=1.25, group="TIN", color = "grey") %>%  # Delaunay
  
  addPolygons(data=vor_spdf,
              group="Voronoi by freq",
              stroke=F, weight=1,
              fill=TRUE, fillOpacity = 0.6,
              color = ~qpal1(novels$freq), 
              smoothFactor=0.9, label = sprintf("Voronoi point: %s",as.character(vor_spdf@data$lugar)))  %>%
  
  addPolygons(data=vor_spdf,
              group="Voronoi by area",
              stroke=F, weight=1,
              fill=TRUE, fillOpacity = 0.6,
              color = ~qpal2(vtess$summary$dir.area), # From deldir (vtess$summary$dir.area).
              smoothFactor=0.9, label = sprintf("Voronoi point: %s",as.character(vor_spdf@data$lugar)))  %>%
  
  addCircleMarkers(data=novels, group = "EC+freq", lng = ~lon, lat = ~lat, weight = novels$freq, fillOpacity=0.8, popup = paste0("<b>",novels$lugar,"</b>","<br>","eigenvector: ",novels$eigen, "<br>", "freq of mention: ",novels$freq), label = novels$lugar, 
                   color = cols[com.color.ec],
                   radius = ~sqrt(freq)*4,
                   stroke = F) %>% # the cols is set supra, plot() 
  
  addCircleMarkers(data = novels, radius = 6, fillOpacity=0.8, stroke = F, color = cols[com.color.ec], popup = paste0("<b>",novels$lugar,"</b>","<br>","eigenvector: ",novels$eigen, "<br>", "freq of mention: ",novels$freq), label = novels$lugar, group = "EC") %>%
  
  ## <Start Search feature> ##

    # Search in eigen's values (novels$eigen) and in places (novels$lugar) at the same time. Group hidden avoids to be shown in the map
    
    addMarkers(data = novels, label = paste(novels$lugar,": ",novels$eigen), group ="hidden") %>%
    
    addSearchFeatures(
      targetGroups = c("hidden"),
      options = searchFeaturesOptions(
        zoom=6, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE )) %>%
  
  ## <End Search feature/> ##

addLayersControl(
  baseGroups = c("Base (no labels)", "Roman Empire", "no map"),
  overlayGroups = c("EC+freq","EC", "Voronoi by freq", "Voronoi by area", "TIN"),
  options = layersControlOptions(collapsed = T)) %>%
  
  hideGroup(c("hidden", "Voronoi by freq", "EC+freq", "Voronoi by area")) %>%
  
  addLegend(position = "bottomleft", opacity = 0.4, 
            colors = c("#E41A1C", "#377EB8"), 
            labels = c("Center (eigenvalues in Q<sub>1</sub>)", 
                       "Periphery"
            ))