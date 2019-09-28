###-------------------------------------------###
### Core and Periphery with spatial networks  ###
###             J.L. Losada 2019              ###
###-------------------------------------------###

### ├── spatial_net_functions.R
###     └── spatial_net.R          	<--+ you are here
###         └── spatial_net_viz.R
###         └── spatial_net_map.R    


### This script takes the geolocations (latitude and longitude) of places, converts them into spatial points, creates the triangulation (Delaunay) and the spatial network. 

### The funcions in the file spatial_net_functions.r need to be loaded first. I keep the funtions separated in another file just for clarity.

## Load data

  # 1. Cervantes' Persiles -- m1 --
  novels = read.csv("data/persiles_places_manual.csv", stringsAsFactors=F)
  
  # 2. Lope's Peregrino -- m2 --
  # novels = read.csv("data/peregrino_places_manual.csv", stringsAsFactors=F)
  
  # 3. Zuñiga's Semprilis -- m3 --
  # novels = read.csv("data/semprilis_places_manual.csv", stringsAsFactors=F)
  
  # 4. Suarez' Eustorgio -- m4 -- 
  # novels = read.csv("data/eustorgio_places_manual.csv", stringsAsFactors=F)

## Creates the GRAPH in one shoot with pipe %>%----

options(scipen=999)  #  remove scientific notation

  # It converts lat/lon to spatial points. SpatialPointsDataFrame() is a funtion from the package sp.
  graph = SpatialPointsDataFrame(cbind(novels$lon, novels$lat), novels[1], match.ID=TRUE) %>%  
  
  # Pedesma functions
  dd(to = "lines") %>% # Triangulation: "lines" or "polygons".
  SpatialLinesNetwork() # It creates the network. 

  # The network is within the object @g. Just renaming
  grafo = graph@g 

  # Complete the information adding attributes to the graph object: place names and freq of mention in novel----

  V(grafo)$label = novels$lugar[order(match(paste(novels$lat,novels$lon), paste(graph@g$y,graph@g$x)))]
  
  V(grafo)$freq = novels$freq[order(match(paste(novels$lat,novels$lon), paste(graph@g$y,graph@g$x)))]
  
  V(grafo)$degree = graph@g$n

## To get just the TIN. To be used on the map.  
  
  # it converts lat/lon to spatial points. SpatialPointsDataFrame() is a funtion from package sp.
  TIN = SpatialPointsDataFrame(cbind(novels$lon, novels$lat), novels[1], match.ID=TRUE) %>%  
    dd(to = "lines") # Triangulation: "lines" or "polygons" of TIN (these polygons are not the voronoi cells!) 

## To get the VORONOI cells. To be used on the map. 
  
  # I use the package library(ggvoronoi), because  dd(to = "polygons") gives out not the voronoi cells! just the poligons of the TIN), and deldir() data model does not work well with leaflet
  
  library(tidyverse)
  library(ggvoronoi)
  
  vor_spdf <- voronoi_polygon(data=novels,x="lon",y="lat")

## To get the VORONOI area (vtess$summary$dir.area) To be used on the map.
  
  vtess <- deldir(novels$lon,novels$lat)


## METRICS (from the package igraph). EC Eigenvector----

	V(grafo)$ec = eigen_centrality(grafo)$vector

	# Get quantiles 

	ec = eigen_centrality(grafo)$vector  %>%
	  tibble::enframe()

	quants_ec <- quantile(ec$value, 0.25)  # lower: the center; higher: periphery.

	V(grafo)$quant_ec  <- with(ec, factor(ifelse(value < quants_ec[1], 1, 2))) 

# Summarize data


novels$eigen = V(grafo)$ec[match(novels$lugar, V(grafo)$label)] # Eigenvector
novels$dir.area = vtess$summary$dir.area[match(paste(novels$lat,novels$lon), paste(vtess$summary$y,vtess$summary$x))]
novels$quant_ec =  V(grafo)$quant_ec[match(novels$lugar, V(grafo)$label)]


# Final R objets: grafo, TIN, vor_spdf, vtess 

library(RColorBrewer)  