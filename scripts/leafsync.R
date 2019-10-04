
library(tidyverse)
library(leafsync)
library(leaflet)

m1 =  m1 %>% 
  addControl("Cervantes' <em>Persiles</em>", position = "bottomleft",className = "background-color: transparent;" )

# m1 is generated on the fly, the rest is loaded.

m2 =  m2 %>% 
  addControl("Lope de Vega' <em>Peregrino</em>", position = "bottomleft",className = "background-color: transparent;" )

m3 =  m3 %>% 
  addControl("Zuñiga's <em>Semprilis y Genorodano</em>", position = "bottomleft",className = "background-color: transparent;"  )

m4 =  m4 %>% 
  addControl("Suarez' <em>Eustorgio y Clorilene. Historia moscóvica</em>", position = "bottomleft", className = "background-color: transparent;" )

retablo =  sync(m1, m2, m3, m4, 
     ncol = 2
     #, sync = "none"
     ) # 4 panels synchronised


retablo.clasico =  sync(m5, m6, 
                ncol = 2
                #, sync = "none"
) # 4 panels synchronised