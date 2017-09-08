### Set Environment -----------------------------------------------------------#

setwd("~/GitHub/CANOPY/Code/shiny-app/")
library(rgdal)
library(leaflet)

### Prep data -----------------------------------------------------------------#

bc   <- readOGR(dsn = "data/ball-courts.shp",                     layer = "ball-courts")
tr   <- readOGR(dsn = "data/CensusTractsTIGER2010.shp",           layer = "CensusTractsTIGER2010")
cent <- readOGR(dsn = "data/CensusTractsTIGER2010_Centroids.shp", layer = "CensusTractsTIGER2010_Centroids")

bc@data <- within(bc@data, {
  alloc <- round(runif(nrow(bc@data))*10, 0)
  pop <- paste(paste0("<b>", Park, "</b>"),
               paste0("Allocation: ", alloc),
               sep = "<br>")
})
                  

### Gen map

palCr <- colorFactor(palette = "greens",
                     domain = tr@crime)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(tr) %>%
  addCircleMarkers(data = bc, radius = ~alloc, popup = ~pop)

