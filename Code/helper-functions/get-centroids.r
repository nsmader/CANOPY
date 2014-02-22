## This code was gratefully harvested from http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes

gCentroids <- function(myFile, runPlot = FALSE){
  sids <- readShapePoly(myFile, proj4string=CRS("+proj=utm +datum=NAD83")) #, proj4string=CRS("+proj=longlat +ellps=clrk66")
  #class(sids)
  writeSpatialShape(sids, "./data/raw/sids")
  cents <- coordinates(sids)
  cents <- SpatialPointsDataFrame(coords=cents, data=sids@data, proj4string=CRS("+proj=longlat +ellps=WGS84")) # 
  #project(cents@coords, "+proj=longlat +ellps=WGS84") +datum=NAD83
  #library(rgdal)
  #attr(cents, "projection") <-"UTM"
  #attr(cents, "zone") <-16
  #SP <- SpatialPoints(cents, proj4string=CRS("+proj=utm +zone16"))
  #spTransform(SP, CRS("+proj=longlat +datum=NAD83"))
  
  
  centsLL <- spTransform(cents, CRS("+proj=longlat +datum=WGS84" ))
  if (runPlot == TRUE) {
    plot(sids)
    points(cents, col = "Blue")
    points(centroids, pch = 3, col = "Red")
  }
  writeSpatialShape(cents, "/data/raw/cents")
  centroids <- getSpPPolygonsLabptSlots(sids)
  
}