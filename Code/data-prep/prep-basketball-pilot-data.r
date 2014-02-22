#-------------------------------------------
# PULL CENSUS DATA ON YOUTH FOR CANOPY PILOT
#-------------------------------------------

# Using ACS 5-year data, get # of youth aged 6-17, cross-tabbed by poverty

# Set up Workspace
  rm(list = ls())
  setwd("~/GitHub/canopy")

  library("acs")
  library("stringr")
  library("foreign") # This is to read the dbf component of the tract centrod shape file
  library(GISTools) # This is used to calculate centroids from Census files
  # api.key.install(key = "...")
  
    # ... note: this will need to be run, just once, when you obtain a Census API key. Visit
    # http://www.census.gov/developers/tos/key_request.html to obtain your key
  source("./code/helper-functions/geocode-addr.r")
  source("./code/helper-functions/get-centroids.r")

# Geography to pull is: city of Chicago, tract-level

# Table to pull is B17024 (Age By Ratio of Income to Poverty Level in the Past 12 Months)
  #x <- acs.lookup(endyear = 2012, span = 5, dataset = "acs", table.name = c("Poverty", "Age"))
  myGeo <- geo.make(state = "IL", county = "Cook", tract = "*") # ,  place = "Chicago city", 
  povNs <- acs.fetch(endyear = 2012, span = 5, geography = myGeo, table.name = "B17024", col.names = "pretty")
    # Note: need to install an API key above

# Pull counts of interest (from the many columns in the table)
  povNs.e <- data.frame(povNs@estimate)
  colnames(povNs.e) <- colnames(povNs@estimate)
  colnames(povNs.e) <- gsub("Age by Ratio of Income to Poverty Level in the Past 12 Months:  ", "", colnames(povNs.e))
  povNs.e$n0_50FPL    <- povNs.e[, "12 to 17 years: Under .50 "]
  povNs.e$n50_99FPL   <- povNs.e[, "12 to 17 years: .50 to .74 "]   + povNs.e[, "12 to 17 years: .75 to .99 "]
  povNs.e$n100_199FPL <- povNs.e[, "12 to 17 years: 1.00 to 1.24 "] + povNs.e[, "12 to 17 years: 1.25 to 1.49 "] +
                         povNs.e[, "12 to 17 years: 1.50 to 1.74 "] + povNs.e[, "12 to 17 years: 1.75 to 1.84 "] +
                         povNs.e[, "12 to 17 years: 1.85 to 1.99 "]
  povNs.e$n200_.FPL   <- povNs.e[, "12 to 17 years: 2.00 to 2.99 "] + povNs.e[, "12 to 17 years: 3.00 to 3.99 "] +
                         povNs.e[, "12 to 17 years: 4.00 to 4.99 "] + povNs.e[, "12 to 17 years: 5.00 and over "]
  povNs.e <- within(povNs.e, {
    tractName <- rownames(povNs.e)
    tractStr <- sapply(tractName, function(s) substr(s, 14, str_locate(s, ",")[1]-1))
    tractNum <- as.numeric(tractStr)
    tract6 <- sub("[.]", "", as.character(sprintf("%07.2f", tractNum)))
      # Note: we need the "[.]" because sub() uses regular expressions. 
      # In regular expressions, "." is a wildcard rather than a literal.
  })
  myFpls <- grep("^n", colnames(povNs.e), value = T)
  myNs <- povNs.e[, c("tract6", myFpls)]

# Create synthetic individual youth file
  # XXX There must be a more elegant way to do this. When creating matrices with {tract, pov}, couldn't figure out how to condense those lists. ()
  rplPov <- function(myFpl, myT){
    n <- myNs[myNs$tract6 == myT, myFpl]
    as.character(rep(myFpl, times = n))
  }
  rplTr <- function(myFpl, myT){
    n <- myNs[myNs$tract6 == myT, myFpl]
    as.character(rep(myT, times = n))
  }
  nMyTracts <- length(povNs.e$tract6)
  nMyFpls <- length(myFpls)
  synthPov <- mapply(rplPov, rep(myFpls, times = nMyTracts), rep(myNs$tract6, each = nMyFpls))
  synthTr  <- mapply(rplTr,  rep(myFpls, times = nMyTracts), rep(myNs$tract6, each = nMyFpls))
  uP <- unlist(synthPov)
  uT <- unlist(synthTr)
  youthData <- data.frame(cbind(as.character(uP), as.character(uT)))
  rownames(youthData) <- NULL
  colnames(youthData) <- c("pov", "tract6")
  youthData$y.Id <- 1:nrow(youthData)

  # Check creation.
  sum(myNs[,-1]) # Should be # of youth to be created

# Merge violent crimes per capita in youth residential tracts
  crime <- read.csv("./data/raw/Violent-crimes-per-capita-by-tract.csv", header = T)
  crime$tract6 <- sprintf("%06.0f", as.numeric(crime$sTract))
  crime$cr <- crime$Pct_ViolentCrime
  youthCrime <- merge(x=youthData[, c("y.Id", "tract6", "pov")], y=crime[, c("tract6", "cr")], by="tract6")

# Load and merge census tract centroids for XY data
  # R function to calculate centroids of shapefiles http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
  # In the end, got centroids from ArcGIS
  TractCentroids <- data.frame(read.csv(file = "./data/raw/CensusTractsTIGER2010_Centroids.csv", header = T))
  youthGeo <- merge(x=youthCrime[,c("y.Id", "tract6", "pov", "cr")], y=TractCentroids[,c("TRACTCE10", "Long", "Lat")], by.x="tract6", by.y="TRACTCE10", all = FALSE)
  # The all = FALSE argument restricts the merge to only observations within Chicago, since that is the scope of the census tracts in the centroids file

# Jitter youth locations
  # Will jitter youth by up to a 1/2 mile
  # Reference to conversion of degrees to miles: http://geography.about.com/library/faq/blqzdistancedegree.htm
  mi.per.degLat  <- 69; degLat.per.mi  <- 1/mi.per.degLat
  mi.per.degLong <- 53; degLong.per.mi <- 1/mi.per.degLong
  summary(cbind(youthGeo$Lat, youthGeo$Long))
  nYouth <- nrow(youthGeo)
  youthGeo$Xjit <- youthGeo$Lat  + runif(nYouth)*(.5)*(degLong.per.mi)
  youthGeo$Yjit <- youthGeo$Long + runif(nYouth)*(.5)*(degLat.per.mi)
  summary(cbind(youthGeo$Xjit, youthGeo$Yjit))

# Load and geocode basketball court data
  courtData <- read.csv("./data/raw/ball-courts.csv", header = T)
  courtData <- within(courtData, 
                      { Address <- paste(Street.Address, City, State, sep = ", ") })
  courtGeo <- sapply(courtData$Address, gGeoCode) # XXX This is returning NAs that I wouldn't expect it to. Return to this.  
  courtGeo.t <- t(courtGeo)
  courtGeo.t <- courtGeo.t[!is.na(courtGeo.t[,1]),]
  courtXY <- data.frame(cbind(1:ncol(courtGeo), t(courtGeo)))
  rownames(courtXY) <- NULL
  colnames(courtXY) <- c("c.Id", "X", "Y")

# Establish city-block distance calculations
  L1Dist_DegToMi <- function(o.x, o.y, d.x, d.y){
    return(abs(o.x-d.x)*(mi.per.degLong) + abs(o.y-d.y)*(mi.per.degLat))
  } 

# Calculate court-to-court distances
  c2c.dist <- function(c1, c2){
    L1Dist_DegToMi(courtXY$X[c1], courtXY$Y[c1], courtXY$X[c2], courtXY$Y[c2])
  }
  nCourts <- nrow(courtXY)
  c2c <- matrix(mapply(c2c.dist, rep(1:nCourts, each=nCourts), rep(1:nCourts, times=nCourts)), nrow=nCourts)

# Match youth to all courts and keep only those within a given radius (tot avoid carrying around more data than we can store)

  nKeptCourts <- 0 # This will be used to pre-allocate a data.frame that will hold the results
  getBallCourtsBall <- function(ixy, maxRad = 3.0){ # Name is a topology joke
    m <- merge(x=youthGeo[ixy,], y=courtXY)
    m$d <- L1Dist_DegToMi(m$Xjit, m$Yjit, m$X, m$Y)
    keepCourts <- m$d < maxRad & !is.na(m$d)
    nKeptCourts <- nKeptCourts + sum(keepCourts)
    return(m[keepCourts, c("y.Id", "c.Id", "pov", "d")])
  }
  system.time({ y2c <- lapply(1:nrow(youthGeo), getBallCourtsBall) })
  y2c.df <- data.frame(runif(nrow(youthGeo)*nKeptCourts*4), nrow=youthGeo*nKeptCourts)
  y2c.df <- ldply(y2c, data.frame)
  #youth.to.court.matches <- do.call("rbind", yc)
  
# Output file: youth-court matches, coded with
  
  save(youth.to.court.matches, file="./data/prepped/youth-to-court-data.Rda")
  save(y2c, file="./data/prepped/youth-to-court-data.Rda")
  save(c2c, file="./data/prepped/court-to-court-distances.Rda")

  