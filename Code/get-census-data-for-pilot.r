#-------------------------------------------
# PULL CENSUS DATA ON YOUTH FOR CANOPY PILOT
#-------------------------------------------

# Using ACS 5-year data, get # of youth aged 6-17, cross-tabbed by poverty

# Set up Workspace
  rm(list = ls())
  setwd("~/GitHub/canopy")

  library("acs")
  library("stringr")
  # api.key.install(key = "...")
  
    # ... note: this will need to be run, just once, when you obtain a Census API key. Visit
    # http://www.census.gov/developers/tos/key_request.html to obtain your key
  source("./code/geocode-addr.r")

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
  synthPov <- mapply(rplPov, rep(myFpls, times = nMyTracts), rep(myNs$tract6, each = nMyFpls))
  synthTr  <- mapply(rplTr,  rep(myFpls, times = nMyTracts), rep(myNs$tract6, each = nMyFpls))
  uP <- unlist(synthPov)
  uT <- unlist
  youthData <- data.frame(cbind(as.character(uP), as.character(uT)))
  rownames(youthData) <- NULL
  colnames(youthData) <- c("Pov", "tract6")
  youthData$Id <- 1:nrow(youthData)
  

  # Check creation.
  sum(myNs[,-1]) # Should be # of youth to be created
  

# Load and merge census tract centroids for XY data
  # http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes


# Reduce set of tracts to just City of Chicago
  # (may not be all that necessary, since distance-based preferences will keep farther-away youth from being part of the picture)
  # ... may be able to get this from doing the centroid operation, if we're selecting tracts from there


# Jitter youth locations


# Load and geocode basketball court data
  courtData <- read.csv("./data/raw/ball-courts.csv", header = T)
  courtData <- within(courtAddr, 
                      { Address <- paste(Street.Address, City, State, sep = ", ") })
  courtGeo <- sapply(courtData$Address, gGeoCode) # XXX This is returning NAs that I wouldn't expect it to. Return to this.
  courtXY <- cbind(courtData$Park, t(courtGeo))
  colnames(courtXY) <- c("Park", "Address", "X", "Y")

# Create combination of youth-court records
  yc <- merge(youthData, courtXY)

# Calculate city-block distances between youth and courts
  L1_DegToMi <- function(o.x, o.y, d.x, d.y){
    return(abs(o.x-d.x)*(53) + abs(o.y-d.y)*(69))
  } # Reference to conversion of degrees to miles: http://geography.about.com/library/faq/blqzdistancedegree.htm
  


# Output file: youth-court matches, coded with
  # youth ID, youth poverty, youth neighorhood crime, court ID, and distance to court


  