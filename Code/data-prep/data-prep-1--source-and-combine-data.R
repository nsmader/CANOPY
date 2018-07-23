#------------------------------------------------------------------------------#
# GENERATE DATA ON YOUTH AND FACILITIES FOR CANOPY DEMO
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
### Set up workspace -----------------------------------------------------------
#------------------------------------------------------------------------------#

rm(list = ls())
setwd("~/GitHub/CANOPY")

package.list <- c("rgdal", "data.table", "dplyr", "tidyr", "acs", "sp", "sf",
                  "stringr", "haven", "foreign", "RSocrata", "ggmap", "optiRum")
  # optiRum has the CJ.dt function for creating cartesian products of data.table records
for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only = TRUE)
}
grepv <- function(p, x, ...) grep(p, x, value = TRUE, ...)
cn <- function(x) colnames(x)
f.to.c <- function(f){ return(levels(f)[f]) }
f.to.n <- function(f){ return(as.numeric(levels(f)[f])) }
source("code/helper-functions/geocode-addr.R")
source("code/helper-functions/get-centroids.R")

#------------------------------------------------------------------------------#
### Pull ACS data on youth -----------------------------------------------------
#------------------------------------------------------------------------------#

# Specifically--pull ACS data on # of youth aged 6-17, cross-tabbed by poverty
# Geography to pull is: city of Chicago, tract-level

# This is a full list of ACS 2016 tables on social explorer:
#   https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr
# Table B17024 has the information we need: Age By Ratio of Income to Poverty Level in the Past 12 Months
  
### Read and install Census API key, and pull full table
# Note, this must be applied for at the Census API developer's page at:
#   https://api.census.gov/data/key_signup.html
key <- readLines(con = "Code/key.txt")
api.key.install(key = key)
myGeo <- geo.make(state = "IL", county = "Cook County", tract = "*")

### Pull and clean poverty by race numbers ------------------------------------#
# These are the B17020 tables

suffix <- c("all" = "", "B" = "B", "W" = "H", "H" = "I")
povRNs <- NULL
for (s in suffix){
  pull <- acs.fetch(endyear = 2015, span = 5, geography = myGeo,
                    table.number = paste0("B17020", s), col.names = "pretty")  
  pov_r <- data.frame(pull@estimate)
  colnames(pov_r) <-
    pull@acs.colnames %>%
    str_to_lower() %>%
    gsub("(poverty status in the past 12 months by age.*: )(income.+|total.+)", "\\2", x = .)
      # necessary because some tables have all caps, some are proper case
  pov_r <-
    pov_r %>% 
    mutate(geoid = rownames(pov_r),
           race = names(suffix)[which(s == suffix)])
  povRNs <- rbind(povRNs, pov_r)  
}
povRNs_w <-
  povRNs %>% 
  gather(var, val, -geoid, -race) %>% 
  separate(var, into = c("pov", "age"), sep = ":") %>% 
  filter(grepl("6 to|12 to", age)) %>% 
  mutate(tract = gsub(".*Census Tract (\\S+), Cook.+", "\\1", geoid),
         tract = str_pad(as.numeric(tract)*100, side = "left", width = 6, pad = "0"),
         age = gsub("\\s+(\\d+) to (\\d+).+", "\\1to\\2", age),
         pov = gsub(".+below.+", "lt100", pov) %>% gsub(".+above.+", "ge100", x = .)) %>% 
  select(tract, age, pov, race, val) %>%
  spread(race, val) %>% 
  mutate(O = pmax(0, all - B - H - W),
         B_pct = B/all,
         H_pct = H/all,
         O_pct = O/all,
         W_pct = W/all)

### Pull and clean detailed poverty numbres -----------------------------------#

povNs <- acs.fetch(endyear = 2015, span = 5, geography = myGeo,
                   table.number = "B17024", col.names = "pretty")

### Select and develop only data of interest
povNs.e <- data.frame(povNs@estimate)
colnames(povNs.e) <- 
  cn(povNs@estimate) %>%
  gsub("Age by Ratio of Income to Poverty Level in the Past 12 Months: ", "", x = .)
povNs <- 
  povNs.e %>%
  mutate(geoid = rownames(povNs.e)) %>%
  gather(var, val, -geoid) %>%
  separate(var, into = c("age", "range"), sep = ":") %>%
  filter(grepl("6 to 11 years|12 to 17 years", age),
         range != "") %>%
  mutate(tract = gsub(".*Census Tract (\\S+), Cook.+", "\\1", geoid),
         tract = str_pad(as.numeric(tract)*100, side = "left", width = 6, pad = "0"),
         range1 = substr(str_trim(range), 1, 1),
         povInt = sapply(range1, function(x) switch(x, "U" = "lt50", "." = "50to100", "1" = "100to200", "ge200")),
         pov = ifelse(range1 %in% c("U", "."), "lt100", "ge100"),
         age = gsub("(\\d+) to (\\d+).+", "\\1to\\2", age)) %>%
  select(tract, age, pov, povInt, val) %>%
  group_by(tract, age, pov, povInt) %>%
  summarize(val = sum(val)) %>%
  filter(val != 0) %>%
  merge(select(povRNs_w, tract, age, pov, B_pct, H_pct, O_pct, W_pct), 
        by = c("tract", "age", "pov"),
        all.x = TRUE)

### Get a calculation of the total number of residents per tract
totNs <-
  povNs.e %>%
  select(tot = `Total:`) %>%
  mutate(tract = gsub(".*Census Tract (\\S+), Cook.+", "\\1", rownames(povNs.e)),
         tract = str_pad(as.numeric(tract)*100, side = "left", width = 6, pad = "0"))

### Create synthetic individual youth file ------------------------------------#

dt_i <- 
  data.table(povNs) %>% 
  .[, .(race = sample(c("B", "H", "O", "W"),
                               val,
                               prob = c(B_pct, H_pct, O_pct, W_pct),
                               replace = TRUE)), 
             by = c("tract", "age", "povInt")] %>%
  .[, `:=`(i = 1:nrow(.),
           gender = sample(c("M", "F"), nrow(.), prob = c(0.5, 0.5), replace = TRUE))]
  # The sample() function creates a vector that has length equal to the number
  # of youth

# Gut-check values
dt_i[povInt == "lt50",  round(prop.table(table(race)), 2)]
dt_i[povInt == "ge200", round(prop.table(table(race)), 2)]
  # As expected, more minority representation at lower income levels

#------------------------------------------------------------------------------#
### Read in tract-level data ---------------------------------------------------
#------------------------------------------------------------------------------#

### Read shapefiles -----------------------------------------------------------#

tr      <- readOGR(dsn = "data/raw", layer = "CensusTractsTIGER2010")
tr_cent <- readOGR(dsn = "data/raw", layer = "CensusTractsTIGER2010_Centroids")
  # R function to calculate centroids of shapefiles
  #   http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
  # The centroids above were generated through manual calculation using QGIS

### Pull 2015 crime incident data from the Chicago data portal ----------------#

cr <- read.socrata(url = "https://data.cityofchicago.org/resource/vwwp-7yr9.json")
viol <- 
  data.table(cr) %>%
  .[fbi_code %in% c("01A", "02", "03", "04A", "04B")] %>%
  .[, .(x = as.numeric(x_coordinate),
        y = as.numeric(y_coordinate))] %>% # choosing x/y coords since that matches the tract shape files
  filter(!is.na(x) & !is.na(y))
viol_sp <- SpatialPoints(coords = viol)
proj4string(viol_sp) <- proj4string(tr)

### Calculate violent crimes per capita ---------------------------------------#

viol_t <- 
  over(viol_sp, tr) %>%
  data.table() %>%
  .[, .(nViol = .N, tract = TRACTCE10), by = TRACTCE10] %>% 
  merge(totNs, by = "tract", all.y = TRUE) %>% 
  .[, viol_1k := ifelse(tot==0 | is.na(TRACTCE10), 0, nViol/tot*1000)]

#------------------------------------------------------------------------------#
### Finish youth-level data set ------------------------------------------------
#------------------------------------------------------------------------------#

### Merge together youth-level data and tract-level data ----------------------#
dt_it <- 
  merge(dt_i, 
        viol_t[, .(tract, TRACTCE10, viol_1k)], 
        by = "tract",
        all.x = TRUE) %>%
  merge(select(tr_cent@data, TRACTCE10, lat = Lat, lon = Long),
        by = "TRACTCE10",
        all.y = TRUE) %>% # The all.y = TRUE restricts the sample to just Chicago
  .[, .(i, race = factor(race), gender = factor(race), age = factor(age),
        povInt = factor(povInt), viol_1k, tract, lat, lon)]

### Jitter youth locations ----------------------------------------------------#
# Will jitter youth by up to a 1/2 mile
# See this reference for how to use st_sample in the sf package to randomly draw points within geographies: https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/
# Reference to conversion of degrees to miles: http://geography.about.com/library/faq/blqzdistancedegree.htm

mi.per.degLat  <- 69; degLat.per.mi  <- 1/mi.per.degLat
mi.per.degLong <- 53; degLong.per.mi <- 1/mi.per.degLong
pctMiJit <- 0.5
N <- nrow(dt_it)
dt_it[, `:=`(lat_jit = lat + runif(N, min = -pctMiJit/2, max = pctMiJit/2)*degLat.per.mi,
             lon_jit = lon + runif(N, min = -pctMiJit/2, max = pctMiJit/2)*degLong.per.mi)]
dt_it[, cor(lat, lat_jit)]
dt_it[, `:=`(lat = lat_jit, lon = lon_jit, lat_jit = NULL, lon_jit = NULL)]

#------------------------------------------------------------------------------#
### Load and geocode basketball court data -------------------------------------
#------------------------------------------------------------------------------#

d_c <-
  read.csv("data/raw/ball-courts.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  within(Address <- paste(Street.Address, City, State, sep = ", "))
ll_c <- geocode(d_c$Address, output = "latlon")
dll_c <-
  cbind(d_c, ll_c) %>%
  filter(!is.na(lon)) %>%
  mutate(j = row_number()) %>%
  select(j, park = Park, lat, lon)

write.csv(dll_c, file = "data/prepped/ball-courts-geocoded.csv")

#------------------------------------------------------------------------------#
### Calculate Distances --------------------------------------------------------
#------------------------------------------------------------------------------#

# Establish city-block distance calculations
L1Dist_DegToMi <- function(o.x, o.y, d.x, d.y){
  return(abs(o.x-d.x)*(mi.per.degLong) + abs(o.y-d.y)*(mi.per.degLat))
}  

### Calculate court-to-court distances ----------------------------------------#
# Merge to generate all permutations and calculate long-version
dist_cc <- 
  merge(dll_c, dll_c, by = NULL) %>% 
  mutate(dist = L1Dist_DegToMi(lat.x, lon.x, lat.y, lon.y)) %>%
  select(j.x, j.y, dist) %>%
  data.table()

# Generate a square matrix with the same information
c2c.dist <- function(c1, c2){
  L1Dist_DegToMi(dll_c$lat[c1], dll_c$lon[c1], dll_c$lat[c2], dll_c$lon[c2])
}
J <- nrow(dll_c)
dist_c.c <- matrix(mapply(c2c.dist, rep(1:J, each=J), rep(1:J, times=J)), nrow=J)

### Calculate youth-to-court distances ----------------------------------------#

dt_it <- rename(dt_it, lat_i = lat, lon_i = lon)
dt_c <- select(dll_c, j, lat_j = lat, lon_j = lon) %>% data.table()
library(optiRum)
dt_ijt <- CJ.dt(dt_it, dt_c) # Note: this is a very slow step, 
dt_ijt[, dist := L1Dist_DegToMi(lat_i, lon_i, lat_j, lon_j)]

# Remove youth-court combinations outside of a certain radius, to avoid exploding the size of the data
median(dt_ijt$dist)
mean(dt_ijt$dist <= 3.0)
dt_ijt <- dt_ijt[dist <= 3.0]
dt_ijt <- dt_ijt[, `:=`(lat_i = NULL, lon_i = NULL, lat_j = NULL, lon_j = NULL)]
gc()

# Output file: youth-court matches, coded with poverty, and court-to-court distances
  
save(dt_ijt, dist_cc, dist_c.c, file="./data/prepped/youth-and-court-data.Rda")

  