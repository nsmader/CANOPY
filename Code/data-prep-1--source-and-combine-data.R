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
key <- readLines(con = "Code/key/key.txt")
if (FALSE){
  api.key.install(key = key) # This only needs to be installed once  
}
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
  rm(pull)
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

tr      <- readOGR(dsn = "data/raw", layer = "CensusTractsTIGER2010_cook")
tr <- spTransform(tr, CRS("+init=epsg:4326")) # This converts the XY coordinates to lat/long

tr_cent_chi <- readOGR("data/raw", "Chicago_CensusTracts_2010_Centroids")
chiTracts <- unique(tr_cent_chi@data$TRACTCE10)

### Pull 2015 crime incident data from the Chicago data portal ----------------#

cr <- read.socrata(url = "https://data.cityofchicago.org/resource/vwwp-7yr9.json")
viol <- 
  data.table(cr) %>%
  .[fbi_code %in% c("01A", "02", "03", "04A", "04B")] %>%
  .[, .(lon = as.numeric(location.longitude),
        lat = as.numeric(location.latitude))] %>% # choosing x/y coords since that matches the tract shape files
  filter(!is.na(lat), !is.na(lon))
rm(cr)
viol_sp <- SpatialPoints(coords = viol)
proj4string(viol_sp) <- proj4string(tr)

### Calculate violent crimes per capita ---------------------------------------#

viol_t <- 
  over(viol_sp, tr) %>%
  data.table() %>%
  .[, .(nViol = .N, tract = TRACTCE), by = TRACTCE] %>% 
  merge(totNs, by = "tract", all.y = TRUE) %>% 
  .[, viol_1k := ifelse(tot==0 | is.na(TRACTCE), 0, nViol/tot*1000)] %>%
  .[tract %in% chiTracts]

#------------------------------------------------------------------------------#
### Finish youth-level data set ------------------------------------------------
#------------------------------------------------------------------------------#

### Merge together youth-level data and tract-level data ----------------------#
dt_it <- 
  merge(dt_i,
        tr@data %>%
          mutate(lat = as.numeric(as.character(INTPTLAT)),
                 lon = as.numeric(as.character(INTPTLON))) %>% # The "interpolated" lat and lon are centroid coords
          select(tract = TRACTCE, lat_i = lat, lon_i = lon),
        by = "tract",
        all.y = TRUE) %>%
  .[, .(i, race = factor(race), gender = factor(gender), age = factor(age),
        povInt = factor(povInt), tract, lat_i, lon_i)] %>% 
  .[tract %in% chiTracts]
rm(dt_i)

### Jitter youth locations ----------------------------------------------------#
# Will jitter youth by up to a 1/2 mile
# See this reference for how to use st_sample in the sf package to randomly draw points within geographies: https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/
# Reference to conversion of degrees to miles: http://geography.about.com/library/faq/blqzdistancedegree.htm

mi.per.degLat  <- 69; degLat.per.mi  <- 1/mi.per.degLat
mi.per.degLong <- 53; degLong.per.mi <- 1/mi.per.degLong
pctMiJit <- 0.5
N <- nrow(dt_it)
dt_it[, `:=`(lat_jit = lat_i + runif(N, min = -pctMiJit/2, max = pctMiJit/2)*degLat.per.mi,
             lon_jit = lon_i + runif(N, min = -pctMiJit/2, max = pctMiJit/2)*degLong.per.mi)]
dt_it[, cor(lat_i, lat_jit)]
dt_it[, `:=`(lat_i = lat_jit, lon_i = lon_jit, lat_jit = NULL, lon_jit = NULL)]

#------------------------------------------------------------------------------#
### Load and geocode basketball court data -------------------------------------
#------------------------------------------------------------------------------#

# Read data
d_c <-
  read.csv("data/raw/ball-courts.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  within(Address <- paste(Street.Address, City, State, sep = ", "))

# Geocode and combine information
ll_c <- geocode(d_c$Address, output = "latlon")
dll_c <-
  cbind(d_c, ll_c) %>%
  filter(!is.na(lon)) %>%
  mutate(j = row_number()) %>%
  select(j, park = Park, lat, lon)

c_sp <- SpatialPoints(coords = dll_c[, c("lon", "lat")])
proj4string(c_sp) <- CRS(proj4string(tr))

dt_c <- 
  over(c_sp, tr) %>%
  cbind(dll_c, .) %>%
  select(j, park, lat_j = lat, lon_j = lon, tract = TRACTCE) %>%
  filter(tract %in% chiTracts) %>% 
  merge(viol_t %>% select(tract, viol_1k), by = "tract", all.x = TRUE) %>% 
  data.table()

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
  CJ.dt(dt_c, dt_c) %>% 
  mutate(dist = L1Dist_DegToMi(lat_j, lon_j, i.lat_j, i.lon_j)) %>%
  select(j1 = j, j2 = i.j, dist) %>%
  data.table()

# Generate a square matrix with the same information
c2c.dist <- function(c1, c2){
  L1Dist_DegToMi(dt_c$lat[c1], dt_c$lon[c1], dt_c$lat[c2], dt_c$lon[c2])
}
J <- nrow(dt_c)
dist_c.c <- matrix(mapply(c2c.dist, rep(1:J, each=J), rep(1:J, times=J)), nrow=J)

### Calculate youth-to-court distances ----------------------------------------#

# We will first approximate youth addresses by tract centroids. While the CJ.dt()
# function can perform large cross-joins with data tables between e.g. youth 
# and courts, this can lead to significant memory limitations.

# Calculate distances between courts and each tract centroid
dist_tc <-
  tr@data %>%
  select(tract = TRACTCE, lat_t = INTPTLAT, lon_t = INTPTLON) %>% 
  filter(tract %in% chiTracts) %>% 
  merge(select(dt_c, j, lat_j, lon_j)) %>%
  mutate(lat_t = as.numeric(as.character(lat_t)),
         lon_t = as.numeric(as.character(lon_t)),
         dist = L1Dist_DegToMi(lat_t, lon_t, lat_j, lon_j))
median(dist_tc$dist)
mean(dist_tc$dist <= 3.0)
local_tc <- 
  dist_tc %>% 
  filter(dist <= 3.0) %>% 
  select(tract, j)

# Create permutations between youth and local ball courts
dt_ijt <- 
  merge(dt_it, local_tc, by = "tract", all.x = TRUE, allow.cartesian = TRUE) %>% 
  merge(dt_c %>% select(j, lat_j, lon_j, viol_1k), by = "j", all.x = TRUE) %>% 
  .[!is.na(j)] %>% 
  select(i, race, gender, age, povInt, j, viol_1k) %>% 
  arrange(i, j)
  
# Output file: youth-court matches, coded with poverty, and court-to-court distances
  
save(dt_ijt, dist_cc, dist_c.c, file="./data/prepped/youth-and-court-data.Rda")

