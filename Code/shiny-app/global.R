#------------------------------------------------------------------------------#
#
# LOAD DATA AND FUNCTIONS FOR OPERATION OF BASKETBALL SIM
#
#------------------------------------------------------------------------------#

### Set Up Workspace -----------------------------------------------------------

rm(list=ls())
setwd("~/GitHub/CANOPY/Code/shiny-app/")
library(plyr)
library(data.table) # To race against join() and other methods of merging tables for speed
library(microbenchmark)
library(magrittr)
require(compiler)
enableJIT(0)

cn <- function(x) colnames(x)
rn <- function(x) rownames(x)


### Set Up Initialization, Objective, Neighbors and Temperature ----------------

### Load youth data 
load("./data/youth-to-court-data.Rda")
x_ij <- data.table(y2c)
NC <- nrow(x_ij) # N is the number of youth, C(i) is the choice set available to individual
i.u <- unique(x_ij$i)
j.u <- unique(x_ij$j)
N <- length(i.u)
J <- length(j.u)

## Load court-to-court data
load("./data/court-to-court-distances.Rda")
c2c <- data.table(c2c)
colnames(c2c) <- as.character(1:J)
rownames(c2c) <- as.character(1:J)

### Reduce the size of objects to practically test timing
iSub <- sample(i.u, 50000)
jSub <- sample(j.u, 50)
x_ij <- x_ij[i %in% iSub]
x_ij <- x_ij[j %in% jSub]

c2c <- c2c[jSub, c(jSub), with = FALSE]

### Reset the sizes of objects
NC <- nrow(x_ij)
i.u <- unique(x_ij$i)
j.u <- unique(x_ij$j)
N <- length(i.u)
J <- length(j.u)

### Determine weight for youth in objective
wMap <- c("n200_.FPL"   = 1,
          "n100_199FPL" = 2,
          "n50_99FPL"   = 3,
          "n0_50FPL"    = 4)
x_ij[, w := wMap[as.character(pov)]]
#x_ij[, table(pov, w)]

### Set Initial Allocation and Counts ------------------------------------------

### Starting resources are equal number of staff as sites
r_j <- data.table(j = j.u, r = 1)
R <- r_j[, sum(r), drop = T]

### Set lower and upper bounds
vLowerBound <- as.vector(rep(0, J))
vUpperBound <- as.vector(rep(J, J)) # This is effectively unconstrained, except at zero below


### Preallocating space/precalculating objects used in the Objective call ------

### Precalculate what we'll need in xr_ij
xr_ij <- merge(x_ij, r_j, by = "j")
xr_ij[, xb := -1.5 + (-1.0)*d + (-2.0)*d*cr]
xr_ij[, eV := exp(xb + 0.5*r)]
hist(exp(xr_ij$xb))

V_ij <- xr_ij[order(i,j), .(i, j, eV)] %>%
  setkey(i)
w_i <- as.matrix(unique(xr_ij[order(i), .(i, w)]))


### Set Function To Be Minimized -----------------------------------------------

### The objective is a weighted sum of the probability that each youth participates in any park
Obj <- function(V_ij){
  sumV <- V_ij[, sum(eV), by = i]
  vSumExbs <- as.vector(sumV$V1)
  p_i <- as.vector(vSumExbs / (1 + vSumExbs))
  #all(table(w_i[,"i"] == p_i$i)) # This checks that all youth are in the same order
  t(w_i[,2]) %*% p_i
}

### Set a function to adjust the valuations according to the proposed change to the state
# jFrom <- sample(j.u, 1); jTo <- sample(j.u, 1)
upVal <- exp(+1*0.5); downVal <- exp(-1*0.5)
updateV <- function(V_ij, jFrom, jTo){
  V_ij[j == jFrom, eV := eV*downVal]
  V_ij[j == jTo,   eV := eV*upVal]
  return(V_ij)
}


### Set Function for Selecting Neighbors ---------------------------------------

### Prepare inverse distances between courts
c2c.inv <- 1/c2c
diag(c2c.inv) <- 0 # Otherwise, this is infinite
J == nrow(c2c.inv) # Check that the number of courts in court-to-court data is the same as from y2c data

# /!\ Consider whether the draw steps could be optimized using data.table
#     Right now, this is low priority since this function runs so quickly.
GenProposal <- function(r0, TransMatrix, lb, ub) {
  
  # Randomly select a courts to pull seat from
  
  jFromElig <- r0$j[r0$r!=lb]
  jFrom <- sample(jFromElig, 1)
  jFromRow <- which(rn(TransMatrix) == jFrom)
  
  # Randomly select a community area to send seat to
  
  jToElig <- r0$j[r0$r!=ub]
  jToEligCols <- which(cn(TransMatrix) %in% jToElig)
  
  # /!\ Could be simplified using sweep() and multinomial draws
  vTransProbs <- TransMatrix[jFromRow, jToEligCols]/sum(TransMatrix[jFromRow, jToEligCols])
  vCumTransProbs <- cumsum(vTransProbs)
  jToCol <- findInterval(runif(1), vCumTransProbs)
  jTo <- jToElig[jToCol]
  
  return(list(jFrom, jTo))
}

### Establish a Temperature Function -------------------------------------------

Temp <- function(Iter, MaxIter) {
  # In tests with a prior application, the optimization seems to work best when it's pretty chilly.
  # So: quadratic cooling, and we start with low temperature
  #(1 - (Iter/MaxIter))/10
  (1 - (Iter/MaxIter))^2/5
}
