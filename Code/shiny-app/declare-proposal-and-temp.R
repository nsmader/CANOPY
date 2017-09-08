#------------------------------------------------------------------------------#
#
# DECLARE OBJECTIVE, PROPOSAL, AND TEMPERATURE METHODS
#
#------------------------------------------------------------------------------#

### Establish the Objective Function -------------------------------------------

### The objective is a weighted sum of the probability that each youth participates in any park
Obj <- function(V_ij, w_i){
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


### Establish the Proposal Function --------------------------------------------

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
