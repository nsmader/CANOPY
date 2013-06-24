####################################################
#
# TEST SIMULATED ANNEALING METHOD USING SIMPLE SETUP
#
#   The objective function is to allocate seats to 
#   odd-numbered community areas.
#
####################################################

  #install.packages("GenSA")
  library(GenSA)
  # See documentation at http://cran.r-project.org/web/packages/GenSA/GenSA.pdf
  MyDir <- "C:/Users/nmader/Documents/Econ/Chapin Hall Projects/Head Start Reallocation/Code"


#####################
# Set Run Parameters
#####################

  RunGenSA  <- 0
  RunMySA   <- 1

#######################
# Create General Setup 
#######################

  # # # Set Initial Allocation and Counts # # #
    nCCA <- 77
    nSeats <- 20000
    vAlloc0_GenSA <- as.vector(rep(0, nCCA-1)) # We represent the constraint by leaving one out, since it is mechanically related to the other allocations.
    vAlloc0[1] <- nSeats

    vAlloc0_MySA <- as.vector(rep(0, nCCA))
    vAlloc0_MySA[1] <- nSeats
    
    vLowerBound_GenSA <- as.vector(rep(0, nCCA-1))
    vUpperBound_GenSA <- as.vector(rep(nSeats, nCCA-1))

    vLowerBound_MySA <- as.vector(rep(0, nCCA))
    vUpperBound_MySA <- as.vector(rep(nSeats, nCCA))
  
  # # # Set Function To Be Minimized # # #
  
    # We'll choose only odd numbered Community Areas, with score equal to their Community Number
    
    vScores <- as.vector(1:nCCA)
    vScores[(vScores %% 2) == 0] <- -1 # I.e. zero-out even scores
    
    Obj_GenSA <- function(vAlloc) {
      Alloc_CCA77 <- as.vector(nSeats - sum(vAlloc))
      vAlloc_Aug <- c(vAlloc, Alloc_CCA77)    
      
      -(t(vAlloc_Aug) %*% vScores)
    }

    Obj_MySA <- function(vStateN) {
      -(t(vStateN) %*% vScores)
    }


  # # # Set Function for Selecting Neighbors # # #

    # Load distances between CCAs
    #tDist <- read.csv2(file = paste(MyDir, "Raw Data/CCA Distance Matrix.csv", sep=""), sep = ",", header = TRUE)

    tDist <- matrix(data = runif(nCCA^2), nrow=nCCA)
    mDist <- as.matrix(tDist)
    mInvDist <- solve(mDist)

    ReAlloc <- function(State0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a community area to pull seat from
      
        FromRow <- ceiling(runif(1)*nCCA)
      
      # Randomly select a community area to send seat to
  
        vTransProbs <- TransMatrix[FromRow]/sum(TransMatrix[FromRow])
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), vCumTransProbs)

      # Reassign
        StateN <- State0
        StateN[FromRow] <- StateN[FromRow] - 1
        StateN[ToRow]   <- StateN[ToRow]   + 1
        return(StateN)
    }

  # # # Establish a Temperature Function # # #

    Temp <- function(Iter, MaxIter) {
      
      # For now, keeping this very simple
      return(Iter/MaxIter)
      
    }


##################################
# Attempt Use of the GenSA Package 
##################################

# # # Try Calling GenSA Method # # #

  if (1 == RunGenSA)      Out_GenSA <- GenSA(par = vAlloc0_GenSA, lower = vLowerBound, upper = vUpperBound, fn = Obj, control=list(maxit = 10000))

# # # Try Calling Simple Method # # #

  if (1 == RunSimpleSA)   Out_MySA <- simulated_annealing(cost = Obj_MySA,
                             s0 = vAlloc0_MySA,
                             lower = vLowerBound_MySA,
                             upper = vUpperBound_MySA,
                             neighbor = ReAlloc,
                             temperature = Temp,
                             iterations = 10000,
                             keep_best = TRUE,
                             trace = TRUE,
                             verbose = FALSE)


# # # To Do # # #

  # 1. Fix neighbor sampling to (a) sample only from from above the minimum; (b) allocate to only those below max
        
  # 2. Improve the returned object from MySA procedure to include history
  # 3. If error is not cleared up in Set up diagnostic for state trnastion
  # 4. Need to understand how GenSA is identifying neighbors
  

