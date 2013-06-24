####################################################
#
# TEST SIMULATED ANNEALING METHOD USING SIMPLE SETUP
#
#   The objective function is to allocate seats to 
#   odd-numbered community areas.
#
####################################################

  #install.packages("GenSA")
  #install.packages("foreign")
  library(GenSA)
  library(foreign)
  # See documentation at http://cran.r-project.org/web/packages/GenSA/GenSA.pdf
  MyDir <- "C:/Users/nmader/Documents/Econ/Chapin Hall Projects/Head Start Reallocation/"
  source(paste(MyDir, "Code/Simple Simulated Annealing Method.r", sep=""))


#####################
# Set Run Parameters
#####################

  RunGenSA  <- 0
  RunMySA   <- 1

#######################
# Create General Setup 
#######################

  #-------------------------------------------#
  # # # Set Initial Allocation and Counts # # #
  #-------------------------------------------#

    nCCA <- 77
    nSeats <- 20000
    vAlloc0_GenSA <- as.vector(rep(0, nCCA-1)) # We represent the constraint by leaving one out, since it is mechanically related to the other allocations.
    vAlloc0_GenSA <- rep(ceiling(nSeats/nCCA),nCCA-1)

    vAlloc0_MySA <- as.vector(rep(ceiling(nSeats/nCCA), nCCA))
    vAlloc0_MySA[nCCA] <- nSeats - (nCCA-1)*ceiling(nSeats/nCCA)
    

    vLowerBound_GenSA <- as.vector(rep(0, nCCA-1))
    vUpperBound_GenSA <- as.vector(rep(nSeats, nCCA-1))

    vLowerBound_MySA <- as.vector(rep(0, nCCA))
    vUpperBound_MySA <- as.vector(rep(nSeats, nCCA))
  

  #--------------------------------------#
  # # # Set Function To Be Minimized # # #
  #--------------------------------------#
  
    # We'll choose only odd numbered Community Areas, with score equal to their Community Number
    
    vScores <- as.vector(1:nCCA)
    vScores[(vScores %% 2) == 0] <- 0 # I.e. zero-out even scores
    
    Obj_GenSA <- function(vAlloc) {
      Alloc_CCA77 <- as.vector(nSeats - sum(vAlloc))
      vAlloc_Aug <- c(vAlloc, Alloc_CCA77)    
      
      -(t(vAlloc_Aug) %*% vScores)
    }

    Obj_MySA <- function(vStateN) {
      -(t(vStateN) %*% vScores)
    }


  #----------------------------------------------#
  # # # Set Function for Selecting Neighbors # # #
  #----------------------------------------------#

    # Load distances between CCAs
    tXY <- data.frame(read.dbf(file = paste(MyDir, "Raw Data/CommArea_Centroids.dbf", sep="")))
    vOnes <- as.vector(rep(1, nCCA))
    mXinRows <- vOnes %*% t(tXY$POINT_X) 
    mYinRows <- vOnes %*% t(tXY$POINT_Y)
    mXinCols <- tXY$POINT_X %*% t(vOnes)
    mYinCols <- tXY$POINT_Y %*% t(vOnes)

    mDistPairs <- sqrt( (mXinRows - mXinCols)^2 + (mYinRows - mYinCols)^2 )
    mInvDist <- 1/mDistPairs
    diag(mInvDist) <- 0

    ReAlloc <- function(State0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a community area to pull seat from
      
        EligFrom <- seq(1:nCCA)[State0!=vLower]
        FromRow <- sample(EligFrom, 1)
        # print(paste("EligFrom =",EligFrom))
        print(paste("FromRow =", FromRow))
      
      # Randomly select a community area to send seat to
  
        EligTo <- seq(1:nCCA)[State0!=vUpper]
        vTransProbs <- TransMatrix[FromRow,]/sum(TransMatrix[FromRow,])
        vTransProbs[-EligTo] <- 0
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), vCumTransProbs)
        print(paste("ToRow =", ToRow))

      # Reassign
        StateN <- State0
        # print("Preassignment"); print(StateN)
        StateN[FromRow] <- StateN[FromRow] - 1
        StateN[ToRow]   <- StateN[ToRow]   + 1
        # print("Postassignment"); print(StateN)
        return(StateN)
    }


  #------------------------------------------#
  # # # Establish a Temperature Function # # #
  #------------------------------------------#

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

  # debug(simulated_annealing)
  if (1 == RunMySA)   Out_MySA <- simulated_annealing(cost = Obj_MySA,
                             s0 = vAlloc0_MySA,
                             lower = vLowerBound_MySA,
                             upper = vUpperBound_MySA,
                             neighbor = ReAlloc,
                             temperature = Temp,
                             iterations = 100000,
                             keep_best = TRUE,
                             trace = TRUE,
                             verbose = FALSE)


# # # To Do # # #

  # 2. Get distances from ArcGIS
  # 3. Set up check-pointing in the called function
  # 5. Read GenSA code 
    # - try to understand how GenSA is identifying neighbors
  

