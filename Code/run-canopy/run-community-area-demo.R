cd ####################################################
#
# TEST SIMULATED ANNEALING METHOD USING BASKETBALL PILOT
#
#   The objective function is to allocate seats to 
#   even-numbered community areas.
#
####################################################

  #install.packages("foreign") 
  library(foreign) # This allows us to import dbf files.
  MyDir <- "C:/Users/nmader/Documents/Econ/Chapin Hall Projects/Head Start Reallocation/"
  source(paste(MyDir, "Code/Simple Simulated Annealing Method.r", sep=""))
  "%&%" <- function(...){paste(..., sep="")}


#####################
# Set Run Parameters
#####################

  RunMySA     <- TRUE
    nIter       <- 3e5
    nCheckPoint <- 6e3

#######################
# Create General Setup 
#######################

  #-------------------------------------------#
  # # # Set Initial Allocation and Counts # # #
  #-------------------------------------------#

    nCCA <- 77
    nSeats <- 20000

    vAlloc0 <- as.vector(rep(ceiling(nSeats/nCCA), nCCA))
    vAlloc0[nCCA] <- nSeats - (nCCA-1)*ceiling(nSeats/nCCA)
    
    vLowerBound <- as.vector(rep(0, nCCA))
    vUpperBound <- as.vector(rep(nSeats, nCCA))
  

  #--------------------------------------#
  # # # Set Function To Be Minimized # # #
  #--------------------------------------#

    # The objective is a concave function of the number of seats in the even numbered Community Areas
    Obj <- function(vStateN) {
      # -(t(vStateN) %*% vScores)
      x <- vStateN
      x[(1:nCCA %% 2) == 0] <- 0
      #x <- sqrt(x)
      -sum(sqrt(x))
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
      
      # Randomly select a community area to send seat to
  
        EligTo <- seq(1:nCCA)[State0!=vUpper]
        vTransProbs <- TransMatrix[FromRow,]/sum(TransMatrix[FromRow,])
        vTransProbs[-EligTo] <- 0
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), c(0,vCumTransProbs))

      # Reassign
        StateN <- State0
        # print("Preassignment"); print(StateN)
        StateN[FromRow] <- StateN[FromRow] - 1
        StateN[ToRow]   <- StateN[ToRow]   + 1
        # print("Postassignment"); print(StateN)
        #print("From Row and Seats = (" %&% FromRow %&% ", " %&% State0[FromRow] %&% ")" %&%
        #      ", To Row and Seats = (" %&%   ToRow %&% ", " %&% State0[ToRow]   %&% "), TotSeats = " %&% sum(State0))
        return(StateN)
    }


  #------------------------------------------#
  # # # Establish a Temperature Function # # #
  #------------------------------------------#

    Temp <- function(Iter, MaxIter) {
      
      # For now, keeping this very simple. This optimization seems to work best when it's pretty chilly.
      #(1 - (Iter/MaxIter))/10
      (1 - (Iter/MaxIter))^2/5
      
    }


##################################
# Attempt Use of the GenSA Package 
##################################

# # # Try Calling Simple Method # # #

  # debug(simulated_annealing)
  Out <- simulated_annealing(cost = Obj,
           s0 = vAlloc0,
           lower = vLowerBound,
           upper = vUpperBound,
           neighbor = ReAlloc,
           temperature = Temp,
           iterations = nIter,
           checkpoint =  nCheckPoint,
           verbose = FALSE)

  save(Out, file = paste(MyDir, "Prepped Data/Simulated Annealing Output", sep=""))

