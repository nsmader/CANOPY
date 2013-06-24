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

  RunMySA   <- 1

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
  
    # We'll choose only odd numbered Community Areas, with score equal to their Community Number
    
    vScores <- as.vector(rep(0:1, length.out=nCCA)) # Even communities have score of 1, odd have score of 0.

    # The objective is a concave function of the number of seats in the even numbered Community Areas
    Obj <- function(vStateN) {
      # -(t(vStateN) %*% vScores)
      x <- vStateN
      x[(1:nCCA %% 2) == 0] <- 0
      x <- sqrt(x)
      -(t(x) %*% x)
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
        # print(paste("FromRow =", FromRow))
      
      # Randomly select a community area to send seat to
  
        EligTo <- seq(1:nCCA)[State0!=vUpper]
        vTransProbs <- TransMatrix[FromRow,]/sum(TransMatrix[FromRow,])
        vTransProbs[-EligTo] <- 0
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), vCumTransProbs)
        # print(paste("ToRow =", ToRow))

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

# # # Try Calling Simple Method # # #

  # debug(simulated_annealing)
  if (1 == RunMySA)   Out <- simulated_annealing(cost = Obj,
                             s0 = vAlloc0,
                             lower = vLowerBound,
                             upper = vUpperBound,
                             neighbor = ReAlloc,
                             temperature = Temp,
                             iterations = 100000,
                             checkpoint = 5000,
                             trace = TRUE,
                             verbose = FALSE)


# # # To Do # # #

  # 1. Map progress using R
  # 2. Write up short abstract for this
    # - Send to Wladimir and Bob. Goal for exercise is to demonstrate (1) we have an implementation; (2) it's fast; (3) it's intuitive; (4) we haven't done so much yet that there isn't room for them to input and work with us
    # Later, send to Parv and Jen

