########################################################
#
# TEST SIMULATED ANNEALING METHOD USING BASKETBALL PILOT
#
#   The objective function is to allocate seats to 
#   even-numbered community areas.
#
########################################################

  rm(list=ls())  
  #library(foreign) # This allows us to import dbf files.
  setwd("~/GitHub/canopy")
  source("./code/run-canopy/declare-canopy-method.r")
  "%&%" <- function(...){paste(..., sep="")}

#####################
# Set Run Parameters
#####################

  nIter       <- 3e5
  nCheckPoint <- ceiling(nIter/50) # 6e3

#############################################################
# Set Up Initialization, Objective, Neighbors and Temperature
#############################################################

  load("./data/prepped/youth-to-court-data.Rda")
    ny <- nrow(y2c)
  load("./data/prepped/court-to-court-distances.Rda")
    
  # Determine weight for youth in objective
    
    y2c$Wgt[y2c$pov == "n200_.FPL"  ] <- 1
    y2c$Wgt[y2c$pov == "n100_199FPL"] <- 2
    y2c$Wgt[y2c$pov == "n50_99FPL"  ] <- 3
    y2c$Wgt[y2c$pov == "n0_50FPL"   ] <- 4
    yWgt <- unique(y2c[, c("y.Id", "Wgt")])

  # Add an error draw for youth
    y2c$e <- runif(nrow(y2c))

  #-------------------------------------------#
  # # # Set Initial Allocation and Counts # # #
  #-------------------------------------------#

    # Starting resources are equal number of staff as sites
  
    Alloc <- data.frame(unique(y2c$c.Id))
    colnames(Alloc)[1] <- "c.Id"
    Alloc$s0 <- 1 # "s" is for either "staff" or "state"
    nc <- nrow(Alloc)

    vLowerBound <- as.vector(rep(0, nc))
    vUpperBound <- as.vector(rep(nc, nc))

  #--------------------------------------#
  # # # Set Function To Be Minimized # # #
  #--------------------------------------#

    # The objective is a concave function of the number of seats in the even numbered Community Areas
    Obj <- function(vStateN) {
      # Merge in staff allocation data for probability calculations
      # Generate function value for each y2c combination
      # Get youth probability by dividing value by sum of values by y
      # Inner product between probability and scores
      
      y2c.o <- merge(x=y2c, y=vStateN, by="c.Id")
      
      y2c.o <- within(y2c.o, eVij <- exp(1.5 + (-1.0)*d + (-0.2)*d*cr + 0.5*(s0) + 1.5*e))
      
      Sum.eVij <- tapply(y2c.o$eVij, factor(y2c.o$y.Id), mean, na.rm = T)
      colnames(Sum.eVij) <- c("y.Id", "Sum.eVij")
      yPr <- merge(y2c.o[, c("y.Id", "eVij")], Sum.eVij, by="y.Id")
      yPr$Pr <- yPr$eVij / (1 + yPr$Sum.eVij)
        # The 1 represents the normalized value of the alternative Vi0, since: 1 = exp(0)
      
      #Sum probability that each youth plays ball
      Sum.Pr <- aggregate(yPr$Pr, list(yPr$y.Id), mean, na.rm = T)
      colnames(Sum.Pr) <- c("y.Id", "TotPr")
      
      # Merge in youth objective weights and score
      yPrPov <- merge(Sum.Pr, yWgt, by="y.Id")
      score <- as.vector(yPrPov$TotPr) %*% as.vector(t(yPrPov$Wgt))
      #hist(Sum.Pr$TotPr)
      
      return(score)
    }

  #----------------------------------------------#
  # # # Set Function for Selecting Neighbors # # #
  #----------------------------------------------#

    # Prepare inverse distances between courts
    c2c.inv <- 1/c2c
    diag(c2c.inv) <- 0 # Otherwise, this is infinite
    nc == nrow(c2c.inv)
      # Check that the length of the distance file is the same as the allocations vector (which is based off of # of court ids)

    ReAlloc <- function(s0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a community area to pull seat from
      
        EligFrom <- seq(1:nc)[s0!=vLower]
        FromRow  <- sample(EligFrom, 1)
        vTransTo <- TransMatrix[FromRow, ]
      
      # Randomly select a community area to send seat to
  
        EligTo <- seq(1:nc)[s0!=vUpper]
        vTransTo[-EligTo] <- 0
        vTransProbs <- vTransTo/sum(vTransTo)
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), c(0,vCumTransProbs))

      # Reassign
        sN <- s0
        # print("Preassignment"); print(sN)
        sN[FromRow] <- sN[FromRow] - 1
        sN[ToRow]   <- sN[ToRow]   + 1
        # print("Postassignment"); print(sN)
        #print(paste0("From Row and Seats = (", FromRow, ", ", s0[FromRow], "), To Row and Seats = (",
        #   ToRow, ", ", s0[ToRow], "), TotSeats = ", sum(s0))
        return(sN)
    }


  #------------------------------------------#
  # # # Establish a Temperature Function # # #
  #------------------------------------------#

    Temp <- function(Iter, MaxIter) {
      
      # In tests with a prior application, the optimization seems to work best when it's pretty chilly.
        # So: quadratic cooling, and we start with low temperature
      #(1 - (Iter/MaxIter))/10
      (1 - (Iter/MaxIter))^2/5
      
    }


#########################
# RUN ANNEALING PROCEDURE 
#########################

  # debug(simulated_annealing)
  Out <- canopy(cost        = Obj,
                s0          = Alloc$s0,
                lower       = vLowerBound,
                upper       = vUpperBound,
                neighbor    = ReAlloc,
                temperature = Temp,
                iterations  = nIter,
                checkpoint  = nCheckPoint,
                verbose     = FALSE)

  save(Out, file = "./data/out/Simulated Annealing Output", sep="")

