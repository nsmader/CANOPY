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
  library(plyr) # Need to use join()
  library(data.table) # To race against join() and other methods of merging tables for speed
  library(rbenchmark)
  library(ggplot2)
  library(ggmap)
  require(compiler)
  #enableJIT(3)
  enableJIT(0)

  source("./code/run-canopy/declare-canopy-method.r")
  "%&%" <- function(...){paste(..., sep="")}
  cn <- function(x) colnames(x)
  rn <- function(x) rownames(x)
  p0 <- function(...) paste0(...)


#####################
# Set Run Parameters
#####################

  nIter       <- 3e5
  nCheckPoint <- ceiling(nIter/50) # 6e3
  subsetData <- TRUE
    nSubsetN <- 50000
    nSubsetJ <- 50

#############################################################
# Set Up Initialization, Objective, Neighbors and Temperature
#############################################################

  load("./data/prepped/youth-to-court-data.Rda")
    x_ij <- y2c
    NC <- nrow(x_ij) # N is the number of youth, C(i) is the choice set available to individual
    i.u <- unique(x_ij$i)
    j.u <- unique(x_ij$j)
    N <- length(i.u)
    J <- length(j.u)
  load("./data/prepped/court-to-court-distances.Rda")
    colnames(c2c) <- 1:J
    rownames(c2c) <- 1:J
    
  # Reduce the size of objects to practically test timing
    if (TRUE == subsetData){
      iSub <- sample(i.u, nSubsetN)
      jSub <- sample(j.u, nSubsetJ)
      x_ij <- x_ij[x_ij$i %in% iSub, ]
      x_ij <- x_ij[x_ij$j %in% jSub, ]
  
      c2c <- c2c[jSub, jSub]
      colnames(c2c) <- jSub
      rownames(c2c) <- jSub
    }

  # Reset the sizes of objects
    NC <- nrow(x_ij)
    i.u <- unique(x_ij$i)
    j.u <- unique(x_ij$j)
    N <- length(i.u)
    J <- length(j.u)

  # Determine weight for youth in objective
    
    x_ij$w[x_ij$pov == "n200_.FPL"  ] <- 1
    x_ij$w[x_ij$pov == "n100_199FPL"] <- 2
    x_ij$w[x_ij$pov == "n50_99FPL"  ] <- 3
    x_ij$w[x_ij$pov == "n0_50FPL"   ] <- 4

  #-------------------------------------------#
  # # # Set Initial Allocation and Counts # # #
  #-------------------------------------------#

  # Starting resources are equal number of staff as sites
    r_j <- data.frame(cbind(j.u, 1))
    colnames(r_j) <- c("j", "r")
    R <- sum(r_j[,2])

  # Set lower and upper bounds
    vLowerBound <- as.vector(rep(0, J))
    vUpperBound <- as.vector(rep(J, J)) # This is effectively unconstrained, except at zero below

    #----
    # # # Preallocating space/precalculating objects used in the Objective call
    #----

    # Precalculate what we'll need in xr_ij
      xr_ij <- merge(x_ij, r_j, by = "j")
      xr_ij <- within(xr_ij, {
        xb <- -1.5 + (-2.0)*d + (-2.0)*d*cr # Apply parameters of demand
        V <- exp(xb + 0.5*r)
      })
      hist(exp(xr_ij$xb))
      
      V_ij <- data.table(xr_ij[, c("i", "j", "d", "cr", "xb", "w", "V")], key = "i")
      w_i <- as.matrix(unique(xr_ij[order(xr_ij$i), c("i", "w")]))
      vW_i <- t(w_i[,2])


#--------------------------------------#
# # # Set Function To Be Minimized # # #
#--------------------------------------#

  # The objective is a weighted sum of the probability that each youth participates in any park
    Obj <- function(V_ij){
      sumV <- V_ij[, sum(V), by=i]
      vSumExbs <- as.vector(sumV$V1)
      p_i <- as.vector(vSumExbs / (1 + vSumExbs))
      #table(w_i[,1] == sumV$i) # Checking that youth ids are in the same order.
      vW_i %*% p_i
    }
    benchmark(Obj(V_ij))
  
  # Set a function to adjust the valuations according to the proposed change to the state
    # jFrom <- sample(j.u, 1); jTo <- sample(j.u, 1)
    upVal <- exp(+1*0.5); downVal <- exp(-1*0.5)
    updateV <- function(V_ij, jFrom, jTo){
      from_ij <- V_ij$j == jFrom; to_ij <- V_ij$j == jTo
      V_ij$V[from_ij] <- V_ij$V[from_ij]*downVal
      V_ij$V[to_ij  ] <- V_ij$V[to_ij  ]*upVal
      return(V_ij)
    }
    benchmark(updateV(V_ij, sample(j.u, 1), sample(j.u, 1))) # about 0.03 per call with N = 5e5, J = 50

# comp <- merge(V_ij, V_ij_0, by = c("i", "j"))
# with(comp, table(j[V.x != V.y]))
# table(V_ij$j[from_ij])
# table(V_ij$j[to_ij])

  #----------------------------------------------#
  # # # Set Function for Selecting Neighbors # # #
  #----------------------------------------------#

  # Prepare inverse distances between courts
    c2c.inv <- 1/c2c
    diag(c2c.inv) <- 0 # Otherwise this is infinite
    J == nrow(c2c.inv) # Check that the number of courts in court-to-court data is the same as from y2c data

    GenProposal <- function(r0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a courts to pull seat from
      
        jFromElig <- r0$j[r0$r!=vLower]
        jFrom <- sample(jFromElig, 1)
        jFromRow <- which(rn(TransMatrix) == jFrom)
      
      # Randomly select a community area to send seat to
      
        jToElig <- r0$j[r0$r!=vUpper]
        jToEligCols <- which(cn(TransMatrix) %in% jToElig)
        
        # XXX Could be simplified using sweep() and multinomial draws
        myTrans <- TransMatrix[jFromRow, jToEligCols]
        vTransProbs <- myTrans/sum(myTrans)
          # Could be done with sweep() although at present this is a very fast operation
        vCumTransProbs <- cumsum(vTransProbs)
        jToCol <- findInterval(runif(1), vCumTransProbs) + 1
        jTo <- jToElig[jToCol]
      
        # Alternative: trying rmultinom() to get one iteration of one draw from the multinomial prob
        # x <- jToElig[rmultinom(1, 1, myTrans) == 1]
        # Note that this streamlines things a bit by internally normalizing the probabilities
          # benchmark()ing this against the above gets no practical speed-up
      
      return(list(jFrom, jTo))
    }
    system.time(for (i in 1:1000) GenProposal(r_j, TransMatrix = c2c.inv, vLowerBound, vUpperBound))
    # r0 = r_j; TransMatrix = c2c.inv; vLower = vLowerBound; vUpper = vUpperBound
    # Run time is about 1e-5 per call. That's good.

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

  V_ij_0 <- V_ij
  # debug(simulated_annealing)
  Out <- canopy(obj         = Obj,
                alloc       = r_j,
                v_ij        = V_ij,
                theta       = 0.5,
                lower       = vLowerBound,
                upper       = vUpperBound,
                proposal    = GenProposal,
                transmat    = c2c.inv,
                nudge       = updateV,
                temperature = Temp,
                iterations  = 1000, # nIter
                checkpoint  =  100, # nCheckPoint
                verbose     = FALSE)
  
  save(Out, file = "./output/solutions/canopy-solution -- bball, full data, 1000 iters")

# Arguments for passing into debugging for CANOPY
obj         = Obj;
alloc       = r_j;
v_ij        = V_ij;
theta       = 0.5;
lower       = vLowerBound;
upper       = vUpperBound;
proposal    = GenProposal;
transmat    = c2c.inv;
nudge       = updateV;
temperature = Temp;
iterations  = 1000; # nIter
checkpoint  = 100; # nCheckPoint
verbose     = FALSE

