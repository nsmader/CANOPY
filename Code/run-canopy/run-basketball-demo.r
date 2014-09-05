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
  library(plyr) # Need to use join()
  library(data.table) # To race against join() and other methods of merging tables for speed
  require(compiler)
  #enableJIT(3)
  enableJIT(0)


#####################
# Set Run Parameters
#####################

  nIter       <- 3e5
  nCheckPoint <- ceiling(nIter/50) # 6e3

#############################################################
# Set Up Initialization, Objective, Neighbors and Temperature
#############################################################

  load("./data/prepped/youth-to-court-data.Rda")
    x_ij <- y2c
    x_ij$i <- x_ij$y.id
    x_ij$j <- x_ij$c.id
    NC <- nrow(x_ij) # N is the number of youth, C(i) is the choice set available to individual
    NC <- nrow(x_ij)
    i.u <- unique(x_ij$i)
    j.u <- unique(x_ij$j)
    N <- length(i.u)
    J <- length(j.u)
  load("./data/prepped/court-to-court-distances.Rda")
    
  # Reduce the size of objects to practically test timing
    iSub <- sample(i.u, 50000)
    jSub <- sample(j.u, 50)
    x_ij <- x_ij[x_ij$y.id %in% iSub, ]
    x_ij <- x_ij[x_ij$c.id %in% jSub, ]

    c2c <- c2c[jSub, jSub]

  # Reset the sizes of objects
    NC <- nrow(x_ij)
    i.u <- unique(x_ij$y.id)
    j.u <- unique(x_ij$c.id)
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
    s_j <- cbind(j.u, 1)
    colnames(s_j) <- c("j", "s")
    R <- sum(s_j$s)
    
  # Also break out these columns as vectors to see if their use can speed of runs of Obj()
    j.u <- as.vector(j.u)
    s0  <- as.vector(s_j[,2])

  # Set lower and upper bounds
    vLowerBound <- as.vector(rep(0, J))
    vUpperBound <- as.vector(rep(J, J)) # This is effectively unconstrained, except at zero below

    #----
    # # # Preallocating space/precalculating objects used in the Objective call
    #----

    # Precalculate what we'll need in xs_ij
      xs_ij <- merge(x_ij, s_j, by = "j")
      xs_ij <- within(xs_ij, {
        #e <- rnorm(NC)
        xb <- -1.5 + (-1.0)*d + (-2.0)*d*cr
        #eXb <- exp(xb)
        V <- exp(xb + 0.5*s)
      })
      hist(xs_ij$xb) # if properly tuned, should have decent density both above and below 0
      
      V_ij <- data.table(xs_ij[, c("i", "j", "V")], key = "i")
      w_i <- as.matrix(unique(xs_ij[order(xs_ij$i), c("i", "w")]))


#--------------------------------------#
# # # Set Function To Be Minimized # # #
#--------------------------------------#

  # The objective is a weighted sum of the probability that each youth participates in any park

  upVal <- exp(+1*0.5); downVal <- exp(-1*0.5)
  # jFrom <- sample(j.u, 1); jTo <- sample(j.u, 1)
  Obj <- function(V_ij, jFrom, jTo){ # upVal, downVal # ... the Deltas are always -1 and 1 in this implementation
    
    from_ij <- V_ij$j == jFrom; to_ij <- V_ij$j == jTo
    
    # Approach of modifying all records, without time cost of subsetting
    system.time({
      V_ij$V[from_ij] <- V_ij$V[from_ij]*downVal
      V_ij$V[to_ij  ] <- V_ij$V[to_ij  ]*upVal
      sumV <- V_ij[, sum(V), by=i]
      vSumExbs <- as.vector(sumV$V1)
      p_i <- as.vector(vSumExbs / (1 + vSumExbs))
      #table(w_i[,1] == sumV$i) # Checking that youth ids are in the same order.
      t(w_i[,2]) %*% p_i
    })
    
    # Approach of subsetting records, to modify just those affected by the change
    # XXX Note: updates to V_ij aren't saved for the next iteration.
    # Thus, we'd need to join the new V values back to the original data set, which likely makes this
    #   close to equivalent with the approach above.
    # Then again, because we will not accept all proposals, we do not want to modify the data
    #   when we call Obj(). This, then, should be a faster way to go.
    system.time(
      system.time(for (i in 1:100) iUpd <- V_ij$i[from_ij | to_ij])
        # This seems to take up a lot of clock time: 0.03. Half of it is calculating "from_ij | to_ij"
      system.time(VUpd <- V_ij[V_ij$i %in% iUpd,]) # This one too: 0.03 -- see if we can speed up subsetting with data.table
      VUpd$V2 <- VUpd$V
      
      VUpd$V2[VUpd$j == jFrom] <- VUpd$V2[VUpd$j == jFrom]*downVal
      VUpd$V2[VUpd$j == jTo  ] <- VUpd$V2[VUpd$j == jTo  ]*upVal
      
      VSums <- VUpd[, lapply(.SD, sum), by="i", .SDcols=c("V", "V2")]
      pDiff_i <- (VSums$V2 / (1 + VSums$V2)) - (VSums$V / (1 + VSums$V))
      #table(w_i[,1] == sumV$i) # Checking that youth ids are in the same order.
      score <- score + t(w_i[w_i[,1] %in% iUpd,2]) %*% pDiff_i
    })
  }
  system.time(for (i in 1:1) Obj(V_ij, sample(j.u, 1), sample(j.u, 1)))


  #----------------------------------------------#
  # # # Set Function for Selecting Neighbors # # #
  #----------------------------------------------#

    # Prepare inverse distances between courts
    c2c.inv <- 1/c2c
    diag(c2c.inv) <- 0 # Otherwise, this is infinite
    J == nrow(c2c.inv)

    # XXX Still need to slightly refactor this, to return the j's for the from and to
    GenProposal <- function(s0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a courts to pull seat from
      
        EligFrom <- (1:J)[s0!=vLower]
        FromRow <- sample(EligFrom, 1)
        # print(paste("FromRow =", FromRow))
      
      # Randomly select a community area to send seat to
      
        EligTo <- (1:J)[s0!=vUpper]
        vTransProbs <- TransMatrix[FromRow, EligTo]/sum(TransMatrix[FromRow, EligTo])
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), vCumTransProbs)
        # print(paste("ToRow =", ToRow))
      
      # Reassign -- modify s0 to be s_prop when it is returned
      
        s0[FromRow] <- s0[FromRow] - 1
        s0[ToRow]   <- s0[ToRow]   + 1
        return(s0)
    }
    system.time(for (i in 1:1000) GenProposal(s0, TransMatrix = c2c.inv, vLowerBound, vUpperBound))
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

  # debug(simulated_annealing)
  Out <- canopy(obj         = Obj,
                alloc       = Alloc,
                lower       = vLowerBound,
                upper       = vUpperBound,
                neighbor    = GenProposal,
                transmat    = c2c.inv,
                temperature = Temp,
                iterations  = 15, # nIter
                checkpoint  = 3, # nCheckPoint
                verbose     = FALSE) # s0 = Alloc$s0,

  save(Out, file = "./data/out/Simulated Annealing Output", sep="")


######################
# CALCULATE BENCHMARKS 
######################

  ### Calculate multiple benchmarks of the objective:
    xs_ij <- data.table(xs_ij, key="i,j")

  # Uniform distribution of staff
    V_ij$s.unif <- R / J
    bench.unif <- Obj()

  # Distribution of staff in rough (i.e. rounded-off) proportion to neighborhood poverty
    w_j <- xs_ij[, sum(w), by = j]$V1
    w <- sum(w_j)
    w_j <- w_j / w
    V_ij$s.pov <- w_j * R
    bench.pov <- Obj()
    
  # Distribution of staff in rough (i.e. rounded-off) proportion to # of youth in given radius
    n_j <- xs_ij[, length(w), by = j]
    n <- sum(n_j)
    n_j <- n_j / n
    V_ij$s.pop <- n_j * R
    bench.pop <- Obj()

  # Distribution of staff based on simple simulated annealing objective -- without demand side eq
    # XXX Need to think about what this is: i.e. what we could conceivably do, if we did not have
    #     the resources to do a proper demand-side investigation.
  # Distribution of staff based on simple simulated annealing objective -- with demand side eq
    bench.can <- Obj()

  # Frame the differences in equivalent # of high-poverty youth who are now expected to be successfully
  #   targeted, at no cost, based simply on having better resources (data, demand side, algorithm) for planning

###############
# DESIGN OUTPUT
###############

  ### This is likely for a different file -- a server.r file for the Shiny application we'll build
  
  # Conditional selection for prioritizing all youth, versus weights by poverty...
  # Sliders are: relative weight placed on youth of different backgrounds, total amount of resources
  #   made available, number of iterations offered to the run, (# of initializations, or # of
  #   restarts from previous best solution? ... these may be too technical)

  ### Brainstormed elements:
  # Progress bar, based on the # of iterations run so far (x% complete), and plot of checkpoints
  #   showing the best value seen so far. The (x% complete) bar can be flipped on through a flag that
  #   is hit once the annealing routine is called, and flipped off when it's done.
  # Bar chart showing the benchmark values, which highlights the added value from using 
