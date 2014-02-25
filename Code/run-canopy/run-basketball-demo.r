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
  enableJIT(3)


#####################
# Set Run Parameters
#####################

  nIter       <- 3e5
  nCheckPoint <- ceiling(nIter/50) # 6e3

#############################################################
# Set Up Initialization, Objective, Neighbors and Temperature
#############################################################

  load("./data/prepped/youth-to-court-data.Rda")
    n.yc <- nrow(y2c)

    y.Id.unq <- unique(y2c$y.Id)
    c.Id.unq <- unique(y2c$c.Id)
    n.y <- length(y.Id.unq)
    n.c <- length(c.Id.unq)
  load("./data/prepped/court-to-court-distances.Rda")
    
  # Determine weight for youth in objective
    
    y2c$w[y2c$pov == "n200_.FPL"  ] <- 1
    y2c$w[y2c$pov == "n100_199FPL"] <- 2
    y2c$w[y2c$pov == "n50_99FPL"  ] <- 3
    y2c$w[y2c$pov == "n0_50FPL"   ] <- 4
    yWgt <- unique(y2c[, c("y.Id", "w")])

  #-------------------------------------------#
  # # # Set Initial Allocation and Counts # # #
  #-------------------------------------------#

    # Starting resources are equal number of staff as sites
  
    Alloc <- data.frame(c.Id.unq)
    colnames(Alloc) <- "c.Id"
    Alloc$s0 <- as.integer(1)
    
    # Also break out these columns as vectors to see if their use can speed of runs of Obj()
    c.Id.unq <- as.vector(c.Id.unq)
    s0       <- as.vector(Alloc$s0)

    # Set lower and upper bounds
    vLowerBound <- as.vector(rep(0, n.c))
    vUpperBound <- as.vector(rep(n.c, n.c)) # This is effectively unconstrained


#-------------------------------------------------------------------------------#
# # # Sandbox to Test Functions to Improve Runtimes for Components of Obj() # # #
#-------------------------------------------------------------------------------#

    #----
    # # # 1. Compare run times for various procedures to map allocation to youth-to-court data
    #----

      # Try standard merging (my first instinct)
        system.time(m <- merge(y2c, Alloc, by="c.Id"))
        # In a test with nyc ~= 800k, nc = 60, different runs
        # took 8.64, 5.54, 3.68, 4.43 seconds (before enableJIT(3))

      # Try a join() using plyr instead... seems to work a bit faster
        system.time(m <- join(y2c, Alloc, by="c.Id"))
        # In a test with nyc ~= 800k, nc = 60, different runs
        # took 4.79, 3.87, 6.71, 3.82, 4.68 seconds (before enableJIT(3))

      # Generate indicator function to create mapping using inner products.
        # (The indicator matrix requires a huge chunk of memory, but may speed operation)

        # Prepare the matrix, which should be nyc by nc
        y2c_c.times.vert  <- matrix(rep(y2c$c.Id, n.c), nrow = n.yc)
        c_y2c.times.horiz <- matrix(rep(c.Id.unq, n.yc), nrow = n.yc, byrow=TRUE)

        # Declare s.to.yc as a logical matrix, to avoid typing as double
        s.to.yc <- array(FALSE, c(nrow(y2c), ncol(n.c)))
        s.to.yc <- (y2c_c.times.vert == c_y2c.times.horiz) 
        # Also create a version of the matrix which is a double, since %*% casts into a double, and this may save time
        s.to.yc.d <- (y2c_c.times.vert == c_y2c.times.horiz)
        rm(y2c_c.times.vert, c_y2c.times.horiz)

        # Test speeds
        system.time(y2c.s <- s.to.yc %*% s0)
        # In a test with nyc ~= 800k, nc = 60, since s.to.yc has been typed as logical, 
        # different runs took 10.7, 0.83, 0.83, 0.73... seconds.
        # After enableJIT(3), took: 8.08, 0.94, 0.78, 0.61, 0.54, 0.47, 0.45
        system.time(y2c.s <- s.to.yc.d %*% s0)
        # Left as a double, the result is that
        # different runs took 0.60, 0.77, 0.89, 0.73... seconds. 
        # After enableJIT(3), took: 15.63, 0.78, 0.64, 0.56, 0.52

        # Conclusion seems to be no special difference in how this matrix is stored, expect that the logical matrix
        #   saves on RAM.

      # Try using data.table storage and merging for the tables
        y2c.dt <- data.table(y2c, key="c.Id")
        s.dt <- data.table(Alloc, key="c.Id")
        system.time(y2c.s.dt <- merge(y2c.dt, s.dt))
        # Run times are fastest yet (before enableJIT(3)): 0.43, 0.36, 0.33, 0.36... seconds.
        # After enableJIT(3), took: 0.36, 0.30, 0.29, 0.24 seconds.

      # Conclusion: seems that data.table is the clear winner barring further developments

    #----
    # # # 2. Preallocating space/precalculating objects used in the Objective call
    #----

      # Precalculate what we'll need in y2c
      y2c <- within(y2c, {
        e <- rnorm(n.yc)
        xb <- 1.5 + (-1.0)*d + (-2.0)*d*cr + 1.5*e # indirect utility, except for staff allocation, which gets added in at each step
      })
      y2c <- data.table(y2c)

      # Preallocate components of y2c.o which will be added in
      y2c.o <- within(y2c, {
        s <- 1
        e.Xbs <- exp(xb + 0.5*s)
        eXb <- 0
        Sum.eXb <- 0
        p <- 0
      })
      y2c.o <- data.table(y2c.o)
      hist(y2c.o$xb) # if properly tuned, should have decent density both above and below 0

      #Sum.p <- data.table(aggregate(y2c.o$p, list(y2c.o$y.Id), mean, na.rm = T))
      #Sum.p <- setnames(Sum.p, c(1,2), c("y.Id", "p"))
      Sum.p <- y2c.o[, lapply(.SD, sum), by = y.Id, .SDcols=c("p")]

      yWgt   <- data.table(yWgt)
      yPrPov <- merge(Sum.p, yWgt, by="y.Id")
      score  <- t(as.vector(yPrPov$p)) %*% as.vector(yPrPov$w)

  #----
  # # # 3. Try different means to sum e.Vij within individual -- looks like data.table wins again
  #----
  
  # Generating indicator function for all rows corresponding to each youth
  #         y2c_y.times.vert  <- matrix(rep(y2c$y.Id, n.y), nrow = n.yc)
  #         y_y2c.times.horiz <- matrix(rep(y.Id,    n.yc), nrow = n.yc, byrow=TRUE)
  #         Sum.to.y <- array(FALSE, c(n.yc, n.y))
  #         Sum.to.y <- (y2c_y.times.vert == y_y2c.times.horiz)
  #         rm(y2c_y.times.vert, y_y2c.times.horiz)
  
  # Attempting the same with data.table -- works faster than 
  y.pr.sums <- y2c.o[, lapply(.SD, sum), by = y.Id, .SDcols=c("p")]
  # See data.table FAQ at: http://rwiki.sciviews.org/doku.php?id=packages:cran:data.table

    
#--------------------------------------#
# # # Set Function To Be Minimized # # #
#--------------------------------------#

    # The objective is a concave function of the number of seats in the even numbered Community Areas
    Obj <- function(a) {
      # Merge in staff allocation data for probability calculations
      # Generate function value for each y2c combination
      # Get youth probability by dividing value by sum of values by y
      # Inner product between probability and scores
      
      colnames(a) <- c("c.Id", "s") # We're naming this to generic "s" since many states may be passed into this function
      y2c.o <- merge(y2c, a, by="c.Id")
      y2c.o <- within(y2c.o, e.Xbs <- exp(xb + 0.5*s)) # add staff allocations to indirect utility and raise as exponent
      
      #Sum.eXbs <- tapply(e.xbs, list(y2c.o$y.Id), mean, na.rm = T) # Faster than aggregate... slower than lapply within a data.table
      Sum.eXbs <- y2c.o[, lapply(.SD, sum), by = y.Id, .SDcols=c("e.Xbs")]
      y2c.o <- merge(y2c.o, Sum.eXbs, by="y.Id")
      y2c.o$p <- y2c.o$eXbs / (1 + y2c.o$Sum.eXbs)
        # The 1 represents the normalized value of the alternative Vi0, since: 1 = exp(0)
      
      #Sum probability that each youth plays ball
      Sum.p <- y2c.o[, lapply(.SD, sum), by = y.Id, .SDcols=c("p")]
      
      # Merge in youth objective weights and score
      yPrPov <- merge(Sum.p, yWgt, by="y.Id")
      score  <- t(as.vector(yPrPov$p)) %*% as.vector(yPrPov$w)
      #hist(Sum.p$TotPr)
      
      return(score)
    }

  #----------------------------------------------#
  # # # Set Function for Selecting Neighbors # # #
  #----------------------------------------------#

    # Prepare inverse distances between courts
    c2c.inv <- 1/c2c
    diag(c2c.inv) <- 0 # Otherwise, this is infinite
    n.c == nrow(c2c.inv)
      # Check that the length of the distance file is the same as the allocations vector (which is based off of # of court ids)

    ReAlloc <- function(s0, TransMatrix, vLower, vUpper) {
      
      # Randomly select a courts to pull seat from
      
        EligFrom <- (1:n.c)[s0!=vLower]
        FromRow <- sample(EligFrom, 1)
        # print(paste("FromRow =", FromRow))
      
      # Randomly select a community area to send seat to
      
        EligTo <- (1:n.c)[s0!=vUpper]
        vTransProbs <- TransMatrix[FromRow, EligTo]/sum(TransMatrix[FromRow, EligTo])
        vCumTransProbs <- cumsum(vTransProbs)
        ToRow <- findInterval(runif(1), vCumTransProbs)
        # print(paste("ToRow =", ToRow))
      
      # Reassign -- modify s0 to be s_prop when it is returned
      
        s0[FromRow] <- s0[FromRow] - 1
        s0[ToRow]   <- s0[ToRow]   + 1
        return(s0)
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
  Out <- canopy(obj         = Obj,
                alloc       = Alloc,
                lower       = vLowerBound,
                upper       = vUpperBound,
                neighbor    = ReAlloc,
                transmat    = c2c.inv,
                temperature = Temp,
                iterations  = 15, # nIter
                checkpoint  = 3, # nCheckPoint
                verbose     = FALSE) # s0 = Alloc$s0,

  save(Out, file = "./data/out/Simulated Annealing Output", sep="")

