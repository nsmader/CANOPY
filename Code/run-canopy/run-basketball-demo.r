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
  require(compiler)
  library(plyr) # Need to use join()
  library(data.table) # To horserace against join() and other methods of merging tables for speed
  

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
    
    y2c$Wgt[y2c$pov == "n200_.FPL"  ] <- 1
    y2c$Wgt[y2c$pov == "n100_199FPL"] <- 2
    y2c$Wgt[y2c$pov == "n50_99FPL"  ] <- 3
    y2c$Wgt[y2c$pov == "n0_50FPL"   ] <- 4
    yWgt <- unique(y2c[, c("y.Id", "Wgt")])

  # Add an error draw for youth
    y2c$e <- rnorm(nrow(y2c))

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
        c_y2c.times.horiz <- matrix(rep(c.Id,    n.yc), nrow = n.yc, byrow=TRUE)

        # Declare s.to.yc as a logical matrix, to avoid typing as double
        s.to.yc <- array(FALSE, c(nrow(y2c), ncol(n.c)))
        s.to.yc <- (y2c_c.times.vert == c_y2c.times.horiz) 
        # Also create a version of the matrix which is a double, since %*% casts into a double, and this may save time
        s.to.yc.d <- (y2c_c.times.vert == c_y2c.times.horiz)
        rm(y2c.by.c, c.by.y2c)

        # Test speeds
        system.time(y2c.s <- s.to.yc %*% s0)
        # In a test with nyc ~= 800k, nc = 60, since s.to.yc has been typed as logical, 
        # different runs took 0.83, 0.75, 0.63... seconds.
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
    # # # 2. Preallocating space for y2c.o and other objects used in the Objective call
    #----
      
      y2c.o <- within(y2c, {
        s <- 1
        eXb <- 0
        Sum.eXb <- 0
        p <- 0
      })
      y2c.o <- data.table(y2c.o)

      Sum.Pr <- data.table(aggregate(y2c.o$p, list(y2c.o$y.Id), mean, na.rm = T))
      Sum.Pr <- setnames(Sum.Pr, c(1,2), c("y.Id", "Sum.Crt.Pr"))

      yWgt <- data.table(yWgt)      
      yPrPov <- merge(Sum.Pr, yWgt, by="y.Id")
      score <- as.vector(yPrPov$Sum.Crt.Pr) %*% as.vector(t(yPrPov$Wgt))
    
    #----
    # # # 3. Pre-calculate quantities used in Obj() call
    #----
      xb <- with(y2c, 1.5 + (-1.0)*d + (-0.2)*d*cr + 0.5*(s0) + 1.5*e) # indirect utility, except for staff allocation
      hist(xb) # if properly tuned, should have decent density both above and below 0
      # XXX Note: may still play with parameters to get good probability distributions, especially showing 

    #----
    # # # 4. Try different means to sum e.Vij within individual
    #----

      # Generating indicator function for all rows corresponding to each youth
        y2c_y.times.vert  <- matrix(rep(y2c$y.Id, n.y), nrow = n.yc)
        y_y2c.times.horiz <- matrix(rep(y.Id,    n.yc), nrow = n.yc, byrow=TRUE)
        Sum.to.y <- array(FALSE, c(n.yc, n.y))
        Sum.to.y <- (y2c_y.times.vert == y_y2c.times.horiz)
        rm(y2c_y.times.vert, y_y2c.times.horiz)

      # Attempting the same with data.table
        y.pr.sums <- y2c.o[, lapply(.SD, sum), by = y.Id, .SDcols=c("p")]
          # See data.table FAQ at: http://rwiki.sciviews.org/doku.php?id=packages:cran:data.table
        

#--------------------------------------#
# # # Set Function To Be Minimized # # #
#--------------------------------------#

    # The objective is a concave function of the number of seats in the even numbered Community Areas
    Obj <- function(vStateN) {
      # Merge in staff allocation data for probability calculations
      # Generate function value for each y2c combination
      # Get youth probability by dividing value by sum of values by y
      # Inner product between probability and scores
      
      e.Xbs <- exp(xb + 0.5*s) # add staff allocations to indirect utility and raise as exponent
      
      Sum.eXbs <- tapply(e.xbs, list(y2c.o$y.Id), mean, na.rm = T) # Runs faster than aggregate
      Sum.eXbs <- data.frame(Sum.eXb)
      Sum.eXb$y.Id <- rownames(Sum.eXb)
      y2c.o <- merge(y2c.o, Sum.eXb, by="y.Id")
      y2c.o$p <- y2c.o$eXb / (1 + y2c.o$Sum.eXb)
        # The 1 represents the normalized value of the alternative Vi0, since: 1 = exp(0)
      
      #Sum probability that each youth plays ball
      Sum.Pr <- aggregate(y2c.o$p, list(y2c.o$y.Id), mean, na.rm = T)
      colnames(Sum.Pr) <- c("y.Id", "TotPr")
      
      # Merge in youth objective weights and score
      yPrPov <- join(Sum.Pr, yWgt, by="y.Id")
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

