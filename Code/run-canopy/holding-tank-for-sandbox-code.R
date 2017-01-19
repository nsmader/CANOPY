##############################################
#
### Holding tank for work in various sandboxes
#
##############################################

#-------------------------------------------------------------------------------#
# # # Sandbox to Test Functions to Improve Runtimes for Components of Obj() # # #
#-------------------------------------------------------------------------------#

  #----
  # # # 1. Compare run times for various procedures to map allocation to youth-to-court data
  #----

    ### While the discovery of data.table's speed was very interesting, the current view of 
    # speeding up this merge operation is to avoid it altogether, where the modification to 
    # the state space is made directly to the y2c data

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

      # Prepare the matrix, which should be n.yc by n.c
      y2c_c.times.vert  <- matrix(rep(y2c$c.Id, n.c), nrow = n.yc)
      c_y2c.times.horiz <- matrix(rep(c.u, n.yc), nrow = n.yc, byrow=TRUE)

      # Declare s.to.yc as a logical matrix, to avoid typing as double
      s.to.yc <- array(FALSE, c(nrow(y2c), ncol(n.c)))
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

      # Conclusion seems to be no special difference in how this matrix is stored, except that the logical matrix
      #   saves on RAM.

    # Try using data.table storage and merging for the tables
      y2c.dt <- data.table(y2c, key="c.Id")
      s.dt <- data.table(Alloc, key="c.Id")
      system.time(y2c.s.dt <- merge(y2c.dt, s.dt))
      # Run times are fastest yet (before enableJIT(3)): 0.43, 0.36, 0.33, 0.36... seconds.
      # After enableJIT(3), took: 0.36, 0.30, 0.29, 0.24 seconds.

    # Conclusion: seems that data.table is the clear winner barring further developments

  #----
  # # # 2. Try different means to sum e.Vij within individual -- looks like data.table wins again
  #----
  
  # data.table -- clearly works faster than other methods tried
  y.pr.sums <- y2c.o[, lapply(.SD, sum), by = y.id, .SDcols=c("p")]

  # See data.table FAQ at: http://rwiki.sciviews.org/doku.php?id=packages:cran:data.table

  #----
  # # # 3. Attempt to speed the normalization of probability values in the calculation of the objective
  #----

    z <- sample(mtcars[, c("cyl", "disp")], 1e4, replace = T)
    z$id <- 1:nrow(z)

    # Trying sweep()
    disp_norm <- sweep(z, 2, 1 + sum(z$disp), "/")[, "disp"] # works to divide all of the columns by the sum
    sweepProbs <- function(x) sweep(data.frame(x), 2, 1 + sum(x), "/")
    sweepProbs(z)
    sweepProbs(data.frame(myData = c(1, 3, 5))) # 

    # Trying sweep() with ddply()
    dispNorm_byCyl <- ddply(z, .(cyl), summarize, norm = sweep(z, 2, 1 + sum(z$disp), "/")[, "disp"])
      # Gives unexpected results -- ddply is intended to return results by .(cyl), but the sweep is
      # returning a range of values, not just a scalar calculation. z has 32 rows... the result has 96.
    
    # Trying sweep() with data.table()
      dt <- data.table(z, key="cyl")
      dispNorm_byCyl2 <- dt[, lapply(.SD, sweepProbs), by = "cyl", .SDcols=c("disp")]  
        # Throws an error for "sweepProbs", but works for "sum". Conclusion is: this also only likes scalar returns for functions

    # Trying split() and *apply() to accomplish the sweep()
      f <- factor(z$cyl)
      z.spl <- split(z, f)
      z.norm <- lapply(z.spl, function(x) sweepProbs(data.frame(x$disp)))
      z.unspl <- unsplit(z.norm, f)
      z.by <- unsplit(lapply(split(z$disp, f), sweepProbs), f)

    # Trying tapply() to split data into a ragged array
      norm <- tapply(z$disp, z$cyl, sweepProbs)
      z.unspl <- cbind(z, unsplit(norm, z$cyl))
      
    # Trying unsplit

    # Clocking the split() unsplit() method
  
  #----
  # # # 4. Attempt to speed the normalization of probability values in the calculation of the objective
  # # #     ... specifically by trying to keep the data set as a list object which we can use to apply
  # # #         functions to each individual directly.
  #----
  
    z <- sample(mtcars[, c("cyl", "disp")], 1e4, replace = T)
    z$id <- sample(1:100, nrow(z), replace = T)

    spl <- split(z, z$id)
