########################################################################
#
# SIMPLE SIMULATED ANNEALING METHOD
#   Adapted from http://www.r-bloggers.com/simulated-annealing-in-julia/
#
########################################################################

# canopy()
# Arguments:
# * obj: Function from states to the real numbers. Often called an energy function, but this algorithm works for both positive and negative costs. We're economists. Unlike engineers, I'm maximizing rather than minimizing.
# * alloc: a mapping between facility ID and initial allocation (which is pulled out as r_0)
# * v_ij: information on current state of individual (i) valuations for alternative (j)
# * theta: this is the impact of an additional unit of resource on individual valuation
# * lower, upper: lower and upper bounds of what can feasibly allocated to each facility
# * proposal: function from state to recommended swap. Produces what the Metropolis algorithm would call a proposal.
# * transmat: or transition matrix, an argument to the proposal function which governs how proposals are selected
# * nudge: this is a function that adjusts the valuations according to the proposal
# * temperature: function specifying the temperature at step i.
# * iterations: number of iterations of the algorithm that will be run. This is the only termination condition.
# * checkpoint: frequency, in number of interations, that the procedure will save step, run time, best state visited, best obj value obtained.
# * keep_best: do we return the best state visited or the last state visited? This is defaulted to true and, for now, is not offered as an option to turn off.

# Additional notation:
# * r, o_n: The current state of the system, and the current value of the objective.
# * r_prop, o_prop: The next proposed state, and the corresponding objective

canopy <- function(obj,
                   alloc,
                   v_ij,
                   theta, 
                   lower,
                   upper,
                   proposal,
                   transmat,
                   nudge,
                   temperature,
                   iterations,
                   checkpoint,
                   verbose = FALSE) {

  # Declare the initial state to be the best state so far.
    r_best <- alloc$r
    o_n    <- obj(v_ij) 
    o_best <- o_n
    
  # Calculate benchmark achievements
    # Uniform distribution
      b <- v_ij
      b$V <- exp(b$xb + theta*(R/J))
      o_unif <- Obj(b) # XXX Note: this is because the initialization is uniform
  
    # Proportional to poverty
      sumw_j <- v_ij[, sum(w), by = j]
      sumw <- sum(sumw_j$V1)
      sumw_j$w_j <- sumw_j$V1 / sumw
      b <- merge(b, sumw_j, by = "j")
      b$V <- exp(b$xb + theta*(b$w_j * R))
      o_pov <- Obj(b)
  
    # Proportional to population density
      # XXX Note: this is implicitly the number of youth living within 3 miles, baseed on how the data were constructed
      sumn_j <- v_ij[, length(w), by = j] 
      sumn <- sum(sumn_j$V1)
      sumn_j$n_j <- sumn_j$V1 / sumn
      b <- merge(b, sumn_j, by = "j")
      b$V <- exp(b$xb + theta*(b$n_j * R))
      o_pop   <- Obj(b)
  
    stop("Finished benchmark calcs")
  
  # Initialize checkpoints. XXX Come back to this: initialize the full size of the object
    checks <- list(Step = 0, RunTime = 0, StateHistory = r_best, Obj = o_best)
    temp <- temperature(Iter = 1, MaxIter = iterations)
    progUpdates <- data.frame(i = 1, temp = temp, obj = o_best)
    StartTime <- Sys.time()
  
  # Set iterations of the algorithm
    for (i in 1:iterations) {
      
      # Obtain a proposal, and prepare to compare it to the current state.
      r_prop    <- proposal(alloc, TransMatrix = transmat, vLower = lower, vUpper = upper)
      fromProp  <- r_prop[[1]]; toProp <- r_prop[[2]]
      v_ij_prop <- nudge(v_ij, fromProp, toProp)
      o_prop <- obj(v_ij_prop)
      
      # Determine whether to keep the proposal
      if (o_prop >= o_n) { # If the move improves our objective, save the step
        o_n <- o_prop
        v_ij <- v_ij_prop
        alloc$r[alloc$j == fromProp] <- alloc$r[alloc$j == fromProp] - 1
        alloc$r[alloc$j == toProp  ] <- alloc$r[alloc$j == toProp] + 1
      } else { # If the move increased our obj, consider acceptance
        temp <- temperature(Iter = i, MaxIter = iterations) # Note: we're only calculating the temp if we have to, i.e. if we're not improving
        p_a <- exp(-((o_n - o_prop) / temp) )
          # XXX Do we want to standardize the units of difference between o_n and o_prop?
        if (runif(1) <= p_a) { # Accept anyway
          o_n <- o_prop
          v_ij <- v_ij_prop
          alloc$r[alloc$j == fromProp] <- alloc$r[alloc$j == fromProp] - 1
          alloc$r[alloc$j == toProp  ] <- alloc$r[alloc$j == toProp] + 1
#           if (TRUE == verbose) print(paste("  Lower Obj Accepted"))
        } # If we don't accept, then we retain r and o_n from the current iteration and wait for another proposal
#         else if (TRUE == verbose) print(paste("  Lower Obj Rejected"))
      } # End of conditional examining whether to accept the proposal
      
    # If the current step is the best we've seen, save the results
      if (o_n > o_best) {
        r_best <- alloc$r
        o_best <- o_n
      }

    # Save our current results if we're at a checkpoint
      if (i %% checkpoint == 0 ) {
        Time <- difftime(Sys.time(), StartTime, units = "secs")
        print(paste0("Step ", prettyNum(i, big.mark=","), ": Current vs. Best Obj: (", round(o_n,1), ", ", round(o_best,1), 
                "), Temp (as of last rejection):",  round(temp, 3), ", Time taken: ", round(Time), ", Best State:"))
        print(r_best); cat("\n")
        checks$Step         <- cbind(checks$Step, i)
        checks$StateHistory <- cbind(checks$StateHistory, r_best)
        checks$RunTime      <- cbind(checks$RunTime, t)
        checks$Obj          <- cbind(checks$obj, o_best)
      }
      if (i*10 %% checkpoint == 0){ # Generates progress reports at 10x frequency of checkpoint
        progUpdates <- rbind(progUpdates, c(1, temp, o_n))
        tempProg <- ggplot(progUpdates, aes(x = iter, y = temp)) + geom_line(colour = black) +
          ggtitle("Degree of Experiementation(i.e. Temperature)\nby Step ofthe Optimizer") + xlab("Step of Optimizer")
        objProg  <- ggplot(progUpdates, aes(x = iter, y = obj)) + geom_line(colour = black) +
          ggtitle("Degree of Experiementation(i.e. Temperature)\nby Step ofthe Optimizer") + xlab("Step of Optimizer") +
          abline(intercept = o_unif, slope = 0, colour = blue, linetype = dashed) +
          abline(intercept = o_pov, slope = 0, colour = blue, linetype = dashed) +
          abline(intercept = o_pop, slope = 0, colour = blue, linetype = dashed)
      }

  } # End of loop across iterations
  
  # Output Results
  canopyResults <- list(BestState = r_best, CheckPoints = checks, progUpdates = progUpdates)
  return(canopyResults)  
  
}
