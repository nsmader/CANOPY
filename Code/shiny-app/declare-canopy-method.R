#------------------------------------------------------------------------------#
#
# SIMPLE SIMULATED ANNEALING METHOD
#
#------------------------------------------------------------------------------#

# Originally adapted from http://www.r-bloggers.com/simulated-annealing-in-julia/

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
                   checkpoint) {

  set.seed(60637) # Setting this so that each run with equivalent settings will be the same
  
  ### Declare the initial state to be the best state so far
    rStar <- alloc$r
    o_n   <- obj(v_ij) 
    oStar <- o_n
  
  ### Initialize checkpoints. /!\ Come back to this: initialize the full size of the object
    checks <- list(StepInfo = data.frame(Step = 0, RunTime = 0, Obj = oStar),
                   StateHistory = data.frame(j = alloc$j, State0 = alloc$r))
    temp <- temperature(Iter = 1, MaxIter = iterations)
    progUpdates <- data.frame(i = 1, temp = temp, obj = o_best)
    StartTime <- Sys.time()

  ### Set iterations of the algorithm
    for (i in 1:iterations) {
      
      # Obtain a proposal, and prepare to compare it to the current state.
      r_prop    <- proposal(alloc, TransMatrix = transmat, lb = lower, ub = upper)
      fromProp  <- r_prop[[1]]; toProp <- r_prop[[2]]
      v_ij_prop <- nudge(v_ij, fromProp, toProp)
      o_prop    <- obj(v_ij_prop)

      ### Determine whether to keep the proposal
      if (o_prop >= o_n) { # If the move improves our objective, save the step
        o_n <- o_prop
        v_ij <- v_ij_prop
        alloc$r[alloc$j == fromProp] <- alloc$r[alloc$j == fromProp] - 1
        alloc$r[alloc$j == toProp  ] <- alloc$r[alloc$j == toProp] + 1
      } else { # If the move increased our obj, consider acceptance
        temp <- temperature(Iter = i, MaxIter = iterations) # Note: we're only calculating the temp if we have to, i.e. if we're not improving
        p_a <- exp(-((o_n - o_prop) / temp) )
          # The argument to exp() will always be negative, ensuring probability of acceptance p_a < 1.
          # That argument is more negative as temperature decreases, corresponding with lower values of o_prop
        if (runif(1) <= p_a) { # Accept anyway
          o_n <- o_prop
          v_ij <- v_ij_prop
          alloc$r[alloc$j == fromProp] <- alloc$r[alloc$j == fromProp] - 1
          alloc$r[alloc$j == toProp  ] <- alloc$r[alloc$j == toProp] + 1
        } # If we don't accept, then we retain r and o_n from the current iteration and wait for another proposal
      } # End of conditional examining whether to accept the proposal
      
    # If the current step is the best we've seen, save the results
      if (o_n > oStar) {
        rStar <- alloc$r
        oStar <- o_n
      }

    # Save our current results if we're at a checkpoint
      if ((i %% checkpoint) == 0 ) {
        Time <- difftime(Sys.time(), StartTime, units = "secs")
        print(paste0("Step ", prettyNum(i, big.mark=","), ": Current vs. Best Obj: (",
                     round(o_n, 1), ", ", round(oStar, 1), "), Temp (as of last rejection): ",
                     round(temp, 3), ", Time taken: ", round(Time))) # , ", Best State:")); print(rStar)
        cat("\n")
    
        StepInfo_i     <- data.frame(Step = i, RunTime = Time, Obj = oStar)
        StateHistory_i <- data.frame(rStar); colnames(StateHistory_i) <- paste0("State", i)
        
        checks$StepInfo     <- rbind(checks$StepInfo, StepInfo_i)
        checks$StateHistory <- cbind(checks$StateHistory, StateHistory_i)
      }
      if (i*10 %% checkpoint == 0){ # Generates progress reports at 10x frequency of checkpoint
        progUpdates <- rbind(progUpdates, c(1, temp, o_n))
        tempProg <-
          ggplot(progUpdates, aes(x = iter, y = temp)) +
            geom_line(colour = black) +
            ggtitle("Degree of Experiementation(i.e. Temperature)\nby Step ofthe Optimizer") +
            xlab("Step of Optimizer")
          objProg <- 
            ggplot(progUpdates, aes(x = iter, y = obj)) + geom_line(colour = black) +
            ggtitle("Degree of Experiementation(i.e. Temperature)\nby Step ofthe Optimizer") +
            xlab("Step of Optimizer") +
            abline(intercept = o_unif, slope = 0, colour = blue, linetype = dashed) +
            abline(intercept = o_pov,  slope = 0, colour = blue, linetype = dashed) +
            abline(intercept = o_pop,  slope = 0, colour = blue, linetype = dashed)
      }
      
  } # End of loop across iterations
  
  # Output Results
  canopyResults <- list(BestState = rStar, CheckPoints = checks)
  return(canopyResults)  
  
}
