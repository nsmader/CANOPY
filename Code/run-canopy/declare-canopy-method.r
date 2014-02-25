########################################################################
#
# SIMPLE SIMULATED ANNEALING METHOD
#   Adapted from http://www.r-bloggers.com/simulated-annealing-in-julia/
#
########################################################################

# canopy()
# Arguments:
# * obj: Function from states to the real numbers. Often called an energy function, but this algorithm works for both positive and negative costs. We're economists. Unlike engineers, I'm maximizing rather than minimizing.
# * alloc: a mapping between facility ID and initial allocation (which is pulled out as s_0).
# * lower, upper: Lower and upper bounds of what can feasibly allocated to each facility.
# * neighbor: Function from states to states. Produces what the Metropolis algorithm would call a proposal.
# * temperature: Function specifying the temperature at time i.
# * iterations: Number of iterations of the algorithm that will be run. This is the only termination condition.
# * checkpoint: Frequency, in number of interations, that the procedure will save step, run time, best state visited, best obj value obtained.
# * keep_best: Do we return the best state visited or the last state visited? This is defaulted to true and, for now, is not offered as an option to turn off.

# Additional notation:
# * s_n, o_n: The current state of the system, and the current value of the objective.
# * s_prop, o_prop: The next proposed state, and the corresponding objective

canopy <- function(obj,
                   alloc,
                   lower,
                   upper,
                   neighbor,
                   transmat,
                   temperature,
                   iterations,
                   checkpoint,
                   verbose = FALSE) {

  # Set our current state to the specified intial state.
  print(class(alloc))
  alloc$s_n <- alloc$s_0
  print(colnames(alloc))
  
  # Declare the initial state to be the best state so far.
  
  best_s <- alloc$s_n
  print(colnames(alloc))
  best_o <- obj(alloc[, c("c.Id", "s_n")])
  
  # Initialize checkpoints.
  checks <- list(0, 0, best_s, best_o)
  names(checks) <- c("Step", "RunTime", "StateHistory", "Cost")
  StartTime <- Sys.time()
  
  # Set iterations of the procedure.
  for (i in 1:iterations) {
    
    # Obtain a proposal, and prepare to compare it to the current state.
    o_n    <- obj(alloc[, c("c.Id", "s_n")])
    s_prop <- neighbor(alloc, TransMatrix = transmat, vLower = lower, vUpper = upper)
    o_prop <- obj(alloc[, c("c.Id", "s_prop")])
    if (TRUE == verbose) {
      print("Current state:");  print(alloc$s_n)
      print("Proposed state:"); print(alloc$s_prop)
      print(paste0("Current Cost: ", o_n, " --- Proposed Step Cost: ", o_prop))
    }
    
    # Determine whether to keep the proposal.
    if (o_prop >= o_n) { # If the move improves our objective, save the step
            o_n <-       o_prop
      alloc$s_n <- alloc$s_prop
      if (TRUE == verbose) print(paste("Step ", i, "Accepted"))
    } else { # If the move increased our cost, consider acceptance
      t <- temperature(Iter = i, MaxIter = iterations)
      p_a <- exp(-((o_n - o_prop) / t) )
        # The argument to exp() will always be negative, ensuring probability of acceptance p_a < 1.
        # That argument is more negative as temperature decreases, as the relatively lower that o_prop is.
        # XXX Do we want to standardize the units of difference between o_n and o_prop?
      if (TRUE == verbose) paste0("Step ", i, ": pr = ", p_a)
      if (runif(1) <= p_a) { # Accept anyway
              o_n <-       o_prop
        alloc$s_n <- alloc$s_prop
        if (TRUE == verbose) print(paste("  Higher Cost Accepted"))
      } # If we don't accept, then we retain s_n and o_n from the current iteration and wait for another proposal
      else if (TRUE == verbose) print(paste("  Higher Cost Rejected"))
    }
    # If the current step is the best we've seen, save the results
    if (o_n > best_o) {
      best_s <- alloc$s_n
      best_o <- o_n
    }

    # Save our current results if we're at a checkpoint
    if (i %% checkpoint == 0 ) {
      Time <- difftime(Sys.time(), StartTime, units = "secs")
      print(paste0("Step ", prettyNum(i, big.mark=","), ": Current vs. Best Cost: (", round(o_n,1), ", ", round(best_o,1), 
              "), Temp:",  round(t,3), ", Time taken: ", round(Time), ", Best State:"))
      print(best_s); cat("\n")
      checks$Step         <- cbind(checks$Step, i)
      checks$StateHistory <- cbind(checks$StateHistory, best_s)
      checks$RunTime      <- cbind(checks$RunTime, t)
      checks$Cost         <- cbind(checks$cost, best_o)
    }

  }
  
  # Output Results
  canopyResults <- list(best_s, checks)
  names(canopyResults) <- c("Best State", "CheckPoints")
  return(canopyResults)  
  
}
